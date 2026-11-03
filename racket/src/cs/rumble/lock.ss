;; locking code for hash.ss

(define make-scheduler-lock (lambda () #f))
(define scheduler-lock-acquire (lambda (l) (#%void)))
(define scheduler-lock-release (lambda (l) (#%void)))

(define (set-scheduler-lock-callbacks! make acquire release)
  (set! make-scheduler-lock make)
  (set! scheduler-lock-acquire acquire)
  (set! scheduler-lock-release release))

;; Use `with-global-lock` to guard against both engine-based
;; concurrency and Scheme thread concurrency.

;; Use `with-global-lock*` when no guard against engine-based
;; concurrency is needed (because some operation is already atomic at
;; that level).

;; Taking the global lock disables GC, as does even waiting on the
;; lock. So, of course, make sure all globally-locked actions are
;; short.

(meta-cond
 [(not (threaded?))
  ;; Using a Chez Scheme build without thread support,
  ;; but we need to cooperate with engine-based threads.
  (define-syntax-rule (with-global-lock* e ...)
    (begin e ...))
  (define-syntax-rule (with-global-lock e ...)
    (with-interrupts-disabled
     e ...))]
 [else
  ;; Using a Chez Scheme build with thread support, so a stronger
  ;; global lock is needed.

  (define global-lock (make-mutex))
  (define-syntax-rule (with-global-lock* e ...)
    (with-global-lock e ...))
  (define-syntax-rule (with-global-lock e ...)
    ;; Disable interrupts before taking the lock, because some threads
    ;; may have interrupts disabled when they take the lock (which
    ;; blocks GCs), so always disabling interrupts imposes an order.
    (begin
      (disable-interrupts)
      (mutex-acquire global-lock)
      (begin0
       (begin e ...)
       (mutex-release global-lock)
       (enable-interrupts))))])

;; ------------------------------------------------------------
;; Locks used for hash tables

;; `with-eq-read-lock` wraps a lookup in an `eq?`-based table, where
;; the lookup is atomic with respect to engines by itself (i.e.,
;; `eq-hashtable-ref` has no interrupt trap), as long as `e` neither
;; mutates the table nor raises an exception.

(meta-cond
 [(not (threaded?))
  (define (make-nonscheduler-lock) #f)
  (define (lock-acquire lock)
    (cond
      [(not lock) (disable-interrupts)]
      [else
       (scheduler-lock-acquire lock)]))
  (define (lock-release lock)
    (cond
      [(not lock) (enable-interrupts) (#%void)]
      [else
       (scheduler-lock-release lock)]))
  (define-syntax-rule (with-eq-read-lock lock e) e)]
 [else
  ;; A nonscheduler lock is for `eq?`- and `eqv?`-based hash tables,
  ;; where the operation is atomic from the Racket perspective. It
  ;; combines a writer lock taken by CAS with a sequence lock, so that a
  ;; lookup in an `eq?`-based table takes no lock and writes nothing
  ;; shared; a lookup that overlaps a change to the table tries again.
  ;;
  ;; A nonscheduler lock is a vector of three slots:
  ;;
  ;;  * Slot 0 is the writer lock: 0 when unlocked, 1 when locked, and
  ;;    3 when locked with some thread possibly parked.
  ;;
  ;;  * Slot 1 is a counter that a writer makes odd before it changes
  ;;    the table and even again before it releases the lock.
  ;;
  ;;  * Slot 2 is #f or the lock's `parking` record, created the first
  ;;    time that a thread parks.
  ;;
  ;; A writer or reader spins briefly before parking. A thread that
  ;; parks first changes slot 0 from 1 to 3, so that the holder's
  ;; release CAS from 1 to 0 fails and the holder wakes parked threads.
  ;;
  ;; A lookup without a lock depends on Chez Scheme's `eq-hashtable-ref`
  ;; tolerating a concurrent update of the table (in the sense that it
  ;; terminates without crashing; the retry discards its result), and
  ;; on the store--store fence that Chez Scheme puts before every
  ;; pointer store on platforms with weak memory ordering, so that an
  ;; object is initialized before it is reachable from the table.
  ;;
  ;; Note that `vector-cas!` can fail spuriously on platforms (such as
  ;; AArch64) where it is implemented with load-linked/store-conditional,
  ;; so a failed CAS does not imply that a slot has changed.

  (define (make-nonscheduler-lock)
    (#%vector 0 0 #f))

  (define-record-type parking
    (fields mutex
            wcond             ; signaled to wake one parked writer
            rcond             ; broadcast to wake all parked readers
            (mutable writers) ; number of parked writers
            (mutable readers)) ; number of parked readers
    (nongenerative rumble-hash-lock-parking-1))

  (define (lock-parking lock)
    (or (#%vector-ref lock 2)
        (let ([pk (make-parking (make-mutex) (make-condition) (make-condition) 0 0)])
          (let loop ()
            (cond
              [(#%vector-cas! lock 2 #f pk) pk]
              [(#%vector-ref lock 2)]
              [else (loop)])))))

  (define-syntax-rule (bump-sequence! lock)
    (#%vector-set! lock 1 (fx+/wraparound (#%vector-ref lock 1) 1)))

  (define (lock-acquire lock)
    (cond
      [(not lock) (disable-interrupts)]
      [(#%vector? lock)
       ;; need to block engine swaps to provide atomic behavior,
       ;; and the engine-specific opeartion is cheaper than disabling all interrupts
       (start-engine-uninterrupted 'lock-acquire)
       (unless (#%vector-cas! lock 0 0 1)
         (#%$app/no-inline lock-acquire/slow lock))
       (memory-order-acquire)
       (bump-sequence! lock)
       ;; make the odd count visible before any change to the table
       (memory-order-release)]
      [else
       (scheduler-lock-acquire lock)]))

  (define (lock-acquire/slow lock)
    (let loop ([n 0])
      (cond
        [(and (eq? 0 (#%vector-ref lock 0)) (#%vector-cas! lock 0 0 1)) (void)]
        [(fx< n 100) (loop (fx+ n 1))]
        [else (park-and-acquire lock)])))

  (define (park-and-acquire lock)
    (let ([pk (lock-parking lock)])
      (let loop ()
        (let ([v (#%vector-ref lock 0)])
          (cond
            [(eq? v 0)
             ;; take the lock as 3, since other threads may still be parked
             (unless (#%vector-cas! lock 0 0 3)
               (loop))]
            [else
             (when (or (eq? v 3) (#%vector-cas! lock 0 1 3))
               (let ([m (parking-mutex pk)])
                 (mutex-acquire m)
                 (when (eq? 3 (#%vector-ref lock 0))
                   (parking-writers-set! pk (fx+ (parking-writers pk) 1))
                   (condition-wait (parking-wcond pk) m)
                   (parking-writers-set! pk (fx- (parking-writers pk) 1)))
                 (mutex-release m)))
             (loop)])))))

  (define (lock-release lock)
    (cond
      [(not lock) (enable-interrupts) (#%void)]
      [(#%vector? lock)
       ;; make changes to the table visible before the even count
       (memory-order-release)
       (bump-sequence! lock)
       ;; make the even count visible before the unlock, since the
       ;; next writer increments the count that it reads
       (memory-order-release)
       (unless (#%vector-cas! lock 0 1 0)
         (#%$app/no-inline lock-release/slow lock))
       (end-engine-uninterrupted 'lock-release)
       (void)]
      [else
       (scheduler-lock-release lock)]))

  (define (lock-release/slow lock)
    (cond
      [(eq? 1 (#%vector-ref lock 0))
       ;; spurious CAS failure
       (unless (#%vector-cas! lock 0 1 0)
         (lock-release/slow lock))]
      [else
       ;; slot 0 is 3, and only the holder changes it from there;
       ;; whoever set it to 3 installed the parking record first
       (#%vector-set! lock 0 0)
       (let ([pk (#%vector-ref lock 2)])
         (mutex-acquire (parking-mutex pk))
         (unless (eq? 0 (parking-writers pk))
           (condition-signal (parking-wcond pk)))
         (unless (eq? 0 (parking-readers pk))
           (condition-broadcast (parking-rcond pk)))
         (mutex-release (parking-mutex pk)))]))

  (define-syntax-rule (with-eq-read-lock lock e)
    (let ([l lock])
      (cond
        [l
         (let retry ()
           (let ([before (#%vector-ref l 1)])
             (cond
               [(fxodd? before)
                (#%$app/no-inline wait-for-writer l)
                (retry)]
               [else
                ;; order the count read before the table reads
                (memory-order-acquire)
                (let ([v e])
                  ;; order the table reads before the count read
                  (memory-order-acquire)
                  (if (eq? before (#%vector-ref l 1))
                      v
                      (retry)))])))]
        [else e])))

  (define (wait-for-writer l)
    (let loop ([n 0])
      (cond
        [(fxeven? (#%vector-ref l 1)) (void)]
        [(fx< n 100) (loop (fx+ n 1))]
        [else
         (park-reader l)
         (loop 0)])))

  (define (park-reader l)
    (let* ([pk (lock-parking l)]
           [m (parking-mutex pk)])
      (mutex-acquire m)
      (parking-readers-set! pk (fx+ (parking-readers pk) 1))
      (when (and (fxodd? (#%vector-ref l 1))
                 (let ([v (#%vector-ref l 0)])
                   (or (eq? v 3)
                       (and (eq? v 1) (#%vector-cas! l 0 1 3)))))
        (condition-wait (parking-rcond pk) m))
      (parking-readers-set! pk (fx- (parking-readers pk) 1))
      (mutex-release m)))])
