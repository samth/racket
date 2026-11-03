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
  ;; where the operation is atomic from the Racket perspective. It is
  ;; a writer lock that doubles as a sequence lock, so that a lookup
  ;; in an `eq?`-based table takes no lock and writes nothing shared;
  ;; a lookup that overlaps a change to the table tries again.
  ;;
  ;; A nonscheduler lock is a vector of two slots. Slot 0 is a fixnum
  ;; whose bit 0 is set while a writer holds the lock, whose bit 1 is
  ;; set when some thread may be parked waiting for the holder, and
  ;; whose remaining bits count releases. Slot 1 is #f or the lock's
  ;; `parking` record, created before any thread sets bit 1.
  ;;
  ;; A writer or reader spins briefly before parking. A thread parks
  ;; only after setting bit 1, and a release clears bit 1 in the same
  ;; CAS that unlocks, so the releasing thread knows to wake parked
  ;; threads.
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
    (#%vector 0 #f))

  (define-record-type parking
    (fields mutex
            wcond             ; signaled to wake one parked writer
            rcond             ; broadcast to wake all parked readers
            (mutable writers) ; number of parked writers
            (mutable readers)) ; number of parked readers
    (nongenerative rumble-hash-lock-parking-1))

  (define (lock-parking lock)
    (or (#%vector-ref lock 1)
        (let ([pk (make-parking (make-mutex) (make-condition) (make-condition) 0 0)])
          (let loop ()
            (cond
              [(#%vector-cas! lock 1 #f pk) pk]
              [(#%vector-ref lock 1)]
              [else (loop)])))))

  (define-syntax-rule (locked? w) (fxodd? w))
  (define-syntax-rule (locked+parked? w) (fx= 3 (fxand w 3)))
  (define-syntax-rule (parked? w) (fx= 2 (fxand w 2)))

  ;; Sets bit 1 if `w` is locked, and returns whether it is set
  (define (mark-parked! lock w)
    (and (locked? w)
         (or (parked? w)
             (#%vector-cas! lock 0 w (fxior w 2)))))

  (define (lock-acquire lock)
    (cond
      [(not lock) (disable-interrupts)]
      [(#%vector? lock)
       ;; need to block engine swaps to provide atomic behavior,
       ;; and the engine-specific opeartion is cheaper than disabling all interrupts
       (start-engine-uninterrupted 'lock-acquire)
       (let ([w (#%vector-ref lock 0)])
         (unless (and (not (locked? w))
                      (#%vector-cas! lock 0 w (fx+ w 1)))
           (#%$app/no-inline lock-acquire/slow lock)))
       (memory-order-acquire)
       ;; make the lock visible before any change to the table
       (memory-order-release)]
      [else
       (scheduler-lock-acquire lock)]))

  (define (lock-acquire/slow lock)
    (let loop ([n 0])
      (let ([w (#%vector-ref lock 0)])
        (cond
          [(and (not (locked? w)) (#%vector-cas! lock 0 w (fx+ w 1))) (void)]
          [(fx< n 100) (loop (fx+ n 1))]
          [else (park-and-acquire lock)]))))

  (define (park-and-acquire lock)
    (let ([pk (lock-parking lock)])
      (let loop ()
        (let ([w (#%vector-ref lock 0)])
          (cond
            [(not (locked? w))
             ;; take the lock with bit 1 set, since other threads may
             ;; still be parked
             (unless (#%vector-cas! lock 0 w (fxior w 3))
               (loop))]
            [else
             (when (mark-parked! lock w)
               (let ([m (parking-mutex pk)])
                 (mutex-acquire m)
                 (when (locked+parked? (#%vector-ref lock 0))
                   (parking-writers-set! pk (fx+ (parking-writers pk) 1))
                   (condition-wait (parking-wcond pk) m)
                   (parking-writers-set! pk (fx- (parking-writers pk) 1)))
                 (mutex-release m)))
             (loop)])))))

  (define (lock-release lock)
    (cond
      [(not lock) (enable-interrupts) (#%void)]
      [(#%vector? lock)
       ;; make changes to the table visible before the unlock
       (memory-order-release)
       (let loop ()
         (let ([w (#%vector-ref lock 0)])
           ;; clear bits 0 and 1, and count the release
           (cond
             [(#%vector-cas! lock 0 w (fx+/wraparound (fxand w -4) 4))
              (when (parked? w)
                (#%$app/no-inline wake-parked lock))]
             [else
              ;; another thread set bit 1, or a spurious failure
              (loop)])))
       (end-engine-uninterrupted 'lock-release)
       (void)]
      [else
       (scheduler-lock-release lock)]))

  (define (wake-parked lock)
    (let ([pk (#%vector-ref lock 1)])
      (mutex-acquire (parking-mutex pk))
      (unless (eq? 0 (parking-writers pk))
        (condition-signal (parking-wcond pk)))
      (unless (eq? 0 (parking-readers pk))
        (condition-broadcast (parking-rcond pk)))
      (mutex-release (parking-mutex pk))))

  (define-syntax-rule (with-eq-read-lock lock e)
    (let ([l lock])
      (cond
        [l
         (let retry ()
           (let ([before (#%vector-ref l 0)])
             (cond
               [(locked? before)
                (#%$app/no-inline wait-for-writer l)
                (retry)]
               [else
                ;; order the lock read before the table reads
                (memory-order-acquire)
                (let ([v e])
                  ;; order the table reads before the lock read
                  (memory-order-acquire)
                  ;; bit 1 is clear in `before`, and a change in bit 1
                  ;; alone does not mean that the table changed
                  (if (fx= before (fxand (#%vector-ref l 0) -3))
                      v
                      (retry)))])))]
        [else e])))

  (define (wait-for-writer l)
    (let loop ([n 0])
      (cond
        [(not (locked? (#%vector-ref l 0))) (void)]
        [(fx< n 100) (loop (fx+ n 1))]
        [else
         (park-reader l)
         (loop 0)])))

  (define (park-reader l)
    (let* ([pk (lock-parking l)]
           [m (parking-mutex pk)])
      (mutex-acquire m)
      (parking-readers-set! pk (fx+ (parking-readers pk) 1))
      (when (mark-parked! l (#%vector-ref l 0))
        (condition-wait (parking-rcond pk) m))
      (parking-readers-set! pk (fx- (parking-readers pk) 1))
      (mutex-release m)))])
