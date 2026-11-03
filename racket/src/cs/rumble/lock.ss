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
  ;; a writer lock combined with a sequence lock, so that a lookup in
  ;; an `eq?`-based table takes no lock and writes nothing shared: it
  ;; reads a count, does the lookup, and tries again if the count
  ;; changed or is odd. A writer makes the count odd while it holds
  ;; the lock and even again when it releases the lock.
  ;;
  ;; The layout depends on the platform's memory ordering:
  ;;
  ;;  * On x86, where stores become visible in order, a lock is a
  ;;    vector of three slots. Slot 0 is the writer lock: 0 when
  ;;    unlocked, 1 when locked, and 3 when locked with some thread
  ;;    possibly parked. Slot 1 is the count. Slot 2 is #f or the
  ;;    lock's `parking` record. The CASes on slot 0 compare against
  ;;    constants, and the count updates are off their critical path.
  ;;
  ;;  * Elsewhere, a lock is a vector of two slots. Slot 0 combines
  ;;    the writer lock and the count: bit 0 is set while a writer
  ;;    holds the lock (so the count is odd), bit 1 is set when some
  ;;    thread may be parked, and the CAS that releases the lock clears
  ;;    both bits and adds to the count. Slot 1 is #f or the `parking`
  ;;    record. Keeping the count in the lock word saves the fence that
  ;;    would otherwise order a separate count update before the unlock.
  ;;
  ;; A writer or reader spins briefly before parking. A thread parks
  ;; only after marking the lock word as possibly having parked
  ;; threads, so that the holder's release notices and wakes them.
  ;; The `parking` record is created before any thread sets that mark.
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

  ;; x86 targets have the form `[t]a6...` or `[t]i3...`
  (meta define (strong-store-order?)
    (let* ([s (#%symbol->string (#%$target-machine))]
           [s (if (#%char=? #\t (#%string-ref s 0))
                  (#%substring s 1 (#%string-length s))
                  s)])
      (and (#%member (#%substring s 0 2) '("a6" "i3"))
           #t)))

  (define-record-type parking
    (fields mutex
            wcond             ; signaled to wake one parked writer
            rcond             ; broadcast to wake all parked readers
            (mutable writers) ; number of parked writers
            (mutable readers)) ; number of parked readers
    (nongenerative rumble-hash-lock-parking-1))

  (meta-cond
   [(strong-store-order?)
    (define-syntax-rule (make-lock-vector) (#%vector 0 0 #f))
    (define PARKING-SLOT 2)
    (define-syntax-rule (lock-count l) (#%vector-ref l 1))
    (define-syntax-rule (same-count? l c) (eq? c (#%vector-ref l 1)))
    (define-syntax-rule (bump-count! l)
      (#%vector-set! l 1 (fx+/wraparound (#%vector-ref l 1) 1)))
    (define-syntax-rule (fast-acquire! l)
      (begin
        (unless (#%vector-cas! l 0 0 1)
          (#%$app/no-inline lock-acquire/slow l))
        (bump-count! l)))
    (define-syntax-rule (fast-release! l)
      (begin
        (bump-count! l)
        (unless (#%vector-cas! l 0 1 0)
          (#%$app/no-inline lock-release/slow l))))
    (define (lock-release/slow l)
      (cond
        [(eq? 1 (#%vector-ref l 0))
         ;; spurious CAS failure
         (unless (#%vector-cas! l 0 1 0)
           (lock-release/slow l))]
        [else
         ;; slot 0 is 3, and only the holder changes it from there
         (#%vector-set! l 0 0)
         (wake-parked l)]))
    (define-syntax-rule (unlocked? v) (eq? v 0))
    (define-syntax-rule (try-acquire! l v) (#%vector-cas! l 0 0 1))
    ;; take the lock as 3, since other threads may still be parked
    (define-syntax-rule (try-acquire/parked! l v) (#%vector-cas! l 0 0 3))
    (define-syntax-rule (locked+parked? v) (eq? v 3))
    (define-syntax-rule (mark-parked! l v)
      (or (eq? v 3)
          (and (eq? v 1) (#%vector-cas! l 0 1 3))))]
   [else
    (define-syntax-rule (make-lock-vector) (#%vector 0 #f))
    (define PARKING-SLOT 1)
    (define-syntax-rule (lock-count l) (#%vector-ref l 0))
    ;; bit 1 is clear in an even count, and a change in bit 1 alone
    ;; does not mean that the table changed
    (define-syntax-rule (same-count? l c) (fx= c (fxand (#%vector-ref l 0) -3)))
    (define-syntax-rule (fast-acquire! l)
      (let ([w (#%vector-ref l 0)])
        (unless (and (fxeven? w)
                     (#%vector-cas! l 0 w (fx+ w 1)))
          (#%$app/no-inline lock-acquire/slow l))))
    (define-syntax-rule (fast-release! l)
      (let loop ()
        (let ([w (#%vector-ref l 0)])
          ;; clear bits 0 and 1, and count the release
          (cond
            [(#%vector-cas! l 0 w (fx+/wraparound (fxand w -4) 4))
             (when (fx= 2 (fxand w 2))
               (#%$app/no-inline wake-parked l))]
            [else
             ;; another thread set bit 1, or a spurious failure
             (loop)]))))
    (define-syntax-rule (unlocked? v) (fxeven? v))
    (define-syntax-rule (try-acquire! l v) (#%vector-cas! l 0 v (fx+ v 1)))
    ;; take the lock with bit 1 set, since other threads may still be parked
    (define-syntax-rule (try-acquire/parked! l v) (#%vector-cas! l 0 v (fxior v 3)))
    (define-syntax-rule (locked+parked? v) (fx= 3 (fxand v 3)))
    (define-syntax-rule (mark-parked! l v)
      (and (fxodd? v)
           (or (fx= 2 (fxand v 2))
               (#%vector-cas! l 0 v (fxior v 2)))))])

  (define (make-nonscheduler-lock)
    (make-lock-vector))

  ;; Called before a thread sets the lock word's parked mark, which
  ;; tells a releasing thread to use the parking record
  (define (lock-parking l)
    (let ([pk (or (#%vector-ref l PARKING-SLOT)
                  (let ([pk (make-parking (make-mutex) (make-condition) (make-condition) 0 0)])
                    (let loop ()
                      (cond
                        [(#%vector-cas! l PARKING-SLOT #f pk) pk]
                        [(#%vector-ref l PARKING-SLOT)]
                        [else (loop)]))))])
      ;; make the record visible before the parked mark
      (memory-order-release)
      pk))

  ;; Called after a release CAS sees the parked mark
  (define (wake-parked l)
    ;; pairs with the fence in `lock-parking`
    (memory-order-acquire)
    (let* ([pk (#%vector-ref l PARKING-SLOT)]
           [m (parking-mutex pk)])
      (mutex-acquire m)
      (unless (eq? 0 (parking-writers pk))
        (condition-signal (parking-wcond pk)))
      (unless (eq? 0 (parking-readers pk))
        (condition-broadcast (parking-rcond pk)))
      (mutex-release m)))

  (define (lock-acquire lock)
    (cond
      [(not lock) (disable-interrupts)]
      [(#%vector? lock)
       ;; need to block engine swaps to provide atomic behavior,
       ;; and the engine-specific opeartion is cheaper than disabling all interrupts
       (start-engine-uninterrupted 'lock-acquire)
       (fast-acquire! lock)
       (memory-order-acquire)
       ;; make the odd count visible before any change to the table
       (memory-order-release)]
      [else
       (scheduler-lock-acquire lock)]))

  (define (lock-acquire/slow l)
    (let loop ([n 0])
      (let ([v (#%vector-ref l 0)])
        (cond
          [(and (unlocked? v) (try-acquire! l v)) (void)]
          [(fx< n 100) (loop (fx+ n 1))]
          [else (park-and-acquire l)]))))

  (define (park-and-acquire l)
    (let ([pk (lock-parking l)])
      (let loop ()
        (let ([v (#%vector-ref l 0)])
          (cond
            [(unlocked? v)
             (unless (try-acquire/parked! l v)
               (loop))]
            [else
             (when (mark-parked! l v)
               (let ([m (parking-mutex pk)])
                 (mutex-acquire m)
                 (when (locked+parked? (#%vector-ref l 0))
                   (parking-writers-set! pk (fx+ (parking-writers pk) 1))
                   (condition-wait (parking-wcond pk) m)
                   (parking-writers-set! pk (fx- (parking-writers pk) 1)))
                 (mutex-release m)))
             (loop)])))))

  (define (lock-release lock)
    (cond
      [(not lock) (enable-interrupts) (#%void)]
      [(#%vector? lock)
       ;; make changes to the table visible before the even count and
       ;; the unlock (on x86, the count's update and the unlock are
       ;; ordered without a fence)
       (memory-order-release)
       (fast-release! lock)
       (end-engine-uninterrupted 'lock-release)
       (void)]
      [else
       (scheduler-lock-release lock)]))

  (define-syntax-rule (with-eq-read-lock lock e)
    (let ([l lock])
      (cond
        [l
         (let retry ()
           (let ([before (lock-count l)])
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
                  (if (same-count? l before)
                      v
                      (retry)))])))]
        [else e])))

  (define (wait-for-writer l)
    (let loop ([n 0])
      (cond
        [(fxeven? (lock-count l)) (void)]
        [(fx< n 100) (loop (fx+ n 1))]
        [else
         (park-reader l)
         (loop 0)])))

  (define (park-reader l)
    (let* ([pk (lock-parking l)]
           [m (parking-mutex pk)])
      (mutex-acquire m)
      (parking-readers-set! pk (fx+ (parking-readers pk) 1))
      (when (and (fxodd? (lock-count l))
                 (let ([v (#%vector-ref l 0)])
                   (mark-parked! l v)))
        (condition-wait (parking-rcond pk) m))
      (parking-readers-set! pk (fx- (parking-readers pk) 1))
      (mutex-release m)))])
