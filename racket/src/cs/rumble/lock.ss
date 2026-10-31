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

;; PROTOTYPE: the lock design for `eq?`- and `eqv?`-based tables is
;; chosen when Rumble is compiled, by the RACKET_HASH_LOCK environment
;; variable:
;;   master   - CAS to take and CAS to release, switching to a mutex
;;              after contention
;;   flag     - like `master`, but no CAS until the place forks a pthread
;;   spin     - CAS to take, plain store to release, waiters spin and sleep
;;   park     - CAS to take, CAS to release, waiters spin and then park
;;              on a condition variable (never switches to a mutex)
;;   seq      - seqlock: writers as in `spin` but with a counter,
;;              `eq-hashtable-ref` readers take no lock and retry if a
;;              writer interfered
;;   seqflag  - `seq` with the single-thread flag
(meta define hash-lock-variant
  (let ([v (#%getenv "RACKET_HASH_LOCK")])
    (if (and v (not (equal? v "")))
        (#%string->symbol v)
        'flag)))

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
  ;; Until a place forks its first extra pthread (for a future or a
  ;; parallel thread), only one OS thread can touch the place's tables.
  ;; The flag is set by the only OS thread of the place, which is not in
  ;; the middle of a locked operation, and it is never cleared.
  (define-syntax-rule (place-in-parallel?)
    (eq? #t (#%vector-ref (place-registers) PARALLEL-PTHREADS-INDEX)))

  (define-syntax (lock-needed? stx)
    (if (memq hash-lock-variant '(flag seqflag))
        #'(place-in-parallel?)
        #'#t))

  ;; Waiting in the spinning variants: spin, then sleep with backoff
  (define-syntax-rule (spin-until test)
    (let loop ([n 0] [sleep-ns 1000])
      (cond
        [test (void)]
        [(fx< n 100) (loop (fx+ n 1) sleep-ns)]
        [else
         (#%sleep (#%make-time 'time-duration sleep-ns 0))
         (loop 0 (fxmin (fx* sleep-ns 2) 1000000))])))

  (define (make-nonscheduler-lock)
    (box (meta-cond
          [(memq hash-lock-variant '(seq seqflag)) 0]
          [else #f])))

  (define-syntax-rule (define-box-lock-ops fast-acquire fast-release)
    (begin
      (define (lock-acquire lock)
        (cond
          [(not lock) (disable-interrupts)]
          [(box? lock)
           ;; need to block engine swaps to provide atomic behavior,
           ;; and the engine-specific opeartion is cheaper than disabling all interrupts
           (start-engine-uninterrupted 'lock-acquire)
           (when (lock-needed?)
             (fast-acquire lock))]
          [else
           (scheduler-lock-acquire lock)]))
      (define (lock-release lock)
        (cond
          [(not lock) (enable-interrupts) (#%void)]
          [(box? lock)
           (when (lock-needed?)
             (fast-release lock))
           (end-engine-uninterrupted 'lock-release)
           (void)]
          [else
           (scheduler-lock-release lock)]))))

  (meta-cond
   [(memq hash-lock-variant '(master flag))
    (define transition-lock (make-mutex))
    (define transition-cond (make-condition))

    ;; box of
    ;;  #f - unlocked, uncontended
    ;;  #t - locked, uncontended
    ;;  <mutex> - contended and managed by mutex
    ;;  <void> - locked, contended, switches to <mutex> on release

    (define-syntax-rule (fast-acquire lock)
      (cond
        [(box-cas! lock #f #t) (memory-order-acquire)]
        [else (#%$app/no-inline lock-acquire/slow lock)]))

    (define-syntax-rule (fast-release lock)
      (begin
        (memory-order-release)
        (cond
          [(box-cas! lock #t #f) (void)]
          [else
           (#%$app/no-inline lock-release-slow lock)])))

    (define-box-lock-ops fast-acquire fast-release)

    (define (lock-acquire/slow lock)
      (let loop ()
        (cond
          [(box-cas! lock #f #t)
           ;; uncontended lock
           (memory-order-acquire)]
          [else
           (let ([v (unbox lock)])
             (cond
               [(mutex? v)
                ;; contended lock that has switched to using a mutex
                (mutex-acquire v)]
               [(or (eq? v #t)
                    (void? v))
                ;; contended lock that needs to switch to a mutex, maybe in process already
                (mutex-acquire transition-lock)
                (cond
                  [(box-cas! lock v (void))
                   (condition-wait transition-cond transition-lock)
                   (mutex-release transition-lock)]
                  [else
                   (mutex-release transition-lock)])
                (loop)]
               [else
                (loop)]))])))

    (define (lock-release-slow lock)
      (let loop ()
        (cond
          [(box-cas! lock #t #f)
           ;; release uncontended lock
           (void)]
          [else
           (let ([v (unbox lock)])
             (cond
               [(mutex? v)
                ;; contended lock that has switched to using a mutex
                (mutex-release v)]
               [(void? v)
                ;; contended lock that needs to switch to a mutex
                (let ([m (make-mutex)])
                  (cond
                    [(box-cas! lock (void) m)
                     (mutex-acquire transition-lock)
                     (condition-broadcast transition-cond)
                     (mutex-release transition-lock)]
                    [else
                     (loop)]))]
               [else
                (loop)]))])))

    ;; `eq-hashtable-ref` is atomic with respect to engines by itself
    (define-syntax-rule (with-eq-read-lock lock e)
      (let ([l lock])
        (cond
          [(and (lock-needed?) l)
           (lock-acquire l)
           (let ([v e])
             (lock-release l)
             v)]
          [else e])))]

   [(eq? hash-lock-variant 'spin)
    ;; box of #f (unlocked) or #t (locked)
    (define-syntax-rule (fast-acquire lock)
      (cond
        [(box-cas! lock #f #t) (memory-order-acquire)]
        [else (#%$app/no-inline lock-acquire/slow lock)]))
    (define-syntax-rule (fast-release lock)
      (begin
        (memory-order-release)
        (set-box! lock #f)))
    (define-box-lock-ops fast-acquire fast-release)
    (define (lock-acquire/slow lock)
      (spin-until (and (not (unbox lock))
                       (box-cas! lock #f #t)))
      (memory-order-acquire))
    (define-syntax-rule (with-eq-read-lock lock e)
      (let ([l lock])
        (cond
          [l
           (lock-acquire l)
           (let ([v e])
             (lock-release l)
             v)]
          [else e])))]

   [(eq? hash-lock-variant 'park)
    ;; box of #f (unlocked), #t (locked), or 'waiters (locked, and
    ;; some thread may be parked); all parked threads share one
    ;; mutex and condition, since contention is expected to be rare
    (define park-mutex (make-mutex))
    (define park-cond (make-condition))
    (define-syntax-rule (fast-acquire lock)
      (cond
        [(box-cas! lock #f #t) (memory-order-acquire)]
        [else (#%$app/no-inline lock-acquire/slow lock)]))
    (define-syntax-rule (fast-release lock)
      (begin
        (memory-order-release)
        (unless (box-cas! lock #t #f)
          (#%$app/no-inline lock-release/slow lock))))
    (define-box-lock-ops fast-acquire fast-release)
    (define (lock-acquire/slow lock)
      (let loop ([n 0])
        (cond
          [(fx< n 100)
           (if (and (not (unbox lock))
                    (box-cas! lock #f #t))
               (memory-order-acquire)
               (loop (fx+ n 1)))]
          [else
           ;; after parking once, take the lock as 'waiters, since
           ;; other threads may still be parked
           (let park-loop ()
             (cond
               [(box-cas! lock #f 'waiters)
                (memory-order-acquire)]
               [else
                (let ([v (unbox lock)])
                  (when (or (eq? v 'waiters)
                            (and (eq? v #t) (box-cas! lock #t 'waiters)))
                    (mutex-acquire park-mutex)
                    (when (eq? (unbox lock) 'waiters)
                      (condition-wait park-cond park-mutex))
                    (mutex-release park-mutex)))
                (park-loop)]))])))
    (define (lock-release/slow lock)
      ;; state must be 'waiters, and only the holder changes it from there
      (set-box! lock #f)
      (mutex-acquire park-mutex)
      (condition-broadcast park-cond)
      (mutex-release park-mutex))
    (define-syntax-rule (with-eq-read-lock lock e)
      (let ([l lock])
        (cond
          [l
           (lock-acquire l)
           (let ([v e])
             (lock-release l)
             v)]
          [else e])))]

   [(memq hash-lock-variant '(seq seqflag))
    ;; box of a fixnum counter: even is unlocked, odd is locked; each
    ;; writer adds 2 in total
    (define-syntax-rule (fast-acquire lock)
      (let ([v (unbox lock)])
        (cond
          [(and (fxeven? v) (box-cas! lock v (fx+ v 1))) (memory-order-acquire)]
          [else (#%$app/no-inline lock-acquire/slow lock)])))
    (define-syntax-rule (fast-release lock)
      (begin
        (memory-order-release)
        (set-box! lock (fx+ (unbox lock) 1))))
    (define-box-lock-ops fast-acquire fast-release)
    (define (lock-acquire/slow lock)
      (spin-until (let ([v (unbox lock)])
                    (and (fxeven? v)
                         (box-cas! lock v (fx+ v 1)))))
      (memory-order-acquire))
    ;; A reader takes no lock. `e` must be safe to run concurrently with
    ;; a writer (it may produce a wrong answer, but must not crash or
    ;; loop forever); `eq-hashtable-ref` is assumed to be so on x86_64,
    ;; while weaker memory models would need a store fence in Chez's
    ;; table updates.
    (define-syntax-rule (with-eq-read-lock lock e)
      (let ([l lock])
        (cond
          [(and l (lock-needed?))
           (let retry ()
             (let ([before (unbox l)])
               (cond
                 [(fxodd? before)
                  (#%$app/no-inline wait-for-writer l)
                  (retry)]
                 [else
                  (memory-order-acquire)
                  (let ([v e])
                    (memory-order-acquire)
                    (if (eq? before (unbox l))
                        v
                        (retry)))])))]
          [l e]
          [else e])))
    (define (wait-for-writer l)
      (spin-until (fxeven? (unbox l))))]))
