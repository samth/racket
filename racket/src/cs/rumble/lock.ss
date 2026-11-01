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
;;   seqflag  - same as `seq`
;;   yield    - like `spin`, but waiters spin and then `sched_yield`
;;   bias     - each table is biased to the OS thread that creates it;
;;              the owner uses plain stores, and another thread revokes
;;              the bias once (with membarrier), after which the table
;;              uses the `yield` lock
;;   seqpark  - `seq` readers, with writers that spin and then park, and
;;              no single-thread flag
;;   seqpark2 - `seqpark` with the lock and the counter in separate words,
;;              so a writer takes the lock with a blind CAS, and readers
;;              that wait sleep with backoff instead of locking
;;   seqpark2b - `seqpark2`, but a writer releases with a plain store and
;;              waiters park with a timed wait, since the releaser can
;;              miss a waiter that arrives at the same time
;; Every variant except `master` and the `seqpark` variants skips locking until the place forks a
;; pthread; setting PLT_HASH_ALWAYS_LOCK at run time disables that.
(meta define hash-lock-variant
  (let ([v (#%getenv "RACKET_HASH_LOCK")])
    (if (and v (not (#%equal? v "")))
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
    (if (memq hash-lock-variant '(master seqpark seqpark2 seqpark2b))
        #'#t
        #'(place-in-parallel?)))

  (define sched-yield
    (let ([proc #f])
      (lambda ()
        (unless proc
          (#%load-shared-object "libc.so.6")
          (set! proc (foreign-procedure "sched_yield" () int)))
        (proc))))

  ;; Waiting in the spinning variants: spin, then sleep with backoff
  ;; (`spin`) or yield the CPU (the others)
  (define-syntax spin-until
    (lambda (stx)
      (syntax-case stx ()
        [(_ test)
         (if (eq? hash-lock-variant 'spin)
             #'(let loop ([n 0] [sleep-ns 1000])
                 (cond
                   [test (void)]
                   [(fx< n 100) (loop (fx+ n 1) sleep-ns)]
                   [else
                    (#%sleep (#%make-time 'time-duration sleep-ns 0))
                    (loop 0 (fxmin (fx* sleep-ns 2) 1000000))]))
             #'(let loop ([n 0])
                 (cond
                   [test (void)]
                   [(fx< n 100) (loop (fx+ n 1))]
                   [else
                    (sched-yield)
                    (loop 0)])))])))

  (meta-cond
   [(eq? hash-lock-variant 'bias)
    (define-record-type blk
      (fields state                ; box of #f or #t, for the revoked mode
              (mutable owner)      ; thread id, or #f after revocation
              (mutable inop)       ; #t while the owner is in an operation
              (mutable revoked))
      (nongenerative rumble-biased-lock-1))
    (define-syntax-rule (nonscheduler-lock? l) (blk? l))]
   [(memq hash-lock-variant '(seqpark2 seqpark2b))
    (define-syntax-rule (nonscheduler-lock? l) (#%vector? l))]
   [else
    (define-syntax-rule (nonscheduler-lock? l) (box? l))])

  (define (make-nonscheduler-lock)
    (meta-cond
     [(eq? hash-lock-variant 'bias) (make-blk (box #f) (get-thread-id) #f #f)]
     [(memq hash-lock-variant '(seq seqflag seqpark)) (box 0)]
     [(memq hash-lock-variant '(seqpark2 seqpark2b)) (vector 0 0)]
     [else (box #f)]))

  (define-syntax-rule (define-box-lock-ops lock-acquire lock-release fast-acquire fast-release)
    (begin
      (define (lock-acquire lock)
        (cond
          [(not lock) (disable-interrupts)]
          [(nonscheduler-lock? lock)
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
          [(nonscheduler-lock? lock)
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

    (define-box-lock-ops lock-acquire lock-release fast-acquire fast-release)

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

   [(memq hash-lock-variant '(spin yield))
    ;; box of #f (unlocked) or #t (locked)
    (define-syntax-rule (fast-acquire lock)
      (cond
        [(box-cas! lock #f #t) (memory-order-acquire)]
        [else (#%$app/no-inline lock-acquire/slow lock)]))
    (define-syntax-rule (fast-release lock)
      (begin
        (memory-order-release)
        (set-box! lock #f)))
    (define-box-lock-ops lock-acquire lock-release fast-acquire fast-release)
    (define (lock-acquire/slow lock)
      (spin-until (and (not (unbox lock))
                       (box-cas! lock #f #t)))
      (memory-order-acquire))
    (define-syntax-rule (with-eq-read-lock lock e)
      (let ([l lock])
        (cond
          [(and l (lock-needed?))
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
    (define-box-lock-ops lock-acquire lock-release fast-acquire fast-release)
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
          [(and l (lock-needed?))
           (lock-acquire l)
           (let ([v e])
             (lock-release l)
             v)]
          [else e])))]

   [(eq? hash-lock-variant 'bias)
    ;; The owner sets `inop`, then checks `revoked`; a revoker sets
    ;; `revoked`, runs membarrier() so that every running thread of the
    ;; process executes a full fence, then waits for `inop` to clear.
    ;; So either the revoker sees the owner's `inop`, or the owner sees
    ;; `revoked`, without a fence on the owner's side. Only the owner
    ;; sets `inop`, so a release is in owner mode exactly when `inop` is
    ;; set and the caller is the owner.
    (define-syntax-rule (fast-acquire lock)
      (cond
        [(eqv? (blk-owner lock) (get-thread-id))
         (blk-inop-set! lock #t)
         (when (blk-revoked lock)
           (blk-inop-set! lock #f)
           (#%$app/no-inline acquire/shared lock))]
        [else (#%$app/no-inline acquire/nonowner lock)]))
    (define-syntax-rule (fast-release lock)
      (cond
        [(and (blk-inop lock) (eqv? (blk-owner lock) (get-thread-id)))
         (blk-inop-set! lock #f)]
        [else
         (memory-order-release)
         (set-box! (blk-state lock) #f)]))
    (define-box-lock-ops lock-acquire lock-release fast-acquire fast-release)
    (define (acquire/shared lock)
      (let ([b (blk-state lock)])
        (spin-until (and (not (unbox b))
                         (box-cas! b #f #t)))
        (memory-order-acquire)))
    (define (acquire/nonowner lock)
      (when (blk-owner lock)
        (blk-revoked-set! lock #t)
        (membarrier)
        (spin-until (not (blk-inop lock)))
        (blk-owner-set! lock #f))
      (acquire/shared lock))
    (define membarrier
      (let ([proc #f])
        (lambda ()
          (unless proc
            (#%load-shared-object "libc.so.6")
            (let ([sys (foreign-procedure (__varargs_after 1) "syscall" (long int unsigned-int int) long)])
              ;; 324 = SYS_membarrier on x86_64; 16 = MEMBARRIER_CMD_REGISTER_PRIVATE_EXPEDITED
              (unless (eqv? 0 (sys 324 16 0 0))
                (#%error 'membarrier "registration failed"))
              ;; 8 = MEMBARRIER_CMD_PRIVATE_EXPEDITED
              (set! proc (lambda ()
                           (unless (eqv? 0 (sys 324 8 0 0))
                             (#%error 'membarrier "failed"))))))
          (proc))))
    (define-syntax-rule (with-eq-read-lock lock e)
      (let ([l lock])
        (cond
          [(and l (lock-needed?))
           (lock-acquire l)
           (let ([v e])
             (lock-release l)
             v)]
          [else e])))]

   [(memq hash-lock-variant '(seqpark2 seqpark2b))
    ;; A vector of two fixnums. Slot 0 is the writer lock: 0 when
    ;; unlocked, 1 when locked, and (for `seqpark2`) 3 when locked with
    ;; some writer possibly parked. Slot 1 is a seqlock counter that the
    ;; holder makes odd before changing the table and even again after.
    (define park-mutex (make-mutex))
    (define park-cond (make-condition))
    (define-syntax-rule (begin-write lock)
      (#%vector-set! lock 1 (fx+ (#%vector-ref lock 1) 1)))
    (define-syntax-rule (fast-acquire lock)
      (begin
        (unless (#%vector-cas! lock 0 0 1)
          (#%$app/no-inline lock-acquire/slow lock))
        (memory-order-acquire)
        (begin-write lock)))
    (define (lock-acquire/slow lock)
      (let loop ([n 0])
        (cond
          [(and (eq? 0 (#%vector-ref lock 0)) (#%vector-cas! lock 0 0 1)) (void)]
          [(fx< n 100) (loop (fx+ n 1))]
          [else (park-and-acquire lock)])))
    (meta-cond
     [(eq? hash-lock-variant 'seqpark2)
      ;; Release with a CAS that notices a parked writer
      (define-syntax-rule (fast-release lock)
        (begin
          (begin-write lock)
          (memory-order-release)
          (unless (#%vector-cas! lock 0 1 0)
            (#%$app/no-inline lock-release/slow lock))))
      (define (park-and-acquire lock)
        (let loop ()
          (let ([v (#%vector-ref lock 0)])
            (cond
              [(eq? v 0)
               ;; after parking, take the lock as 3, since other writers
               ;; may still be parked
               (unless (#%vector-cas! lock 0 0 3)
                 (loop))]
              [else
               (when (or (eq? v 3) (#%vector-cas! lock 0 1 3))
                 (mutex-acquire park-mutex)
                 (when (eq? 3 (#%vector-ref lock 0))
                   (condition-wait park-cond park-mutex))
                 (mutex-release park-mutex))
               (loop)]))))
      (define (lock-release/slow lock)
        ;; slot 0 must be 3, and only the holder changes it from there
        (#%vector-set! lock 0 0)
        (mutex-acquire park-mutex)
        (condition-broadcast park-cond)
        (mutex-release park-mutex))]
     [else
      ;; Release with a plain store, then check a count of parked writers.
      ;; Without a fence between the store and the load, the releaser can
      ;; miss a writer that parks at the same moment, so parking is timed.
      (define parked-count 0)
      (define timeout-count 0)
      (define park-count 0)
      (define park-time (#%make-time 'time-duration 1000000 0))
      (define-syntax-rule (fast-release lock)
        (begin
          (begin-write lock)
          (memory-order-release)
          (#%vector-set! lock 0 0)
          (unless (eq? 0 parked-count)
            (#%$app/no-inline wake-parked))))
      (define (wake-parked)
        (mutex-acquire park-mutex)
        (condition-broadcast park-cond)
        (mutex-release park-mutex))
      (define (park-and-acquire lock)
        (let loop ()
          (cond
            [(and (eq? 0 (#%vector-ref lock 0)) (#%vector-cas! lock 0 0 1)) (void)]
            [else
             (mutex-acquire park-mutex)
             (set! parked-count (fx+ parked-count 1))
             (when (eq? 1 (#%vector-ref lock 0))
               (set! park-count (fx+ park-count 1))
               (unless (condition-wait park-cond park-mutex park-time)
                 (set! timeout-count (fx+ timeout-count 1))
                 ;; visible from `vm-eval` for reporting
                 (#%set-top-level-value! 'hash-lock-park-timeouts timeout-count))
               (#%set-top-level-value! 'hash-lock-parks park-count))
             (set! parked-count (fx- parked-count 1))
             (mutex-release park-mutex)
             (loop)])))])
    (define-box-lock-ops lock-acquire lock-release fast-acquire fast-release)
    ;; A reader takes no lock; see `seq` about safety, which holds on
    ;; x86_64 only
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
                  (memory-order-acquire)
                  (let ([v e])
                    (memory-order-acquire)
                    (if (eq? before (#%vector-ref l 1))
                        v
                        (retry)))])))]
          [else e])))
    (define (wait-for-writer l)
      (let loop ([n 0] [sleep-ns 1000])
        (cond
          [(fxeven? (#%vector-ref l 1)) (void)]
          [(fx< n 100) (loop (fx+ n 1) sleep-ns)]
          [else
           (#%sleep (#%make-time 'time-duration sleep-ns 0))
           (loop 0 (fxmin (fx* sleep-ns 2) 1000000))])))]

   [(eq? hash-lock-variant 'seqpark)
    ;; box of a fixnum 4*gen + 2*waiters + locked: bit 0 is set while a
    ;; writer holds the lock, bit 1 when some writer may be parked, and
    ;; each release bumps `gen` and clears both bits. Only the holder
    ;; changes the value from a locked one, except that a waiter may set
    ;; bit 1. All parked writers share one mutex and condition.
    (define park-mutex (make-mutex))
    (define park-cond (make-condition))
    (define-syntax-rule (fast-acquire lock)
      (let ([v (unbox lock)])
        (cond
          [(and (fxeven? v) (box-cas! lock v (fx+ v 1))) (memory-order-acquire)]
          [else (#%$app/no-inline lock-acquire/slow lock)])))
    (define-syntax-rule (fast-release lock)
      (let* ([v (unbox lock)]
             [next (fx+ (fxand v -4) 4)])
        (memory-order-release)
        (unless (and (fx= 0 (fxand v 2))
                     (box-cas! lock v next))
          (#%$app/no-inline lock-release/slow lock next))))
    (define-box-lock-ops lock-acquire lock-release fast-acquire fast-release)
    (define (lock-acquire/slow lock)
      (let loop ([n 0])
        (let ([v (unbox lock)])
          (cond
            [(and (fxeven? v) (box-cas! lock v (fx+ v 1)))
             (memory-order-acquire)]
            [(fx< n 100) (loop (fx+ n 1))]
            [else (park-and-acquire lock)]))))
    (define (park-and-acquire lock)
      (let loop ()
        (let ([v (unbox lock)])
          (cond
            [(fxeven? v)
             ;; after parking, take the lock with bit 1 set, since other
             ;; writers may still be parked
             (if (box-cas! lock v (fx+ v 3))
                 (memory-order-acquire)
                 (loop))]
            [else
             (when (or (fx= 2 (fxand v 2))
                       (box-cas! lock v (fxior v 2)))
               (mutex-acquire park-mutex)
               (when (fx= 3 (fxand (unbox lock) 3))
                 (condition-wait park-cond park-mutex))
               (mutex-release park-mutex))
             (loop)]))))
    (define (lock-release/slow lock next)
      ;; bit 1 is set, and only the holder changes it from here
      (set-box! lock next)
      (mutex-acquire park-mutex)
      (condition-broadcast park-cond)
      (mutex-release park-mutex))
    ;; A reader takes no lock and ignores changes to bit 1; see `seq`
    ;; about safety, which holds on x86_64 only
    (define-syntax-rule (with-eq-read-lock lock e)
      (let ([l lock])
        (cond
          [l
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
                    (if (fx= (fxand before -3) (fxand (unbox l) -3))
                        v
                        (retry)))])))]
          [else e])))
    (define (wait-for-writer l)
      (let loop ([n 0])
        (cond
          [(fxeven? (unbox l)) (void)]
          [(fx< n 100) (loop (fx+ n 1))]
          [else
           ;; parks if the writer takes a while
           (lock-acquire l)
           (lock-release l)])))]

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
    (define-box-lock-ops lock-acquire lock-release fast-acquire fast-release)
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
      (spin-until (fxeven? (unbox l))))])])
