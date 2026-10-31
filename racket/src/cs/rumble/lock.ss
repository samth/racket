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
  (define (lock-acquire/a lock) (void))
  (define (lock-release/a lock) (void))]
 [else
  (define transition-lock (make-mutex))
  (define transition-cond (make-condition))

  ;; A nonscheduler lock is for `eq?`- and `eqv?`-based hash tables,
  ;; there the operation is atomic from the Racket perspective, and
  ;; it's worth using a CAS to make the uncontended case fast.

  ;; Until a place forks its first extra pthread (for a future or a
  ;; parallel thread), only one OS thread can touch the place's tables,
  ;; so the CAS is skipped; blocking engine swaps is still needed. The
  ;; flag is set by the only OS thread of the place, which is not in
  ;; the middle of a locked operation, and it is never cleared, so an
  ;; acquire and its release always agree on whether the CAS is used.
  (define-syntax-rule (place-in-parallel?)
    (eq? #t (#%vector-ref (place-registers) PARALLEL-PTHREADS-INDEX)))

  ;; box of
  ;;  #f - unlocked, uncontended
  ;;  #t - locked, uncontended
  ;;  <mutex> - contended and managed by mutex
  ;;  <void> - locked, contended, switches to <mutex> on release

  (define (make-nonscheduler-lock)
    (box #f))

  (define (lock-acquire lock)
    (cond
      [(not lock) (disable-interrupts)]
      [(box? lock)
       ;; need to block engine swaps to provide atomic behavior,
       ;; and the engine-specific opeartion is cheaper than disabling all interrupts
       (start-engine-uninterrupted 'lock-acquire)
       (when (place-in-parallel?)
         (cond
           [(box-cas! lock #f #t) (memory-order-acquire)]
           [else (#%$app/no-inline lock-acquire/slow lock)]))]
      [else
       (scheduler-lock-acquire lock)]))

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

  (define (lock-release lock)
    (cond
      [(not lock) (enable-interrupts) (#%void)]
      [(box? lock)
       (when (place-in-parallel?)
         (memory-order-release)
         (cond
           [(box-cas! lock #t #f) (void)]
           [else
            (#%$app/no-inline lock-release-slow lock)]))
       (end-engine-uninterrupted 'lock-release)
       (void)]
      [else
       (scheduler-lock-release lock)]))

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

  ;; The "/a" variants are for an operation that is atomic by itself
  ;; with respect to engines, such as `eq-hashtable-ref`, so they
  ;; have nothing to do while the place has only one OS thread
  (define (lock-acquire/a lock) (when (place-in-parallel?) (lock-acquire lock)))
  (define (lock-release/a lock) (when (place-in-parallel?) (lock-release lock)))])
