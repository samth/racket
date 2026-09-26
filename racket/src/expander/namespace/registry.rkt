#lang racket/base
(require ffi/unsafe/atomic)

(provide (struct-out module-registry)
         make-module-registry
         registry-call-with-lock
         registry-call-with-claim-lock
         registry-call-with-loading-claim

         make-in-progress
         in-progress?
         in-progress-mine?
         in-progress-abandoned?
         in-progress-done!
         in-progress-wait)

(struct module-registry (declarations  ; resolved-module-path -> module
                         lock-box      ; reentrant lock to guard registry for use by on-demand visits
                         loading       ; resolved-module-path -> in-progress, for loads by the module name resolver
                         claim-lock))  ; uninterruptible lock for `loading` and for creating module instances

(define (make-module-registry)
  (module-registry (make-hasheq) (box #f) (make-hasheq) (make-uninterruptible-lock)))

(define (registry-call-with-lock r proc)
  (define lock-box (module-registry-lock-box r))
  (let loop ()
    (define v (unbox lock-box))
    (cond
     [(or (not v)
          (sync/timeout 0 (car v) (let ([t (weak-box-value (cdr v))])
                                    (if (eq? t (current-thread))
                                        never-evt
                                        (or t always-evt)))))
      ;; Lock holder is released its semaphore or terminated
      (define sema (make-semaphore))
      (define lock (cons (semaphore-peek-evt sema) (make-weak-box (current-thread))))
      ((dynamic-wind
        void
        (lambda ()
          (cond
            [(box-cas! lock-box v lock)
             ;; This thread became the lock holder
             (call-with-values
              proc
              (lambda results
                (lambda () (apply values results))))]
            [else
             ;; CAS failed; take it from the top
             (lambda () (loop))]))
        (lambda ()
          (semaphore-post sema))))]
     [(eq? (current-thread) (weak-box-value (cdr v)))
      ;; This thread already holds the lock
      (proc)]
     [else
      ; Wait and try again:
      (sync (car v) (or (weak-box-value (cdr v)) always-evt))
      (loop)])))

(define (registry-lock-held? r)
  (define v (unbox (module-registry-lock-box r)))
  (and v
       (eq? (current-thread) (weak-box-value (cdr v)))
       (not (sync/timeout 0 (car v)))))

;; ----------------------------------------

;; A thread that loads a module or runs a phase level of a module body
;; records an `in-progress` value, so that another thread that needs
;; the same module can wait for it, instead of loading the module again
;; or using a partially run body. Unlike the registry lock, nothing is
;; held while a module is loaded or run, since a module body can run
;; indefinitely (as for a program's main module).
(struct in-progress (thread done))

(define (make-in-progress)
  (in-progress (current-thread) (make-semaphore)))

(define (in-progress-mine? p)
  (eq? (current-thread) (in-progress-thread p)))

;; The owning thread ended or is suspended (and, if `p` is still
;; recorded, it hasn't finished); a suspended owner might never resume,
;; so another thread shouldn't wait for it. Doesn't block, so it can be
;; used with the claim lock.
(define (in-progress-abandoned? p)
  (not (thread-running? (in-progress-thread p))))

(define (in-progress-done! p)
  (semaphore-post (in-progress-done p)))

;; Waits for another thread to finish, end, or be suspended, and returns
;; #t, unless the current thread is the owner (so the use is reentrant)
;; or holds the registry lock `r` (so the owner might need the lock to
;; finish); in those cases, returns #f without waiting
(define (in-progress-wait p r)
  (cond
    [(in-progress-mine? p) #f]
    [(registry-lock-held? r) #f]
    [else
     (define t (in-progress-thread p))
     (when (thread-running? t)
       (sync (semaphore-peek-evt (in-progress-done p))
             t
             (thread-suspend-evt t)))
     #t]))

;; Calls `thunk`, which must not block, while holding the registry's
;; claim lock
(define (registry-call-with-claim-lock r thunk)
  (define lock (module-registry-claim-lock r))
  (uninterruptible-lock-acquire lock)
  (begin0
    (thunk)
    (uninterruptible-lock-release lock)))

;; Calls `load` to load the module `name`, unless another thread is
;; already loading it; in that case, waits for the other thread and
;; returns #f without calling `load`, so that the caller can check
;; whether the other thread's load declared the module
(define (registry-call-with-loading-claim r name load)
  (define loading (module-registry-loading r))
  (define-values (p new?)
    (registry-call-with-claim-lock
     r
     (lambda ()
       (define p (hash-ref loading name #f))
       (cond
         [(and p (not (in-progress-abandoned? p)))
          (values p #f)]
         [else
          (define p (make-in-progress))
          (hash-set! loading name p)
          (values p #t)]))))
  (cond
    [new?
     (dynamic-wind
      void
      load
      (lambda ()
        (registry-call-with-claim-lock
         r
         (lambda ()
           (when (eq? p (hash-ref loading name #f))
             (hash-remove! loading name))))
        (in-progress-done! p)))
     #t]
    [(in-progress-wait p r)
     #f]
    [else
     ;; Nested load in the same thread, or waiting might deadlock
     (load)
     #t]))
