#lang racket/base
(require "../common/small-hash.rkt")

(provide (struct-out module-registry)
         make-module-registry
         registry-call-with-lock
         registry-call-with-loading-claim

         make-in-progress
         in-progress?
         in-progress-done!
         in-progress-wait)

(struct module-registry (declarations  ; resolved-module-path -> module
                         lock-box      ; reentrant lock to guard registry for use by on-demand visits
                         loading))     ; small hash: resolved-module-path -> in-progress, for the module name resolver

(define (make-module-registry)
  (module-registry (make-hasheq) (box #f) (make-small-hasheq)))

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
;; so another thread shouldn't wait for it
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

;; Calls `load` to load the module `name`, unless another thread is
;; already loading it; in that case, waits for the other thread and
;; returns #f without calling `load`, so that the caller can check
;; whether the other thread's load declared the module
(define (registry-call-with-loading-claim r name load)
  (define loading (module-registry-loading r))
  (define p (make-in-progress))
  (let claim ()
    (define other-p (small-hash-ref loading name #f))
    (cond
      [(and other-p (not (in-progress-abandoned? other-p)))
       (cond
         [(in-progress-wait other-p r) #f]
         [else
          ;; Nested load in the same thread, or waiting might deadlock
          (load)
          #t])]
      [(small-hash-cas! loading name other-p p)
       (dynamic-wind
        void
        load
        (lambda ()
          (small-hash-cas! loading name p #f)
          (in-progress-done! p)))
       #t]
      [else (claim)])))
