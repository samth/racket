#lang racket/base

(require
 (for-syntax racket/base  "env/env-req.rkt")
 (for-syntax "utils/timing.rkt") ;; only for timing/debugging
 ;; the below requires are needed since they provide identifiers
 ;; that may appear in the residual program
 "utils/utils.rkt"
 (for-syntax "utils/utils.rkt")
 "utils/any-wrap.rkt" unstable/contract racket/contract/parametric)

(provide (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction])
         with-type
         (for-syntax do-standard-inits))

(begin-for-syntax 
  (define-syntax-rule (dyn-req/time mod id)
    (let ([v (log-time (format "loading `~a`" 'mod) (dynamic-require 'mod 'id))])
      (log-time (format "invoking `~a`" 'mod) (v)))))

(define-for-syntax initialized #f)
(define-for-syntax (do-standard-inits)
  (unless initialized
    (dyn-req/time typed-racket/base-env/base-structs initialize-structs)
    (dyn-req/time typed-racket/base-env/base-env-indexing initialize-indexing)
    (dyn-req/time typed-racket/base-env/base-env init)
    (dyn-req/time typed-racket/base-env/base-env-numeric init)
    (dyn-req/time typed-racket/base-env/base-special-env initialize-special)
    (dyn-req/time typed-racket/base-env/base-contracted initialize-contracted)
    (log-time "loading `base-types`"
              (dynamic-require 
               '(submod typed-racket/base-env/base-types #%type-decl) #f))
    (set! initialized #t))
  (log-time "do-requires" (do-requires)))

(define-syntax-rule (drivers [name sym] ...)
  (begin
    (define-syntax (name stx)
      (log-time 
       (format "running ~a" (syntax-source stx))
       (define f (log-time "loading core" (dynamic-require 'typed-racket/core 'sym)))      
       (log-time "calling core" (f stx do-standard-inits))))
    ...))

(drivers [module-begin mb-core] [top-interaction ti-core] [with-type wt-core])
