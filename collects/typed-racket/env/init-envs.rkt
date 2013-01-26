#lang racket/base

;; Support for defining the initial TR environment

(require "../utils/utils.rkt"
         "../utils/tc-utils.rkt"
         "global-env.rkt"
         "type-name-env.rkt"
         "type-alias-env.rkt"
         "mvar-env.rkt" "pconverter.rkt"
         (rep type-rep object-rep filter-rep rep-utils free-variance)
         (for-template (rep type-rep object-rep filter-rep)
                       (types union abbrev)
                       racket/shared racket/base)
         (for-syntax syntax/parse racket/base)
         (types abbrev)
         racket/syntax racket/dict
         mzlib/pconvert racket/match)

(provide ;; convenience form for defining an initial environment
         ;; used by "base-special-env.rkt" and "base-contracted.rkt"
         define-initial-env
         initialize-type-name-env
         initialize-type-env
         converter
         bound-in-this-module
         tname-env-init-code
         tvariance-env-init-code
         talias-env-init-code
         env-init-code
         mvar-env-init-code )

(define-syntax (define-initial-env stx)
  (syntax-parse stx
    [(_ initialize-env [id-expr ty] ...)
     #`(begin
         (define initial-env (make-env [id-expr (λ () ty)] ... ))
         (define (initialize-env) (initialize-type-env initial-env))
         (provide initialize-env))]))

(define (initialize-type-name-env initial-type-names)
  (for-each (lambda (nm/ty) (register-resolved-type-alias (car nm/ty) (cadr nm/ty))) initial-type-names))

(define (initialize-type-env initial-env)
  (for-each (lambda (nm/ty) (register-type-if-undefined (car nm/ty) (cadr nm/ty))) initial-env))

(define (bound-in-this-module id)
  (let ([binding (identifier-binding id)])
    (if (and (list? binding) (module-path-index? (car binding)))
        (let-values ([(mp base) (module-path-index-split (car binding))])
          (not mp))
        #f)))

(define (tname-env-init-code)
  (define (f id ty)
    (if (bound-in-this-module id)
        #`(register-type-name #'#,id #,(datum->syntax #'here (print-convert ty)))
        #f))
  (parameterize ((current-print-convert-hook converter)
                 (show-sharing #f)
                 (booleans-as-true/false #f))
    #`(begin #,@(filter values (type-name-env-map f)))))

(define (tvariance-env-init-code)
  (define (f id var)
    (if (bound-in-this-module id)
        #`(register-type-variance! #'#,id (list #,@(map variance->binding var)))
        #f))
  (parameterize ((current-print-convert-hook converter)
                 (show-sharing #f)
                 (booleans-as-true/false #f))
    #`(begin #,@(filter values (type-variance-env-map f)))))


(define (talias-env-init-code)
  (define (f id ty)
    (if (bound-in-this-module id)
        #`(register-resolved-type-alias #'#,id #,(datum->syntax #'here (print-convert ty)))
        #f))
  (parameterize ((current-print-convert-hook converter)
                 (show-sharing #f)
                 (booleans-as-true/false #f))
    #`(begin #,@(filter values (type-alias-env-map f)))))

(define (env-init-code syntax-provide? provide-tbl def-tbl)
  (define (f id ty)
    (if (bound-in-this-module id)
        #`(register-type #'#,id #,(datum->syntax #'here (print-convert ty)))
        #f))
  (parameterize ((current-print-convert-hook converter)
                 (show-sharing #f)
                 (booleans-as-true/false #f))
    #`(begin #,@(filter values (type-env-map f)))))

(define (mvar-env-init-code mvar-env)
  (define (f id v)
    (and v (bound-in-this-module id)
         #`(register-mutated-var #'#,id)))
  #`(begin #,@(filter values (dict-map mvar-env f))))



