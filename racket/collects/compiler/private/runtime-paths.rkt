#lang racket/base

(provide get-module-runtime-paths)

(define (strip-module-variable-reference spec)
  ;; The variable-reference slot is only useful for resolving the path in
  ;; the current namespace; callers that inspect or relocate the path do not
  ;; want to retain that namespace reference.
  (if (and (pair? spec)
           (eq? 'module (car spec)))
      (list 'module (cadr spec))
      spec))

(define (get-module-runtime-paths module-path compiled
                                  #:namespace [ns (current-namespace)]
                                  #:who [who 'get-module-runtime-paths])
  (parameterize ([current-namespace ns])
    (unless (module-declared? module-path)
      (parameterize ([current-module-declare-name
                      (module-path-index-resolve
                       (module-path-index-join module-path #f))])
        (eval compiled)))
    (define e
      (expand `(,#'module m racket/kernel
                 (#%require (only ,module-path)
                            racket/runtime-path)
                 (runtime-paths ,module-path))))
    (define specs
      (syntax-case e (quote #%module-begin)
        [(_ m _ (#%module-begin _ (quote (spec ...))))
         (syntax->datum #'(spec ...))]
        [_ (error who
                  "expansion mismatch when getting runtime paths: ~e"
                  (syntax->datum e))]))
    (map strip-module-variable-reference specs)))
