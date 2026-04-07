#lang racket/base

(require (for-template racket/base))

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
  (if (module-compiled-cross-phase-persistent? compiled)
      null
      (let ([module-path
             (if (path? module-path)
                 (path->complete-path module-path)
                 module-path)])
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
          (syntax-case e (quote)
            [(_ m mz (#%mb req (quote (spec ...))))
             (for/list ([p (in-list (syntax->datum #'(spec ...)))])
               (strip-module-variable-reference p))]
            [_ (error who
                      "expansion mismatch when getting runtime paths: ~e"
                      (syntax->datum e))])))))
