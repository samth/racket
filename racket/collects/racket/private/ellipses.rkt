(module ellipses "kernel.rkt"
  (#%require (for-syntax "kernel.rkt"))

  (#%provide ... _)

  (define-syntaxes (...)
    (lambda (stx)
      (raise-syntax-error #f "ellipses not allowed as an expression" stx)))

  (define-syntaxes (_)
    (lambda (stx)
      (raise-syntax-error #f "wildcard not allowed as an expression" stx))))
