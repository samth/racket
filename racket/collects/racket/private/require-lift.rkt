(module require-transform '#%kernel
  (#%require "define-et-al.rkt")

  (#%provide syntax-local-lift-require-definition-param)

  (-define syntax-local-lift-require-definition-param
    (make-parameter #f)))
