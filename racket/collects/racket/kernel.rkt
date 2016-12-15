(module kernel '#%kernel
  (#%require "private/cons.rkt")
  (#%provide cons list (all-from-except '#%kernel))
  #;
  (#%declare #:cross-phase-persistent)
  #;
  (module reader syntax/module-reader
    #:language 'racket/kernel))
