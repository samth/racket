(module kernel '#%kernel

  (#%provide (all-from-except '#%kernel))

  (#%declare #:cross-phase-persistent)

  (module reader syntax/module-reader
    #:language 'racket/kernel))
