(module kernel '#%kernel
  (#%require "cons.rkt")
  (#%provide cons list (all-from-except '#%kernel)))
