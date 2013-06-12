#lang setup/infotab

(define raco-commands
  '(("check-requires"
     (submod macro-debugger/analysis/check-requires main)
     "check for useless requires"
     #f)
    ("show-dependencies"
     (submod macro-debugger/analysis/show-dependencies main)
     "show module dependencies"
     #f)))
(define single-collection "macro-debugger")
(define build-deps '("racket-doc"))
(define scribblings '(("macro-debugger.scrbl" () (tool-library))))
