#lang setup/infotab

(define scribblings
  '(("scribblings/future-visualizer.scrbl" (multi-page) (tool))))

(define deps '("gui-lib" "typed-racket"))
(define build-deps '("racket-doc"))
(define single-collection "future-visualizer")
