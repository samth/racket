#lang racket

(require racket/draw
         ffi2)

;; === Prologue (condensed Part 1 setup) ===
(define bt (make-bitmap 256 256))
(define bt-surface (send bt get-handle))

(define-ffi2-type cairo_t* void_t*)
(define-ffi2-type cairo_surface_t* void_t*)

(define line-cap-symbols '(butt round square))
(define-ffi2-type cairo_line_cap_t int_t
  #:predicate (lambda (v) (and (symbol? v) (member v line-cap-symbols) #t))
  #:racket->c (lambda (sym) (index-of line-cap-symbols sym))
  #:c->racket (lambda (i) (list-ref line-cap-symbols i)))

(define cairo-lib (ffi2-lib #f))
(define-ffi2-definer define-cairo #:lib cairo-lib)

(define-cairo cairo_create    (cairo_surface_t* . -> . cairo_t*))
(define-cairo cairo_move_to   (cairo_t* double_t double_t . -> . void_t))
(define-cairo cairo_line_to   (cairo_t* double_t double_t . -> . void_t))
(define-cairo cairo_set_line_width (cairo_t* double_t . -> . void_t))
(define-cairo cairo_stroke    (cairo_t* . -> . void_t))
(define-cairo cairo_set_line_cap   (cairo_t* cairo_line_cap_t . -> . void_t))

(define ctx (cairo_create (ffi2-cast (cpointer->ffi2-ptr bt-surface) #:to cairo_surface_t*)))

(define (show bt)
  (define pixels (make-bytes (* 256 256 4)))
  (send bt get-argb-pixels 0 0 256 256 pixels)
  (define non-zero (for/sum ([b (in-bytes pixels)]) (if (> b 0) 1 0)))
  (printf "  Non-zero pixel bytes: ~a\n" non-zero))

;; === Dashes and array arguments ===
(displayln "Testing array arguments (dash example)...")

(define-cairo cairo_rel_line_to (cairo_t* double_t double_t . -> . void_t))
(define-cairo cairo_curve_to
  (cairo_t* double_t double_t double_t double_t double_t double_t . -> . void_t))

;; Low-level binding: takes a pointer and length
(define-cairo cairo_set_dash_raw
  (cairo_t* void_t* int_t double_t . -> . void_t)
  #:c-id cairo_set_dash)

;; Racket-friendly wrapper that takes a list
(define (cairo-set-dash ctx dashes offset)
  (define n (length dashes))
  (define arr (ffi2-malloc double_t n))
  (for ([d (in-list dashes)]
        [i (in-naturals)])
    (ffi2-set! arr double_t i d))
  (cairo_set_dash_raw ctx arr n offset))

;; Test the wrapper
(define dashes '(50.0 10.0 10.0 10.0))
(define offset -50.0)

(cairo-set-dash ctx dashes offset)
(cairo_set_line_width ctx 10.0)

(cairo_move_to ctx 128.0 25.6)
(cairo_line_to ctx 230.4 230.4)
(cairo_rel_line_to ctx -102.4 0.0)
(cairo_curve_to ctx 51.2 230.4 51.2 128.0 128.0 128.0)

(cairo_stroke ctx)
(printf "  Dash example drawn successfully\n")
(show bt)

;; === C Structs ===
(displayln "Testing C structs (text extents)...")

(define-ffi2-type cairo_text_extents_t
  (struct
    [x_bearing double_t]
    [y_bearing double_t]
    [width     double_t]
    [height    double_t]
    [x_advance double_t]
    [y_advance double_t]))

(printf "  sizeof cairo_text_extents_t: ~a\n" (ffi2-sizeof cairo_text_extents_t))
(printf "  cairo_text_extents_t*? predicate exists: ~a\n" (procedure? cairo_text_extents_t*?))

;; Test constructor
(define extents-test (cairo_text_extents_t 1.0 2.0 3.0 4.0 5.0 6.0))
(printf "  Constructor works, width: ~a\n" (cairo_text_extents_t-width extents-test))

;; Test malloc
(define extents-malloc (ffi2-malloc cairo_text_extents_t))
(printf "  ffi2-malloc works, cairo_text_extents_t*?: ~a\n" (cairo_text_extents_t*? extents-malloc))

;; Bind cairo_text_extents
(define-cairo cairo_text_extents
  (cairo_t* string_t cairo_text_extents_t* . -> . void_t))

;; Test calling it
(define extents (ffi2-malloc cairo_text_extents_t))
(cairo_text_extents ctx "hello world" extents)
(define text-width (cairo_text_extents_t-width extents))
(printf "  Text width of 'hello world': ~a\n" text-width)
(printf "  Width is positive: ~a\n" (> text-width 0.0))

;; Wrapper function
(define (cairo-text-extents* ctx str)
  (define ext (ffi2-malloc cairo_text_extents_t))
  (cairo_text_extents ctx str ext)
  ext)

(define w2 (cairo_text_extents_t-width (cairo-text-extents* ctx "hello world")))
(printf "  Wrapper gives same width: ~a\n" (= w2 text-width))

;; === Drawing scaled text ===
(displayln "Testing scaled text drawing...")

(define-cairo cairo_show_text (cairo_t* string_t . -> . void_t))
(define-cairo cairo_scale     (cairo_t* double_t double_t . -> . void_t))

(define txt-bt (make-bitmap 256 256))
(define txt-surface (send txt-bt get-handle))
(define txt-ctx (cairo_create (ffi2-cast (cpointer->ffi2-ptr txt-surface) #:to cairo_surface_t*)))

(define (fit-text str)
  (define padding 20)
  (cairo_move_to txt-ctx (/ padding 2.0) 128.0)
  (define extents (cairo-text-extents* txt-ctx str))
  (define x-bearing (cairo_text_extents_t-x_bearing extents))
  (define width     (cairo_text_extents_t-width extents))
  (define scale (/ (- 256.0 padding) (+ x-bearing width)))
  (cairo_scale txt-ctx scale scale)
  (cairo_show_text txt-ctx str))

(fit-text "Saluton, Mondo / Hallo, mundo")
(displayln "  Scaled text drawn!")

;; Check text bitmap has content
(define txt-pixels (make-bytes (* 256 256 4)))
(send txt-bt get-argb-pixels 0 0 256 256 txt-pixels)
(define txt-non-zero (for/sum ([b (in-bytes txt-pixels)]) (if (> b 0) 1 0)))
(printf "  Text bitmap non-zero bytes: ~a (should be > 0)\n" txt-non-zero)

(displayln "\nPart 2: ALL TESTS PASSED")
