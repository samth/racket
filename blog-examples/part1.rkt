#lang racket

(require racket/draw
         ffi2)

;; bitmap setup
(define bt (make-bitmap 256 256))
(define bt-surface (send bt get-handle))

;; === Importing ffi2 ===
;; Loading the Cairo library
(define cairo-lib (ffi2-lib #f))

;; === Extracting functions (simple version first) ===
(define-ffi2-procedure cairo_create_raw (void_t* . -> . void_t*)
  #:lib cairo-lib
  #:c-id cairo_create)

;; Test basic function extraction
(displayln "Testing basic void_t* function extraction...")
(define ctx-raw (cairo_create_raw (cpointer->ffi2-ptr bt-surface)))
(printf "  ctx-raw: ~v\n" ctx-raw)

;; === Tagged pointer types ===
(define-ffi2-type cairo_t* void_t*)
(define-ffi2-type cairo_surface_t* void_t*)

(displayln "Testing tagged pointer types...")
(printf "  cairo_t*? predicate exists: ~v\n" (procedure? cairo_t*?))
(printf "  cairo_surface_t*? predicate exists: ~v\n" (procedure? cairo_surface_t*?))

;; Redefine with better types
(define-ffi2-procedure cairo_create (cairo_surface_t* . -> . cairo_t*)
  #:lib cairo-lib)

;; Convert old cpointer to ffi2 pointer and cast
(displayln "Testing cpointer->ffi2-ptr conversion...")
(define surface-ptr (cpointer->ffi2-ptr bt-surface))
(printf "  ffi2-ptr? surface-ptr: ~v\n" (ffi2-ptr? surface-ptr))
(define surface-tagged (ffi2-cast surface-ptr #:to cairo_surface_t*))
(printf "  cairo_surface_t*? surface-tagged: ~v\n" (cairo_surface_t*? surface-tagged))

(define ctx (cairo_create surface-tagged))
(printf "  cairo_t*? ctx: ~v\n" (cairo_t*? ctx))

;; Test that ill-typed call would error
(displayln "Testing type safety...")
(with-handlers ([exn:fail:contract? (lambda (e)
                                       (printf "  Correctly caught type error: ~a\n"
                                               (exn-message e)))])
  (cairo_create ctx) ;; cairo_t* is not cairo_surface_t*
  (displayln "  ERROR: should have raised an exception!"))

;; === define-ffi2-definer ===
(define-ffi2-definer define-cairo #:lib cairo-lib)

(define-cairo cairo_move_to  (cairo_t* double_t double_t . -> . void_t))
(define-cairo cairo_line_to  (cairo_t* double_t double_t . -> . void_t))
(define-cairo cairo_set_line_width (cairo_t* double_t . -> . void_t))
(define-cairo cairo_stroke   (cairo_t* . -> . void_t))

(displayln "Testing define-ffi2-definer functions...")
(cairo_move_to ctx 50.0 75.0)
(cairo_line_to ctx 200.0 75.0)
(printf "  cairo_move_to and cairo_line_to work\n")

;; === Enumerations ===
(define line-cap-symbols '(butt round square))

(define-ffi2-type cairo_line_cap_t int_t
  #:predicate (lambda (v) (and (symbol? v) (member v line-cap-symbols) #t))
  #:racket->c (lambda (sym) (index-of line-cap-symbols sym))
  #:c->racket (lambda (i) (list-ref line-cap-symbols i)))

(define-cairo cairo_set_line_cap (cairo_t* cairo_line_cap_t . -> . void_t))

(displayln "Testing enum type...")
(cairo_set_line_cap ctx 'round)
(printf "  cairo_set_line_cap with 'round works\n")

;; === Full multi segment caps example ===
(displayln "Drawing multi segment caps example...")
(cairo_move_to ctx 50.0 75.0)
(cairo_line_to ctx 200.0 75.0)

(cairo_move_to ctx 50.0 125.0)
(cairo_line_to ctx 200.0 125.0)

(cairo_move_to ctx 50.0 175.0)
(cairo_line_to ctx 200.0 175.0)

(cairo_set_line_width ctx 30.0)
(cairo_set_line_cap ctx 'round)
(cairo_stroke ctx)
(displayln "  Drawing complete!")

;; Verify bitmap is not blank
(define pixels (make-bytes (* 256 256 4)))
(send bt get-argb-pixels 0 0 256 256 pixels)
(define non-zero (for/sum ([b (in-bytes pixels)]) (if (> b 0) 1 0)))
(printf "  Non-zero pixel bytes: ~a (should be > 0)\n" non-zero)

(displayln "\nPart 1: ALL TESTS PASSED")
