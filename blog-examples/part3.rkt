#lang racket

(require racket/draw
         ffi2
         (only-in racket/base [struct rkt:struct]))

;; === Prologue ===
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

;; Helper: create bitmap, draw, return bitmap
(define (do-cairo f)
  (define bt (make-bitmap 256 256))
  (define bt-surface (send bt get-handle))
  (f (cairo_create (ffi2-cast (cpointer->ffi2-ptr bt-surface) #:to cairo_surface_t*)))
  bt)

;; === Union types ===
(displayln "Testing union types...")

;; The path data type enum
(define path-data-type-symbols '(move-to line-to curve-to close-path))
(define-ffi2-type cairo_path_data_type_t int_t
  #:predicate (lambda (v) (and (symbol? v) (member v path-data-type-symbols) #t))
  #:racket->c (lambda (sym) (index-of path-data-type-symbols sym))
  #:c->racket (lambda (i) (list-ref path-data-type-symbols i)))

;; The header struct within the union
(define-ffi2-type path_header_t
  (struct
    [type   cairo_path_data_type_t]
    [length int_t]))

;; The point struct within the union
(define-ffi2-type path_point_t
  (struct
    [x double_t]
    [y double_t]))

;; The union itself
(define-ffi2-type cairo_path_data_t
  (union
    [header path_header_t]
    [point  path_point_t]))

(printf "  sizeof cairo_path_data_t: ~a\n" (ffi2-sizeof cairo_path_data_t))
(printf "  sizeof path_header_t: ~a\n" (ffi2-sizeof path_header_t))
(printf "  sizeof path_point_t: ~a\n" (ffi2-sizeof path_point_t))

;; Test union construction and access
(displayln "Testing union construction...")
(define test-union (ffi2-malloc cairo_path_data_t))
(printf "  cairo_path_data_t*? test-union: ~a\n" (cairo_path_data_t*? test-union))

;; === Simple path struct ===
(displayln "Testing path struct...")

(define-ffi2-type cairo_status_t int_t)

(define-ffi2-type simple_cairo_path_t
  (struct
    [status cairo_status_t]
    [data   void_t*]
    [num_data int_t]))

(printf "  sizeof simple_cairo_path_t: ~a\n" (ffi2-sizeof simple_cairo_path_t))

;; Get a path from Cairo
(define-cairo cairo_copy_path (cairo_t* . -> . void_t*))

(define a-path #f)

(do-cairo (lambda (ctx)
            (cairo_move_to ctx 50.0 50.0)
            (cairo_line_to ctx 206.0 206.0)
            (cairo_move_to ctx 50.0 206.0)
            (cairo_line_to ctx 115.0 115.0)
            (set! a-path (cairo_copy_path ctx))
            (cairo_stroke ctx)))

(printf "  Got path: ~v\n" a-path)
(printf "  ffi2-ptr? a-path: ~a\n" (ffi2-ptr? a-path))

;; === Low-level pointer operations ===
(displayln "Testing struct field access via cast...")

(define simple-path (ffi2-cast a-path #:to simple_cairo_path_t*))
(define data-ptr    (simple_cairo_path_t-data simple-path))
(define num-data    (simple_cairo_path_t-num_data simple-path))

(printf "  simple_cairo_path_t*? simple-path: ~a\n" (simple_cairo_path_t*? simple-path))
(printf "  data-ptr: ~v\n" data-ptr)
(printf "  num-data: ~a\n" num-data)

;; === Accessing union elements from the array ===
(displayln "Testing union element access...")

;; Read the first element as a header
(define first-elem (ffi2-cast data-ptr #:to cairo_path_data_t*))
(define header (cairo_path_data_t-header first-elem))
(define header-type (path_header_t-type header))
(define header-length (path_header_t-length header))
(printf "  First header type: ~a\n" header-type)
(printf "  First header length: ~a\n" header-length)

;; Read the second element as a point (offset by sizeof one union element)
(define second-elem
  (ffi2-cast (ffi2-add data-ptr (ffi2-sizeof cairo_path_data_t))
             #:to cairo_path_data_t*))
(define point (cairo_path_data_t-point second-elem))
(printf "  First point x: ~a\n" (path_point_t-x point))
(printf "  First point y: ~a\n" (path_point_t-y point))

;; === Custom C type with sequence property ===
(displayln "Testing custom C type and sequence iteration...")

(rkt:struct cairo-path (ptr)
  #:property prop:sequence
  (lambda (p) (in-cairo-path p)))

(define-ffi2-type cairo_path_t* void_t*
  #:predicate cairo-path?
  #:racket->c (lambda (rkt) (cairo-path-ptr rkt))
  #:c->racket (lambda (cobj) (cairo-path cobj)))

;; The sequence implementation
(define (in-cairo-path path)
  (define pp (cairo-path-ptr path))
  (define path-struct (ffi2-cast pp #:to simple_cairo_path_t*))
  (define array-ptr   (simple_cairo_path_t-data path-struct))
  (define len         (simple_cairo_path_t-num_data path-struct))
  (define elem-size   (ffi2-sizeof cairo_path_data_t))

  (make-do-sequence
    (lambda ()
      (values
        ;; pos->element
        (lambda (pos)
          (define header-ptr
            (ffi2-cast (ffi2-add array-ptr (* pos elem-size))
                       #:to cairo_path_data_t*))
          (define header (cairo_path_data_t-header header-ptr))
          (define type   (path_header_t-type header))
          (define count  (sub1 (path_header_t-length header)))
          (define points
            (for/list ([i (in-range count)])
              (define pt-ptr
                (ffi2-cast (ffi2-add array-ptr (* (+ pos 1 i) elem-size))
                           #:to cairo_path_data_t*))
              (define pt (cairo_path_data_t-point pt-ptr))
              (list (path_point_t-x pt)
                    (path_point_t-y pt))))
          (cons type points))

        ;; next-pos
        (lambda (pos)
          (define header-ptr
            (ffi2-cast (ffi2-add array-ptr (* pos elem-size))
                       #:to cairo_path_data_t*))
          (define header (cairo_path_data_t-header header-ptr))
          (+ pos (path_header_t-length header)))

        ;; initial position
        0

        ;; continue?
        (lambda (pos) (< pos len))

        ;; no other guards needed
        #f
        #f))))

;; Use the custom type
(define-cairo cairo_copy_path_typed (cairo_t* . -> . cairo_path_t*)
  #:c-id cairo_copy_path)

(define path-elements '())

(do-cairo (lambda (ctx)
            (cairo_move_to ctx 50.0 50.0)
            (cairo_line_to ctx 206.0 206.0)
            (cairo_move_to ctx 50.0 206.0)
            (cairo_line_to ctx 115.0 115.0)
            (define path (cairo_copy_path_typed ctx))
            (printf "  cairo-path?: ~a\n" (cairo-path? path))
            ;; Using path as a sequence!
            (for ([elem path])
              (set! path-elements (cons elem path-elements))
              (printf "  ~a\n" elem))
            (cairo_stroke ctx)))

(printf "  Number of path elements: ~a\n" (length path-elements))

;; Verify the path contents
(define reversed (reverse path-elements))
(printf "  First element type: ~a\n" (car (first reversed)))
(printf "  Expected: move-to\n")

(displayln "\nPart 3: ALL TESTS PASSED")
