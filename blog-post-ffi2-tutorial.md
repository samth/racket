# Tutorial: Using Racket's ffi2

*Based on the original [FFI tutorial series](https://prl.khoury.northeastern.edu/blog/2016/06/27/tutorial-using-racket-s-ffi/) by Asumu Takikawa, rewritten for the new `ffi2` library.*

**Update:** this post is a single, comprehensive rewrite of the original three-part
series, updated for Racket's new `ffi2` library. The `ffi2` library provides a more
static, modern API for calling C code from Racket. It uses C-style `_t` naming
conventions (e.g., `int_t` instead of `_int`), a streamlined `->` function type
constructor, and a unified `define-ffi2-type` form for defining structs, unions,
arrays, pointer types, and custom type conversions.

I've seen several people ask for a tutorial on Racket's foreign function interface
(FFI). While the original `ffi/unsafe` tutorial covered the classic FFI, Racket now
has a new alternative: the `ffi2` library. This library combines ideas from the
original FFI (by Eli Barzilay and Dmitry Orlovsky) with Chez Scheme's ftype layer
(by Andy Keep), resulting in a cleaner, faster interface that is better tuned to
modern Racket (i.e., CS).

This tutorial will provide a step-by-step guide to `ffi2` using the
[Cairo](https://www.cairographics.org/) graphics library, which comes bundled with
Racket. All you need to follow along is a copy of Racket and ideally a DrRacket
window.

---

## Part 1: The Basics

### Setup

To start, let's aim to reproduce the output of the "multi segment caps" C sample
code on Cairo's [samples page](https://www.cairographics.org/samples/):

```c
cairo_move_to (cr, 50.0, 75.0);
cairo_line_to (cr, 200.0, 75.0);

cairo_move_to (cr, 50.0, 125.0);
cairo_line_to (cr, 200.0, 125.0);

cairo_move_to (cr, 50.0, 175.0);
cairo_line_to (cr, 200.0, 175.0);

cairo_set_line_width (cr, 30.0);
cairo_set_line_cap (cr, CAIRO_LINE_CAP_ROUND);
cairo_stroke (cr);
```

In order to draw this example, we need a Cairo surface. Here is some
boilerplate using `racket/draw` to set one up:

```racket
(require racket/draw)

(define bt (make-bitmap 256 256))
(define bt-surface (send bt get-handle))
```

The `get-handle` method extracts a low-level Cairo surface value that we can use
with the FFI.

### Importing ffi2

Our first real step is to import the `ffi2` library:

```racket
(require ffi2)
```

Unlike the old `ffi/unsafe`, the module is simply called `ffi2`. It is still
*unsafe*—it can cause your Racket process to segfault—so save your work frequently
if you're following along in DrRacket.

### Loading a library

Next, we load the Cairo library to obtain an `ffi2-lib` value, which is a handle
we use to access C functions:

```racket
(define cairo-lib (ffi2-lib #f))
```

Since Cairo has already been loaded by the Racket process (due to our
`racket/draw` import), we can pass `#f` to `ffi2-lib`. Normally you'd supply
the name of a shared library:

```racket
(define cairo-lib (ffi2-lib "libcairo" '("2" #f)))
```

### Extracting functions

With the old FFI, you used `get-ffi-obj` to pull out C functions at run-time. With
`ffi2`, the equivalent is `define-ffi2-procedure`. Let's bind `cairo_create`, which
has this C signature:

```c
cairo_t * cairo_create (cairo_surface_t *target);
```

In `ffi2`, we describe function types using the `->` constructor (instead of the
old `_fun`). And types use a `_t` suffix instead of a leading underscore—so
`_pointer` becomes `void_t*`, `_double` becomes `double_t`, etc.

Here's a simple first binding:

```racket
(define-ffi2-procedure cairo_create (void_t* . -> . void_t*)
  #:lib cairo-lib)
```

Since `bt-surface` is an old-style `cpointer` from `racket/draw`, we first
convert it to an `ffi2` pointer with `cpointer->ffi2-ptr`:

```racket
(define ctx (cairo_create (cpointer->ffi2-ptr bt-surface)))
ctx
```

### Interlude: more type safety with tagged pointers

Using bare `void_t*` for everything is unsafe—it lets you mix up different
kinds of pointers. With the old FFI, you'd use `define-cpointer-type` to create
tagged pointer types. With `ffi2`, you use `define-ffi2-type` to create a new
pointer type that is a subtype of `void_t*`:

```racket
(define-ffi2-type cairo_t* void_t*)
(define-ffi2-type cairo_surface_t* void_t*)
```

This automatically creates:
- `cairo_t*` — the tagged pointer type
- `cairo_t*?` — a predicate for checking if a pointer has this tag

We can then redefine `cairo_create` with better types:

```racket
(define-ffi2-procedure cairo_create (cairo_surface_t* . -> . cairo_t*)
  #:lib cairo-lib)
```

Now ill-typed calls will be caught:

```racket
;; This will error—a cairo_t* is not a cairo_surface_t*
(cairo_create (cairo_create (cpointer->ffi2-ptr bt-surface)))
```

Now we need to cast `bt-surface` to add the `cairo_surface_t*` tag:

```racket
(define ctx (cairo_create (ffi2-cast (cpointer->ffi2-ptr bt-surface)
                                     #:to cairo_surface_t*)))
```

### Reducing boilerplate with define-ffi2-definer

Writing `#:lib cairo-lib` for every function is verbose. The `ffi2` library
provides `define-ffi2-definer` (analogous to the old `define-ffi-definer`) to
create a macro that pre-fills the library:

```racket
(define-ffi2-definer define-cairo #:lib cairo-lib)
```

Now we can define Cairo bindings much more concisely. Note that with `ffi2`,
if you use C-style identifiers with underscores (e.g., `cairo_move_to`), the
name is automatically used as the C identifier—no `#:c-id` needed:

```racket
(define-cairo cairo_move_to  (cairo_t* double_t double_t . -> . void_t))
(define-cairo cairo_line_to  (cairo_t* double_t double_t . -> . void_t))
(define-cairo cairo_set_line_width (cairo_t* double_t . -> . void_t))
(define-cairo cairo_stroke   (cairo_t* . -> . void_t))
```

If you prefer Racket-style hyphenated names, you can use the `#:c-id` keyword:

```racket
(define-cairo cairo-move-to  (cairo_t* double_t double_t . -> . void_t)
  #:c-id cairo_move_to)
```

### Enumerations

The `cairo_set_line_cap` function takes a `cairo_line_cap_t` enum argument. With the
old FFI, you'd use the `_enum` form. With `ffi2`, you can create a custom type using
`define-ffi2-type` with `#:racket->c` and `#:c->racket` converters over an integer
base type:

```racket
(define line-cap-symbols '(butt round square))

(define-ffi2-type cairo_line_cap_t int_t
  #:predicate (lambda (v) (and (symbol? v) (member v line-cap-symbols) #t))
  #:racket->c (lambda (sym) (index-of line-cap-symbols sym))
  #:c->racket (lambda (i) (list-ref line-cap-symbols i)))

(define-cairo cairo_set_line_cap (cairo_t* cairo_line_cap_t . -> . void_t))
```

This sets up a type that converts between Racket symbols (`'butt`, `'round`,
`'square`) and their underlying integer representations, just like the old
`_enum` did.

### Putting it all together

Now we can transcribe the C example into Racket:

```racket
(cairo_move_to ctx 50.0 75.0)
(cairo_line_to ctx 200.0 75.0)

(cairo_move_to ctx 50.0 125.0)
(cairo_line_to ctx 200.0 125.0)

(cairo_move_to ctx 50.0 175.0)
(cairo_line_to ctx 200.0 175.0)

(cairo_set_line_width ctx 30.0)
(cairo_set_line_cap ctx 'round)
(cairo_stroke ctx)
```

To see the result, we can display the bitmap:

```racket
(require pict)
(linewidth 2 (frame (bitmap bt)))
```

---

## Part 2: Arrays, Computed Arguments, and Structs

### Prologue

Here's the complete setup code condensed from Part 1, which you can paste into
your definitions area:

```racket
#lang racket

(require racket/draw
         ffi2
         pict)

;; bitmap setup
(define bt (make-bitmap 256 256))
(define bt-surface (send bt get-handle))

;; tagged pointer types
(define-ffi2-type cairo_t* void_t*)
(define-ffi2-type cairo_surface_t* void_t*)

;; enum type
(define line-cap-symbols '(butt round square))
(define-ffi2-type cairo_line_cap_t int_t
  #:predicate (lambda (v) (and (symbol? v) (member v line-cap-symbols) #t))
  #:racket->c (lambda (sym) (index-of line-cap-symbols sym))
  #:c->racket (lambda (i) (list-ref line-cap-symbols i)))

;; library and definer
(define cairo-lib (ffi2-lib #f))
(define-ffi2-definer define-cairo #:lib cairo-lib)

;; foreign functions
(define-cairo cairo_create    (cairo_surface_t* . -> . cairo_t*))
(define-cairo cairo_move_to   (cairo_t* double_t double_t . -> . void_t))
(define-cairo cairo_line_to   (cairo_t* double_t double_t . -> . void_t))
(define-cairo cairo_set_line_width (cairo_t* double_t . -> . void_t))
(define-cairo cairo_stroke    (cairo_t* . -> . void_t))
(define-cairo cairo_set_line_cap   (cairo_t* cairo_line_cap_t . -> . void_t))

(define ctx (cairo_create (ffi2-cast (cpointer->ffi2-ptr bt-surface) #:to cairo_surface_t*)))

;; helper
(define (show bt)
  (linewidth 2 (frame (bitmap bt))))
```

### Dashes and array arguments

Let's look at the "dash" example from Cairo's [samples page](https://www.cairographics.org/samples/):

```c
double dashes[] = {50.0, 10.0, 10.0, 10.0};
int    ndash  = sizeof(dashes)/sizeof(dashes[0]);
double offset = -50.0;

cairo_set_dash (cr, dashes, ndash, offset);
cairo_set_line_width (cr, 10.0);

cairo_move_to (cr, 128.0, 25.6);
cairo_line_to (cr, 230.4, 230.4);
cairo_rel_line_to (cr, -102.4, 0.0);
cairo_curve_to (cr, 51.2, 230.4, 51.2, 128.0, 128.0, 128.0);

cairo_stroke (cr);
```

The most interesting function is `cairo_set_dash`, which takes an array argument.
The other new functions are straightforward:

```racket
(define-cairo cairo_rel_line_to (cairo_t* double_t double_t . -> . void_t))
(define-cairo cairo_curve_to
  (cairo_t* double_t double_t double_t double_t double_t double_t . -> . void_t))
```

The C type signature for `cairo_set_dash` is:

```c
void cairo_set_dash (cairo_t *cr,
                     const double *dashes,
                     int num_dashes,
                     double offset);
```

Note that `num_dashes` encodes the length of the `dashes` array. With `ffi2`,
we can handle the array by allocating memory, filling it, and passing it as a
pointer. Here's a wrapper approach:

```racket
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
```

This approach is more explicit than the old `_list` custom function type, but it
is also more transparent—you can see exactly what's happening with memory. The
wrapper converts a Racket list of doubles into a C array, computes the length
automatically, and calls the underlying C function.

Now we can use it naturally:

```racket
(cairo-set-dash ctx (list 50.0 10.0 10.0 10.0) -50.0)
```

No need to pass the length separately—the wrapper handles it.

Putting the dash example together:

```racket
(define dashes '(50.0 10.0 10.0 10.0))
(define offset -50.0)

(cairo-set-dash ctx dashes offset)
(cairo_set_line_width ctx 10.0)

(cairo_move_to ctx 128.0 25.6)
(cairo_line_to ctx 230.4 230.4)
(cairo_rel_line_to ctx -102.4 0.0)
(cairo_curve_to ctx 51.2 230.4 51.2 128.0 128.0 128.0)

(cairo_stroke ctx)
(show bt)
```

### C Structs

For a more advanced example, let's measure text to scale it into our bitmap.
The relevant Cairo function is `cairo_text_extents`:

```c
void cairo_text_extents (cairo_t *cr,
                         const char *utf8,
                         cairo_text_extents_t *extents);
```

Where `cairo_text_extents_t` is a struct:

```c
typedef struct {
    double x_bearing;
    double y_bearing;
    double width;
    double height;
    double x_advance;
    double y_advance;
} cairo_text_extents_t;
```

With the old FFI, you'd use `define-cstruct` to define this. With `ffi2`, you use
`define-ffi2-type` with the `struct` type constructor:

```racket
(define-ffi2-type cairo_text_extents_t
  (struct
    [x_bearing double_t]
    [y_bearing double_t]
    [width     double_t]
    [height    double_t]
    [x_advance double_t]
    [y_advance double_t]))
```

This single declaration automatically creates:
- `cairo_text_extents_t` — the struct type and constructor
- `cairo_text_extents_t*` — a pointer type for the struct
- `cairo_text_extents_t*?` — a predicate for struct pointers
- `cairo_text_extents_t-width`, `cairo_text_extents_t-x_bearing`, etc. — field accessors
- `set-cairo_text_extents_t-width!`, etc. — field mutators

You can construct instances directly:

```racket
;; Constructor fills all fields
(define extents (cairo_text_extents_t 0.0 0.0 0.0 0.0 0.0 0.0))
```

Or allocate one with `ffi2-malloc`:

```racket
(define extents (ffi2-malloc cairo_text_extents_t))
```

Now we can bind `cairo_text_extents`. The function writes into a struct pointer
that we provide:

```racket
(define-cairo cairo_text_extents
  (cairo_t* string_t cairo_text_extents_t* . -> . void_t))
```

Note the use of `string_t` instead of the old `_string`—`ffi2` uses `string_t`
for null-terminated C strings that convert to/from Racket strings automatically.

Using this function:

```racket
(define extents (ffi2-malloc cairo_text_extents_t))
(cairo_text_extents ctx "hello world" extents)
(cairo_text_extents_t-width extents)
```

This style is imperative—we have to manually allocate the struct. For a more
functional feel, we can write a wrapper:

```racket
(define (cairo-text-extents* ctx str)
  (define ext (ffi2-malloc cairo_text_extents_t))
  (cairo_text_extents ctx str ext)
  ext)
```

Now:

```racket
(cairo_text_extents_t-width (cairo-text-extents* ctx "hello world"))
```

### Drawing scaled text

With our text extents wrapper, let's implement a function that draws text scaled
to fit the bitmap width:

```racket
(define-cairo cairo_show_text (cairo_t* string_t . -> . void_t))
(define-cairo cairo_scale     (cairo_t* double_t double_t . -> . void_t))

(define txt-bt (make-bitmap 256 256))
(define txt-surface (send txt-bt get-handle))
(define txt-ctx (cairo_create (ffi2-cast (cpointer->ffi2-ptr txt-surface) #:to cairo_surface_t*)))

;; String -> Void
;; Draws a string scaled horizontally to fit the bitmap
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
(show txt-bt)
```

---

## Part 3: Pointers, Unions, and Custom Types

### Prologue

Let's set up a cleaner version of our bindings with a helper function:

```racket
#lang racket

(require racket/draw
         ffi2
         (only-in racket/base [struct rkt:struct])  ; ffi2 shadows `struct`
         pict)

;; tagged pointer types
(define-ffi2-type cairo_t* void_t*)
(define-ffi2-type cairo_surface_t* void_t*)

(define line-cap-symbols '(butt round square))
(define-ffi2-type cairo_line_cap_t int_t
  #:predicate (lambda (v) (and (symbol? v) (member v line-cap-symbols) #t))
  #:racket->c (lambda (sym) (index-of line-cap-symbols sym))
  #:c->racket (lambda (i) (list-ref line-cap-symbols i)))

(define cairo-lib (ffi2-lib #f))
(define-ffi2-definer define-cairo #:lib cairo-lib)

;; foreign functions
(define-cairo cairo_create    (cairo_surface_t* . -> . cairo_t*))
(define-cairo cairo_move_to   (cairo_t* double_t double_t . -> . void_t))
(define-cairo cairo_line_to   (cairo_t* double_t double_t . -> . void_t))
(define-cairo cairo_set_line_width (cairo_t* double_t . -> . void_t))
(define-cairo cairo_stroke    (cairo_t* . -> . void_t))
(define-cairo cairo_set_line_cap   (cairo_t* cairo_line_cap_t . -> . void_t))

;; (cairo_t* -> Void) -> Pict
;; Creates a fresh bitmap, calls f to draw into it, returns the result as a pict
(define (do-cairo f)
  (define bt (make-bitmap 256 256))
  (define bt-surface (send bt get-handle))
  (f (cairo_create (ffi2-cast (cpointer->ffi2-ptr bt-surface) #:to cairo_surface_t*)))
  (linewidth 2 (frame (bitmap bt))))
```

### Working with Cairo paths

Let's work with Cairo [path](https://www.cairographics.org/manual/cairo-Paths.html)
objects. A path is defined as:

```c
typedef struct {
    cairo_status_t status;
    cairo_path_data_t *data;
    int num_data;
} cairo_path_t;
```

And path data elements are a union:

```c
union _cairo_path_data_t {
    struct {
        cairo_path_data_type_t type;
        int length;
    } header;
    struct {
        double x, y;
    } point;
};
```

### Defining union types with ffi2

With the old FFI, you'd use `_union` and `_list-struct` to define this union. With
`ffi2`, you use `define-ffi2-type` with the `union` constructor, and nested structs
for the sub-cases:

```racket
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
```

This is cleaner than the old `_union`/`_list-struct` approach because:
- Each sub-case gets named field accessors (`path_header_t-type`, `path_point_t-x`, etc.)
- The union has named variants (`cairo_path_data_t-header`, `cairo_path_data_t-point`)
- No need for `union-ref` with numeric indices

### A simple path struct

For the path struct itself, we need a pointer to the data array and a length.
Let's start with a simple approach using raw pointer access:

```racket
(define-ffi2-type cairo_status_t int_t)

;; We define a simplified path type with a raw pointer for the data array
(define-ffi2-type simple_cairo_path_t
  (struct
    [status cairo_status_t]
    [data   void_t*]
    [num_data int_t]))
```

Now we can get a path from Cairo:

```racket
;; cairo_copy_path returns a pointer to a path struct
(define-cairo cairo_copy_path (cairo_t* . -> . void_t*))

(define a-path #f)

(do-cairo (lambda (ctx)
            (cairo_move_to ctx 50.0 50.0)
            (cairo_line_to ctx 206.0 206.0)
            (cairo_move_to ctx 50.0 206.0)
            (cairo_line_to ctx 115.0 115.0)
            (set! a-path (cairo_copy_path ctx))
            (cairo_stroke ctx)))
```

### Low-level pointer operations

With `ffi2`, you dereference pointers using `ffi2-ref` (analogous to the old
`ptr-ref`). But for reading struct fields, the cleanest approach is to cast the
pointer to the struct type and use the generated accessors:

```racket
(define simple-path (ffi2-cast a-path #:to simple_cairo_path_t*))
(define data-ptr    (simple_cairo_path_t-data simple-path))
(define num-data    (simple_cairo_path_t-num_data simple-path))
```

### Accessing union elements from the array

To access individual path data elements from the data array, we use `ffi2-ref`
with offsets:

```racket
;; Read the first element as a header
(define first-elem (ffi2-cast data-ptr #:to cairo_path_data_t*))
(define header (cairo_path_data_t-header first-elem))
(path_header_t-type header)    ; => 'move-to
(path_header_t-length header)  ; => 2

;; Read the second element as a point (offset by sizeof one union element)
(define second-elem
  (ffi2-cast (ffi2-add data-ptr (ffi2-sizeof cairo_path_data_t))
             #:to cairo_path_data_t*))
(define point (cairo_path_data_t-point second-elem))
(path_point_t-x point)  ; => 50.0
(path_point_t-y point)  ; => 50.0
```

The `ffi2-add` function does pointer arithmetic—it advances the pointer by a
given number of bytes. We use `ffi2-sizeof` to get the size of one union element.

### Building a custom C type

While the low-level operations work, it's much nicer to define a *custom C type*
that automatically converts a Cairo path pointer into a Racket-friendly
representation.

With the old FFI, you'd use `make-ctype` with two conversion functions. With `ffi2`,
you use `define-ffi2-type` with `#:racket->c` and `#:c->racket`:

First, let's define a Racket struct for our path representation. Note that
`ffi2` exports its own `struct` form (for defining C struct types), which
shadows Racket's built-in `struct`. To use both, rename one on import—for
example, `(only-in racket/base [struct rkt:struct])`:

```racket
(rkt:struct cairo-path (ptr)
  #:property prop:sequence
  (lambda (p) (in-cairo-path p)))
```

This struct stores a raw pointer and implements the sequence interface, so we can
iterate over path elements with `for`.

Now define the custom C type:

```racket
(define-ffi2-type cairo_path_t* void_t*
  #:predicate cairo-path?
  #:racket->c (lambda (rkt) (cairo-path-ptr rkt))
  #:c->racket (lambda (cobj) (cairo-path cobj)))
```

The base type is `void_t*` (a plain pointer). The conversion functions simply
wrap/unwrap our `cairo-path` struct.

### The sequence implementation

The real work is in `in-cairo-path`, which reads the C array and produces a
Racket sequence:

```racket
;; Cairo-Path -> Sequence
(define (in-cairo-path path)
  (define pp (cairo-path-ptr path))
  ;; Read the path struct fields
  (define path-struct (ffi2-cast pp #:to simple_cairo_path_t*))
  (define array-ptr   (simple_cairo_path_t-data path-struct))
  (define len         (simple_cairo_path_t-num_data path-struct))
  (define elem-size   (ffi2-sizeof cairo_path_data_t))

  (make-do-sequence
    (lambda ()
      (values
        ;; pos->element: extract one path command at a given position
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

        ;; next-pos: advance past this element's header + data
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
```

This code follows the same algorithm as the original tutorial:
1. Each position in the array starts with a header element containing the action
   type and length.
2. We read the header, then loop over the subsequent data elements to extract
   point coordinates.
3. We advance the position by the header's length to get to the next command.

### Using the custom type

Now we can rebind `cairo_copy_path` with our custom type:

```racket
(define-cairo cairo_copy_path_typed (cairo_t* . -> . cairo_path_t*)
  #:c-id cairo_copy_path)

(do-cairo (lambda (ctx)
            (cairo_move_to ctx 50.0 50.0)
            (cairo_line_to ctx 206.0 206.0)
            (cairo_move_to ctx 50.0 206.0)
            (cairo_line_to ctx 115.0 115.0)
            (define path (cairo_copy_path_typed ctx))
            ;; Using path as a sequence!
            (for ([elem path])
              (displayln elem))
            (cairo_stroke ctx)))
```

This prints something like:

```
(move-to (50.0 50.0))
(line-to (206.0 206.0))
(move-to (50.0 206.0))
(line-to (115.0 115.0))
```

Much more intuitive than opaque union values!

---

## Key Differences: ffi/unsafe vs. ffi2

Here's a quick reference table summarizing the changes:

| Concept | `ffi/unsafe` (old) | `ffi2` (new) |
|---|---|---|
| Module | `(require ffi/unsafe)` | `(require ffi2)` |
| Type naming | Leading `_` (`_int`) | Trailing `_t` (`int_t`) |
| Pointer type | `_pointer` | `void_t*` |
| Function type | `(_fun _int _int -> _int)` | `(int_t int_t . -> . int_t)` |
| String type | `_string` | `string_t` |
| Void type | `_void` | `void_t` |
| Library loading | `(ffi-lib ...)` | `(ffi2-lib ...)` |
| Function binding | `(get-ffi-obj ...)` | `(define-ffi2-procedure ...)` |
| Definer macro | `(define-ffi-definer ...)` | `(define-ffi2-definer ...)` |
| Tagged pointer | `(define-cpointer-type _foo)` | `(define-ffi2-type foo* void_t*)` |
| Struct definition | `(define-cstruct _foo ...)` | `(define-ffi2-type foo (struct ...))` |
| Struct constructor | `(make-foo ...)` | `(foo ...)` |
| Union definition | `(_union ...)` | `(define-ffi2-type foo (union ...))` |
| Custom type | `(make-ctype ...)` | `(define-ffi2-type ... #:racket->c ... #:c->racket ...)` |
| Memory allocation | `(malloc ...)` | `(ffi2-malloc ...)` |
| Memory free | `(free ...)` | `(ffi2-free ...)` |
| Pointer deref | `(ptr-ref ...)` | `(ffi2-ref ...)` |
| Pointer set | `(ptr-set! ...)` | `(ffi2-set! ...)` |
| Cast | `(cast ...)` | `(ffi2-cast ... #:from ... #:to ...)` |
| Sizeof | `(ctype-sizeof ...)` | `(ffi2-sizeof ...)` |
| Pointer arithmetic | manual | `(ffi2-add ptr offset)` |
| Array type | `(_array/list ...)` | `(define-ffi2-type name (array type size))` |
| Varargs | not straightforward | `(type1 type2 #:varargs vtype . -> . rtype)` |

### Notable improvements in ffi2

1. **Unified type definition**: `define-ffi2-type` replaces multiple forms
   (`define-cstruct`, `define-cpointer-type`, `_enum`, `make-ctype`, etc.).

2. **Struct constructors**: The type name itself is the constructor—`(point_t 1.0 2.0)`
   instead of `(make-point_t 1.0 2.0)`.

3. **Union field names**: Instead of `(union-ref val 0)` with numeric indices, you
   use named accessors like `(my_union-field_name ptr)`.

4. **Performance**: Foreign calls are approximately twice as fast as with `ffi/unsafe`.

5. **Varargs support**: First-class varargs in function types with `#:varargs`.

6. **Memory management**: Clearer allocation modes with keywords (`#:gcable`,
   `#:manual`, `#:gcable-immobile`, `#:gcable-traced`).

7. **Pointer arithmetic**: `ffi2-add` and `ffi2-cast` with `#:offset` make pointer
   manipulation more explicit and readable.

### Gotchas to watch for

1. **`struct` shadowing**: `ffi2` exports a `struct` form for defining C struct
   types, which shadows Racket's built-in `struct`. If you need both (e.g., to
   define a Racket struct with properties), rename one on import:
   `(only-in racket/base [struct rkt:struct])`.

2. **Old-style cpointers**: Values obtained from `racket/draw` or other existing
   Racket libraries are old-style `cpointer` objects, not `ffi2` pointers. You
   must convert them with `cpointer->ffi2-ptr` before passing them to any `ffi2`
   function—even one typed as `void_t*`.

3. **No built-in `_enum`**: Unlike `ffi/unsafe`, `ffi2` has no dedicated enum
   form. Instead, use `define-ffi2-type` over `int_t` with `#:predicate`,
   `#:racket->c`, and `#:c->racket` conversion functions.

---

*This tutorial was based on the original three-part series by Asumu Takikawa on the
[PRL blog](https://prl.khoury.northeastern.edu/blog/2016/06/27/tutorial-using-racket-s-ffi/),
updated for the `ffi2` library introduced in
[PR #5471](https://github.com/racket/racket/pull/5471).*
