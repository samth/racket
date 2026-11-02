#lang racket/base
(require racket/private/place-local
         racket/fixnum
         "../common/set.rkt"
         "../common/scope-set.rkt")

(provide cache-place-init!

         clear-resolve-cache!
         resolve-cache-get
         resolve-cache-set!

         cache-or-reuse-set)

;; ----------------------------------------

;; Cache bindings resolutions with a fairly weak cache keyed on a
;; symbol, phase, and scope sets. The cache is a direct-mapped vector
;; indexed by the symbol's hash code, so it needs no lock:
;;  - A slot holds #f or an immutable `entry`, and a slot write is a
;;    single-word store, so a reader sees either the old or the new
;;    entry, never a mixture. On weakly ordered machines (such as
;;    Arm), Chez Scheme puts a store-store fence before every pointer
;;    store, so a reader cannot see an entry before its fields; the
;;    reader's loads of the fields depend on the loaded pointer, which
;;    orders them on every machine Racket CS supports.
;;  - A reader checks `sym` in the entry, so a collision is a miss.
;;  - Racing writers can lose an entry, which is just a miss later.
;;  - A write racing with `clear-resolve-cache!` can install a stale
;;    entry; that same race exists with a locked table, since the
;;    resolver computes a binding before installing it.
;; The vector is held weakly, so the cache is dropped on a GC.

(define RESOLVE-CACHE-SIZE 4096) ; power of 2

(define (make-cache)
  (box (make-weak-box #f)))

(define-place-local cache (make-cache))

(define (resolve-cache-place-init!)
  (set! cache (make-cache)))

(define-syntax-rule (cache-index sym)
  (fxand (eq-hash-code sym) (fx- RESOLVE-CACHE-SIZE 1)))

(define clear-resolve-cache!
  (case-lambda
    [(sym)
     (define vec (weak-box-value (unbox* cache)))
     (when vec
       (define i (cache-index sym))
       (define e (vector*-ref vec i))
       (when (and e (eq? sym (entry-sym e)))
         (vector*-set! vec i #f)))]
    [()
     (set-box*! cache (make-weak-box #f))]))

(struct entry (sym scs smss phase binding)
  #:authentic)

(define (resolve-cache-get sym phase scs smss)
  (define vec (weak-box-value (unbox* cache)))
  (and vec
       (let ([v (vector*-ref vec (cache-index sym))])
         (and v
              (eq? sym (entry-sym v))
              (eqv? phase (entry-phase v))
              (scope-set=? scs (entry-scs v))
              (set=? smss (entry-smss v))
              (entry-binding v)))))

(define (resolve-cache-set! sym phase scs smss b)
  (define wb (unbox* cache))
  (define vec (weak-box-value wb))
  (cond
   [(not vec)
    (box-cas! cache wb (make-weak-box (make-vector RESOLVE-CACHE-SIZE #f)))
    (resolve-cache-set! sym phase scs smss b)]
   [else
    (vector*-set! vec (cache-index sym) (entry sym scs smss phase b))]))

;; ----------------------------------------

;; For scope sets, we don't intern, but we approximate interning by
;; checking against a small set of recently allocated scope sets.
;; That's good enough to find sharing for a deeply nested sequence of
;; `let`s from a many-argument `or`, for example, where the
;; interleaving of original an macro-introduced syntax prevents the
;; usual child-is-same-as-parent sharing detecting from working well
;; enough. (Without this cache, peak memory use for a 1500-argument
;; `or` is 30% higher.)

(define NUM-CACHE-SLOTS 8) ; power of 2

(define (make-cached-sets)
  (make-weak-box (make-vector NUM-CACHE-SLOTS #f)))

(define-place-local cached-sets (make-cached-sets))
(define-place-local cached-sets-pos 0)

(define (sets-place-init!)
  (set! cached-sets (make-cached-sets)))

(define (cache-or-reuse-set s)
  (define vec (or (weak-box-value cached-sets)
                  (let ([vec (make-vector NUM-CACHE-SLOTS #f)])
                    (set! cached-sets (make-weak-box vec))
                    vec)))
  ;; `scope-set=?` rejects almost every non-equal set by its hash code
  (or (for/or ([s2 (in-vector vec)])
        (and s2
             (scope-set=? s s2)
             s2))
      (begin
        (vector*-set! vec cached-sets-pos s)
        (set! cached-sets-pos (fxand (fx+ 1 cached-sets-pos) (fx- NUM-CACHE-SLOTS 1)))
        s)))

;; ----------------------------------------

(define (cache-place-init!)
  (resolve-cache-place-init!)
  (sets-place-init!))
