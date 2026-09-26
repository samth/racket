#lang racket/base

;; For a hash table that's likely to be small, then a boxed immutable
;; hash table can be more efficient

(provide make-small-hasheq
         make-small-hasheqv
         small-hash-ref
         small-hash-set!
         small-hash-cas!
         small-hash-keys)

(define (make-small-hasheq)
  (box #hasheq()))

(define (make-small-hasheqv)
  (box #hasheqv()))

(define (small-hash-ref small-ht key default)
  (hash-ref (unbox small-ht) key default))

(define (small-hash-set! small-ht key val)
  (set-box! small-ht (hash-set (unbox small-ht) key val)))

;; Atomically changes the value for `key` to `new-val` if the value is
;; currently `old-val` (where a missing key's value is #f), and returns
;; whether it made the change
(define (small-hash-cas! small-ht key old-val new-val)
  (let loop ()
    (define ht (unbox small-ht))
    (and (eq? old-val (hash-ref ht key #f))
         (or (box-cas! small-ht ht (hash-set ht key new-val))
             (loop)))))

(define (small-hash-keys small-ht)
  (hash-keys (unbox small-ht)))
