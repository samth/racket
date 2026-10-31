#lang racket/base

;; A "CAS hash table" is a box containing an immutable hash table.
;; Reads are plain immutable-hash lookups, so they take no lock; writes
;; replace the table with `box-cas!`. Use this for tables that are
;; read far more often than they're changed.

(provide make-cas-hasheq
         make-cas-hasheqv
         cas-hash-ref
         cas-hash-set!)

(define (make-cas-hasheq) (box #hasheq()))
(define (make-cas-hasheqv) (box #hasheqv()))

(define-syntax-rule (cas-hash-ref b k default)
  (hash-ref (unbox* b) k default))

(define (cas-hash-set! b k v)
  (define ht (unbox* b))
  (unless (box-cas! b ht (hash-set ht k v))
    (cas-hash-set! b k v)))
