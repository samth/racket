#lang racket/base

;; A branchy function with many paths that are hard to reach by random testing.
;; This is used to demonstrate that coverage-guided testing explores more branches.

(provide branchy classify-number nested-branches magic-sequence)

;; Simple branchy classification with rare edge cases.
(define (branchy x)
  (cond
    [(< x -1000) 'very-negative]
    [(< x -100) 'negative]
    [(< x -10) 'slightly-negative]
    [(< x 0) 'small-negative]
    [(= x 0) 'zero]
    [(< x 10) 'small-positive]
    [(< x 100) 'positive]
    [(< x 1000) 'large]
    [(= x 1337) 'magic]       ; very hard to find randomly
    [(= x 42) 'answer]        ; also rare in large ranges
    [else 'very-large]))

;; More complex classification with string input.
(define (classify-number s)
  (cond
    [(not (string? s)) 'not-string]
    [(= (string-length s) 0) 'empty]
    [(char=? (string-ref s 0) #\-) 'negative-prefix]
    [(char=? (string-ref s 0) #\+) 'positive-prefix]
    [(char=? (string-ref s 0) #\0) 'zero-prefix]
    [(and (>= (string-length s) 3)
          (string=? (substring s 0 3) "inf"))
     'infinity]
    [(and (>= (string-length s) 3)
          (string=? (substring s 0 3) "nan"))
     'not-a-number]
    [(for/and ([c (in-string s)]) (char-numeric? c))
     'digits-only]
    [(for/and ([c (in-string s)]) (char-alphabetic? c))
     'letters-only]
    [else 'mixed]))

;; Deeply nested branches requiring specific combinations.
(define (nested-branches a b)
  (cond
    [(and (> a 0) (> b 0))
     (cond
       [(and (> a 10) (> b 10))
        (cond
          [(and (> a 100) (> b 100)) 'both-large]
          [(> a 50) 'a-medium-b-small]
          [else 'both-small-positive])]
       [(> a 5) 'a-bigger]
       [else 'b-bigger-or-equal])]
    [(and (< a 0) (< b 0))
     (cond
       [(and (< a -100) (< b -100)) 'both-very-negative]
       [else 'both-negative])]
    [(= a b) 'equal]
    [else 'mixed-signs]))

;; Function where a specific sequence of list elements triggers a bug.
(define (magic-sequence lst)
  (cond
    [(null? lst) 'empty]
    [(< (length lst) 3) 'too-short]
    [(and (= (first lst) 1)
          (= (second lst) 2)
          (= (third lst) 3))
     'magic-found]  ; hard to find: specific 3-element prefix
    [(> (length lst) 10) 'long]
    [(for/and ([x (in-list lst)]) (positive? x)) 'all-positive]
    [(for/and ([x (in-list lst)]) (negative? x)) 'all-negative]
    [else 'mixed]))

(define (first lst) (car lst))
(define (second lst) (cadr lst))
(define (third lst) (caddr lst))
