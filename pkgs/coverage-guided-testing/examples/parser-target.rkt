#lang racket/base

;; A simple string parser with many branches.
;; Coverage-guided testing should be able to discover valid parse inputs
;; more effectively than pure random generation.

(provide parse-expr)

;; Parse a simple arithmetic expression string.
;; Returns a parse result or 'error.
(define (parse-expr s)
  (cond
    [(not (string? s)) 'error]
    [(= (string-length s) 0) 'empty]
    [else (parse-additive s 0)]))

(define (parse-additive s pos)
  (define-values (left new-pos) (parse-multiplicative s pos))
  (cond
    [(eq? left 'error) (values 'error new-pos)]
    [(>= new-pos (string-length s)) (values left new-pos)]
    [(char=? (string-ref s new-pos) #\+)
     (define-values (right final-pos) (parse-additive s (add1 new-pos)))
     (if (eq? right 'error)
         (values 'error final-pos)
         (values (list '+ left right) final-pos))]
    [(char=? (string-ref s new-pos) #\-)
     (define-values (right final-pos) (parse-additive s (add1 new-pos)))
     (if (eq? right 'error)
         (values 'error final-pos)
         (values (list '- left right) final-pos))]
    [else (values left new-pos)]))

(define (parse-multiplicative s pos)
  (define-values (left new-pos) (parse-primary s pos))
  (cond
    [(eq? left 'error) (values 'error new-pos)]
    [(>= new-pos (string-length s)) (values left new-pos)]
    [(char=? (string-ref s new-pos) #\*)
     (define-values (right final-pos) (parse-multiplicative s (add1 new-pos)))
     (if (eq? right 'error)
         (values 'error final-pos)
         (values (list '* left right) final-pos))]
    [(char=? (string-ref s new-pos) #\/)
     (define-values (right final-pos) (parse-multiplicative s (add1 new-pos)))
     (if (eq? right 'error)
         (values 'error final-pos)
         (values (list '/ left right) final-pos))]
    [else (values left new-pos)]))

(define (parse-primary s pos)
  (cond
    [(>= pos (string-length s)) (values 'error pos)]
    [(char=? (string-ref s pos) #\()
     (define-values (inner new-pos) (parse-additive s (add1 pos)))
     (cond
       [(eq? inner 'error) (values 'error new-pos)]
       [(>= new-pos (string-length s)) (values 'error new-pos)]
       [(char=? (string-ref s new-pos) #\))
        (values inner (add1 new-pos))]
       [else (values 'error new-pos)])]
    [(char-numeric? (string-ref s pos))
     (parse-number s pos)]
    [else (values 'error pos)]))

(define (parse-number s pos)
  (let loop ([i pos] [n 0])
    (cond
      [(>= i (string-length s))
       (values n i)]
      [(char-numeric? (string-ref s i))
       (loop (add1 i) (+ (* n 10) (- (char->integer (string-ref s i))
                                       (char->integer #\0))))]
      [else (values n i)])))
