#lang racket/base

;; A tiny interpreter with many edge cases.
;; Good target for coverage-guided testing because valid programs are rare
;; in the space of random s-expressions.

(provide interpret)

;; Interpret a simple expression language.
;; Expressions: numbers, booleans, (+ e e), (- e e), (* e e),
;;              (if e e e), (= e e), (< e e), (and e e), (or e e), (not e)
;; Returns a value or 'error.
(define (interpret expr)
  (cond
    [(number? expr) expr]
    [(boolean? expr) expr]
    [(not (pair? expr)) 'error]
    [(null? expr) 'error]
    [else
     (define op (car expr))
     (define args (cdr expr))
     (cond
       ;; Arithmetic
       [(and (eq? op '+) (= (length args) 2))
        (define a (interpret (car args)))
        (define b (interpret (cadr args)))
        (if (and (number? a) (number? b))
            (+ a b)
            'error)]
       [(and (eq? op '-) (= (length args) 2))
        (define a (interpret (car args)))
        (define b (interpret (cadr args)))
        (if (and (number? a) (number? b))
            (- a b)
            'error)]
       [(and (eq? op '*) (= (length args) 2))
        (define a (interpret (car args)))
        (define b (interpret (cadr args)))
        (if (and (number? a) (number? b))
            (* a b)
            'error)]

       ;; Comparison
       [(and (eq? op '=) (= (length args) 2))
        (define a (interpret (car args)))
        (define b (interpret (cadr args)))
        (if (and (number? a) (number? b))
            (= a b)
            'error)]
       [(and (eq? op '<) (= (length args) 2))
        (define a (interpret (car args)))
        (define b (interpret (cadr args)))
        (if (and (number? a) (number? b))
            (< a b)
            'error)]

       ;; Boolean operations
       [(and (eq? op 'and) (= (length args) 2))
        (define a (interpret (car args)))
        (define b (interpret (cadr args)))
        (if (and (boolean? a) (boolean? b))
            (and a b)
            'error)]
       [(and (eq? op 'or) (= (length args) 2))
        (define a (interpret (car args)))
        (define b (interpret (cadr args)))
        (if (and (boolean? a) (boolean? b))
            (or a b)
            'error)]
       [(and (eq? op 'not) (= (length args) 1))
        (define a (interpret (car args)))
        (if (boolean? a) (not a) 'error)]

       ;; Conditional
       [(and (eq? op 'if) (= (length args) 3))
        (define cond-val (interpret (car args)))
        (cond
          [(eq? cond-val #t) (interpret (cadr args))]
          [(eq? cond-val #f) (interpret (caddr args))]
          [else 'error])]

       ;; Unknown operation
       [else 'error])]))
