#lang racket/base

;; Tests for the shrinking module.

(require rackunit
         coverage-guided-testing/private/shrinking)

(test-case "shrink integer toward 0"
  (define result
    (shrink-failing-input
     100
     (lambda (x) (> x 10))
     50))
  (check-true (<= result 15) "Should shrink toward boundary")
  (check-true (> result 10) "Should still satisfy predicate"))

(test-case "shrink negative integer toward 0"
  (define result
    (shrink-failing-input
     -100
     (lambda (x) (< x -10))
     50))
  (check-true (>= result -15))
  (check-true (< result -10)))

(test-case "shrink list by removing elements"
  (define result
    (shrink-failing-input
     '(1 2 3 4 5 6 7 8 9 10)
     (lambda (xs) (and (list? xs) (> (length xs) 2)))
     50))
  (check-true (list? result))
  (check-true (<= (length result) 5)))

(test-case "shrink string by removing characters"
  (define result
    (shrink-failing-input
     "hello world"
     (lambda (s) (and (string? s) (> (string-length s) 2)))
     50))
  (check-true (string? result))
  (check-true (<= (string-length result) 5)))

(test-case "shrink preserves failure"
  (define result
    (shrink-failing-input
     '(5 10 15)
     (lambda (xs)
       (and (list? xs)
            (not (null? xs))
            (> (car xs) 0)))
     50))
  (check-true (list? result))
  (check-true (not (null? result)))
  (check-true (> (car result) 0)))

(test-case "shrink list of arguments"
  ;; Simulating a multi-argument property
  (define result
    (shrink-failing-input
     '(500 -500)
     (lambda (args)
       (and (list? args)
            (= (length args) 2)
            (> (car args) 0)
            (< (cadr args) 0)))
     100))
  (check-true (list? result))
  (check-equal? (length result) 2)
  (check-true (> (car result) 0))
  (check-true (< (cadr result) 0))
  ;; Should be significantly smaller than the original
  (check-true (< (car result) 500)))

(printf "All shrinking tests passed.\n")
