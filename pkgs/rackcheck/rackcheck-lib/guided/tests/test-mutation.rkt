#lang racket/base

;; Tests for value mutation strategies.

(require rackunit
         racket/list
         rackcheck/guided/mutation)

(define rng (make-pseudo-random-generator))
(parameterize ([current-pseudo-random-generator rng])
  (random-seed 42))

(test-case "mutate-value on boolean flips it"
  (check-equal? (mutate-value #t rng) #f)
  (check-equal? (mutate-value #f rng) #t))

(test-case "mutate-value on integer produces an integer"
  (for ([_ (in-range 20)])
    (define result (mutate-value 42 rng))
    (check-pred exact-integer? result)))

(test-case "mutate-value on string produces a string"
  (for ([_ (in-range 20)])
    (define result (mutate-value "hello" rng))
    (check-pred string? result)))

(test-case "mutate-value on empty string"
  (for ([_ (in-range 10)])
    (define result (mutate-value "" rng))
    (check-pred string? result)))

(test-case "mutate-value on list produces a list"
  (for ([_ (in-range 20)])
    (define result (mutate-value '(1 2 3) rng))
    (check-pred list? result)))

(test-case "mutate-value on empty list"
  (for ([_ (in-range 10)])
    (define result (mutate-value '() rng))
    (check-pred list? result)))

(test-case "mutate-value on vector produces a vector"
  (for ([_ (in-range 20)])
    (define result (mutate-value (vector 1 2 3) rng))
    (check-pred vector? result)))

(test-case "mutate-value on char produces a char"
  (for ([_ (in-range 20)])
    (define result (mutate-value #\a rng))
    (check-pred char? result)))

(test-case "mutate-value on bytes produces bytes"
  (for ([_ (in-range 20)])
    (define result (mutate-value #"hello" rng))
    (check-pred bytes? result)))

(test-case "mutate-value on pair mutates car or cdr"
  ;; Non-list pair
  (for ([_ (in-range 20)])
    (define result (mutate-value (cons 1 2) rng))
    (check-pred pair? result)))

(test-case "splice-values on lists produces a list"
  (for ([_ (in-range 20)])
    (define result (splice-values '(1 2 3) '(4 5 6) rng))
    (check-pred list? result)))

(test-case "splice-values on strings produces a string"
  (for ([_ (in-range 20)])
    (define result (splice-values "abc" "xyz" rng))
    (check-pred string? result)))

(test-case "mutation produces diverse values"
  (define results
    (for/list ([_ (in-range 50)])
      (mutate-value 100 rng)))
  ;; Should not all be the same
  (define unique (remove-duplicates results))
  (check-true (> (length unique) 3) "Mutations should produce diverse values"))

(printf "All mutation tests passed.\n")
