#lang racket/base
(require "function.rkt")

;; Test for the bug fixed in PR #5462:
;; The `with-continuation-mark` case in `extract-expr-functions` had two bugs:
;; 1. Pattern said `with-continuation-marks` (plural) — never matched valid input
;; 2. Recursive call passed the same form back — would cause infinite recursion
;;    if the pattern ever matched
;;
;; Bug reproduction: passing `(with-continuation-marks ...)` (plural, which the
;; buggy pattern matches) causes infinite recursion because the recursive call
;; reconstructs the same `(with-continuation-marks ...)` form.

(define (test name expected actual)
  (unless (equal? expected actual)
    (error 'test "FAIL ~a: expected ~e, got ~e" name expected actual)))

;; Test 1: The primary bug reproduction test.
;; With the buggy code, `(with-continuation-marks a b c)` (PLURAL) matches the
;; misspelled pattern and the recursive call passes back the same form, causing
;; infinite recursion / stack overflow.
;; With the fix, this form doesn't match `with-continuation-mark` (singular)
;; and falls through to the catch-all harmlessly.
(let ()
  (define lambdas (make-hasheq))
  (define result
    (extract-functions #hasheq()
                       '(define x (with-continuation-marks a b c))
                       lambdas))
  (test "wcm-plural: should terminate without infinite recursion"
        0 (hash-count result)))

;; Test 2: with-continuation-mark (singular) with simple sub-expressions
(let ()
  (define lambdas (make-hasheq))
  (define result
    (extract-functions #hasheq()
                       '(define x (with-continuation-mark k v body))
                       lambdas))
  (test "wcm-simple: knowns should be empty"
        0 (hash-count result)))

;; Test 3: with-continuation-mark with a letrec containing a lambda in the body
;; The lambda bound by letrec should be extracted as a known function
(let ()
  (define lambdas (make-hasheq))
  (define result
    (extract-functions #hasheq()
                       '(define f (with-continuation-mark k v
                                    (letrec ([g (lambda (x) x)]) g)))
                       lambdas))
  (test "wcm-letrec: g should be a known function"
        #t (function? (hash-ref result 'g #f)))
  (test "wcm-letrec: only g should be in knowns"
        1 (hash-count result)))

;; Test 4: with-continuation-mark at top level (not inside define)
(let ()
  (define lambdas (make-hasheq))
  (define result
    (extract-functions #hasheq()
                       '(with-continuation-mark k v
                          (letrec ([g (lambda (x) x)]) g))
                       lambdas))
  (test "wcm-top-level: g should be a known function"
        #t (function? (hash-ref result 'g #f))))

;; Test 5: with-continuation-mark nested inside begin
(let ()
  (define lambdas (make-hasheq))
  (define result
    (extract-functions #hasheq()
                       '(begin
                          (define a (lambda (x) x))
                          (define b (with-continuation-mark k v
                                      (letrec ([g (lambda (y) y)]) g))))
                       lambdas))
  (test "wcm-in-begin: a should be a known function"
        #t (function? (hash-ref result 'a #f)))
  (test "wcm-in-begin: g should be a known function"
        #t (function? (hash-ref result 'g #f))))

(printf "All tests passed.\n")
