#lang racket/base
(define (parse-count text)
  (define n (string->number text))
  (unless (exact-positive-integer? n)
    (raise-user-error 'pidigits "expected a positive integer digit count; received ~a" text))
  n)
;; The Computer Language Benchmarks Game
;; https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
;; Based on the Perl version; GMP interface adapted by Eli Barzilay.
;; Matched-work version: both candidate extractions on every transition.
;; Retains both extract operations, including initial/failing candidates.
;; GMP mutable integers are a library/representation change, not compiler magic.
(require racket/cmdline ffi/unsafe)
(define libgmp (ffi-lib "libgmp"))
(define-cstruct _mpz ([alloc _int] [size _int] [limbs _pointer]))
(define init (get-ffi-obj "__gmpz_init_set_ui" libgmp (_fun _mpz-pointer _ulong -> _void)))
(define clear (get-ffi-obj "__gmpz_clear" libgmp (_fun _mpz-pointer -> _void)))
(define mul-ui (get-ffi-obj "__gmpz_mul_ui" libgmp (_fun _mpz-pointer _mpz-pointer _ulong -> _void)))
(define add (get-ffi-obj "__gmpz_add" libgmp (_fun _mpz-pointer _mpz-pointer _mpz-pointer -> _void)))
(define submul-ui (get-ffi-obj "__gmpz_submul_ui" libgmp (_fun _mpz-pointer _mpz-pointer _ulong -> _void)))
(define tdiv-q (get-ffi-obj "__gmpz_tdiv_q" libgmp (_fun _mpz-pointer _mpz-pointer _mpz-pointer -> _void)))
(define get-ui (get-ffi-obj "__gmpz_get_ui" libgmp (_fun _mpz-pointer -> _ulong)))
(define (make-ui n) (define x (make-mpz 0 0 #f)) (init x n) x)
(define (digits n)
  (define q (make-ui 1)) (define r (make-ui 0)) (define t (make-ui 1))
  (define tmp (make-ui 0))
  (define (extract x)
    (mul-ui tmp q x) (add tmp tmp r) (tdiv-q tmp tmp t) (get-ui tmp))
  (dynamic-wind
    void
    (lambda ()
      (let digit ([k 1] [n n] [row 0] [col 0])
        (if (> n 0)
            (let ([y (extract 3)])
              (if (= y (extract 4))
                  (begin
                    (submul-ui r t y) (mul-ui r r 10) (mul-ui q q 10)
                    (if (= col 10)
                        (let ([row (+ row 10)])
                          (printf "\t:~a\n~a" row y)
                          (digit k (sub1 n) row 1))
                        (begin (printf "~a" y) (digit k (sub1 n) row (add1 col)))))
                  (let ([k2 (add1 (* 2 k))])
                    (add r r q) (add r r q) (mul-ui r r k2)
                    (mul-ui q q k) (mul-ui t t k2)
                    (digit (add1 k) n row col))))
            (printf "~a\t:~a\n" (make-string (- 10 col) #\space) (+ row col)))))
    (lambda () (for-each clear (list q r t tmp)))))
(digits (command-line #:args (n) (parse-count n)))
