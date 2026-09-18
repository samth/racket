#lang racket/base
(define (parse-count text)
  (define n (string->number text))
  (unless (and (exact-positive-integer? n) (<= n (quotient (most-positive-fixnum) 2)))
    (raise-user-error 'spectralnorm "expected a positive matrix size at most half the maximum fixnum; received ~a" text))
  n)
;; The Computer Language Benchmarks Game
;; https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
;; Translated from Mike Pall's Lua version.
;; Parallel version originally by Sam Tobin-Hochstadt.
;; Direct matrix-element formulation: all four required procedures participate.
(require racket/cmdline racket/fixnum racket/flonum racket/future)
(#%declare #:unsafe)
(define N (command-line #:args (n) (parse-count n)))
;; This bound covers the original floating product and exact integer formula.
(define integer-denominator?
  (<= (* 4 N N) (min (most-positive-fixnum) 9007199254740992)))
(define-syntax-rule (for/rows ([i size]) body)
  (let* ([n size] [count (processor-count)])
    (define futures
      (for/list ([part (in-range count)])
        ;; Exact arithmetic outside futures: complete disjoint row intervals.
        (define start (quotient (* part n) count))
        (define end (quotient (* (add1 part) n) count))
        (future (lambda () (for ([i (in-range start end)]) body)))))
    (for-each touch futures)))
(define (A i j)
  (define ij (fx+ i j))
  (if integer-denominator?
      (fl/ 1.0 (fx->fl (fx+ (fxrshift (fx* ij (fx+ ij 1)) 1) (fx+ i 1))))
      (fl/ 1.0 (fl+ (fl* (fl* (fx->fl ij) (fx->fl (fx+ ij 1))) 0.5)
                    (fx->fl (fx+ i 1))))))
(define (Av x y n)
  (for/rows ([i n])
    (flvector-set! y i
      (let loop ([j 0] [sum 0.0])
        (if (fx= j n) sum
            (loop (fx+ j 1) (fl+ sum (fl* (flvector-ref x j) (A i j)))))))))
(define (Atv x y n)
  (for/rows ([i n])
    (flvector-set! y i
      (let loop ([j 0] [sum 0.0])
        (if (fx= j n) sum
            (loop (fx+ j 1) (fl+ sum (fl* (flvector-ref x j) (A j i)))))))))
(define (AtAv x y temporary n) (Av x temporary n) (Atv temporary y n))
(define u (make-flvector N 1.0))
(define v (make-flvector N 0.0))
(define temporary (make-flvector N 0.0))
(for ([iteration (in-range 10)]) (AtAv u v temporary N) (AtAv v u temporary N))
(displayln
 (real->decimal-string
  (flsqrt
   (let loop ([i 0] [uv 0.0] [vv 0.0])
     (if (fx= i N) (fl/ uv vv)
         (let ([ui (flvector-ref u i)] [vi (flvector-ref v i)])
           (loop (fx+ i 1) (fl+ uv (fl* ui vi)) (fl+ vv (fl* vi vi))))))) 9))
