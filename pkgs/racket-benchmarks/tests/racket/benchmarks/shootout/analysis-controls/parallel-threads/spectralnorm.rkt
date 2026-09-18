#lang racket/base
;; Parallel-thread analysis control; requires Racket CS 8.18.0.2 or newer.
;; Not an additional submission candidate. See ../README.md.
(define (parse-count text)
  (define n (string->number text))
  (unless (and (exact-positive-integer? n) (<= n (quotient (most-positive-fixnum) 2)))
    (raise-user-error 'spectralnorm "expected a positive matrix size at most half the maximum fixnum; received ~a" text))
  n)
;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/
;; Translated from Mike Pall's Lua version.
;; Parallelized by Sam Tobin-Hochstadt

(require racket/cmdline racket/future racket/fixnum racket/flonum)
(#%declare #:unsafe)

(define-syntax-rule (for/par k ([i N]) b)
  (let* ([size N] [count (min k size)])
    (define threads
      (for/list ([part (in-range count)])
        (define start (quotient (* part size) count))
        (define end (quotient (* (add1 part) size) count))
        (thread #:pool pool #:keep 'results
                (lambda () (for ([i (in-range start end)]) b)))))
    ;; Joining all writers orders their stores before the next matrix pass.
    (for ([worker (in-list threads)])
      (thread-wait worker
                   (lambda () (error 'spectralnorm "parallel worker failed"))))))

(define N (command-line #:args (n) (parse-count n)))
(define C (min 4 N (processor-count)))
(define pool (make-parallel-thread-pool C))

(define (A i j)
  (let ([ij (fx+ i j)])
    (fl/ 1.0 (fl+ (fl* (fl* (fx->fl ij)
                            (fx->fl (fx+ ij 1)))
                       0.5) 
                  (fx->fl (fx+ i 1))))))
;; 4*N*N bounds the original floating product and all recurrence
;; intermediates, including the update after the last matrix element.
(define (integer-denominator? N)
  (<= (* 4 N N) (min (most-positive-fixnum) 9007199254740992)))
(define (Av x y N)
  (if (integer-denominator? N)
      (for/par C ([i N])
        (flvector-set!
         y i
         ;; A(i,j)'s denominator increases by i+j+1.
         (let ([initial-d (fx+ (fxrshift (fx* i (fx+ i 1)) 1) (fx+ i 1))]
               [initial-delta (fx+ i 1)])
           (let L ([a 0.0] [j 0] [d initial-d] [delta initial-delta])
             (if (fx= j N) a
                 (L (fl+ a (fl* (flvector-ref x j) (fl/ 1.0 (fx->fl d))))
                    (fx+ j 1) (fx+ d delta) (fx+ delta 1)))))))
      (for/par C ([i N])
        (flvector-set!
         y i
         (let L ([a 0.0] [j 0])
           (if (fx= j N) a
               (L (fl+ a (fl* (flvector-ref x j) (A i j)))
                  (fx+ j 1))))))))
(define (Atv x y N)
  (if (integer-denominator? N)
      (for/par C ([i N])
        (flvector-set!
         y i
         ;; A(j,i)'s denominator increases by i+j+2.
         (let ([initial-d (fx+ (fxrshift (fx* i (fx+ i 1)) 1) 1)]
               [initial-delta (fx+ i 2)])
           (let L ([a 0.0] [j 0] [d initial-d] [delta initial-delta])
             (if (fx= j N) a
                 (L (fl+ a (fl* (flvector-ref x j) (fl/ 1.0 (fx->fl d))))
                    (fx+ j 1) (fx+ d delta) (fx+ delta 1)))))))
      (for/par C ([i N])
        (flvector-set!
         y i
         (let L ([a 0.0] [j 0])
           (if (fx= j N) a
               (L (fl+ a (fl* (flvector-ref x j) (A j i)))
                  (fx+ j 1))))))))
(define (AtAv x y t N) (Av x t N) (Atv t y N))
(define u (make-flvector N 1.0))
(define v (make-flvector N))
(define t (make-flvector N))
(for ([i (in-range 10)])
  (AtAv u v t N) (AtAv v u t N))
(parallel-thread-pool-close pool)
(displayln (real->decimal-string
            (flsqrt 
             (let L ([vBv 0.0] [vv 0.0] [i 0])
               (if (fx= i N) (fl/ vBv vv)
                   (let ([ui (flvector-ref u i)] [vi (flvector-ref v i)])
                     (L (fl+ vBv (fl* ui vi))
                        (fl+ vv (fl* vi vi))
                        (fx+ i 1))))))
            9))
