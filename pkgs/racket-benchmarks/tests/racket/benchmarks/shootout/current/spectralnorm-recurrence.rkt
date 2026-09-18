#lang racket/base
(define (parse-count text)
  (define n (string->number text))
  (unless (and (exact-positive-integer? n) (<= n (quotient (most-positive-fixnum) 2)))
    (raise-user-error 'spectralnorm "expected a positive matrix size at most half the maximum fixnum; received ~a" text))
  n)
;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/
;; Translated from Mike Pall's Lua version.

(require racket/cmdline 
         racket/unsafe/ops racket/flonum
         (only-in racket/fixnum most-positive-fixnum))

(let* ([A (lambda (i j)
            (let ([ij (unsafe-fx+ i j)])
              (unsafe-fl/ 1.0 (unsafe-fl+ (unsafe-fl* (unsafe-fl* (unsafe-fx->fl ij)
                                                                  (unsafe-fx->fl (unsafe-fx+ ij 1)))
                                                      0.5) 
                                          (unsafe-fx->fl (unsafe-fx+ i 1))))))]
       ;; 4*N*N bounds the original floating product and all recurrence
       ;; intermediates, including the update after the last matrix element.
       [integer-denominator?
        (lambda (N)
          (<= (* 4 N N) (min (most-positive-fixnum) 9007199254740992)))]
       [Av
        (lambda (x y N)
          (if (integer-denominator? N)
              (for ([i (in-range N)])
                (unsafe-flvector-set!
                 y i
                 ;; A(i,j)'s denominator increases by i+j+1.
                 (let ([initial-d (unsafe-fx+ (unsafe-fxrshift (unsafe-fx* i (unsafe-fx+ i 1)) 1)
                                              (unsafe-fx+ i 1))]
                       [initial-delta (unsafe-fx+ i 1)])
                   (let L ([a 0.0] [j 0] [d initial-d] [delta initial-delta])
                     (if (unsafe-fx= j N) a
                         (L (unsafe-fl+ a (unsafe-fl* (unsafe-flvector-ref x j)
                                                     (unsafe-fl/ 1.0 (unsafe-fx->fl d))))
                            (unsafe-fx+ j 1) (unsafe-fx+ d delta) (unsafe-fx+ delta 1)))))))
              (for ([i (in-range N)])
                (unsafe-flvector-set!
                 y i
                 (let L ([a 0.0] [j 0])
                   (if (unsafe-fx= j N) a
                       (L (unsafe-fl+ a (unsafe-fl* (unsafe-flvector-ref x j) (A i j)))
                          (unsafe-fx+ j 1))))))))]
       [Atv
        (lambda (x y N)
          (if (integer-denominator? N)
              (for ([i (in-range N)])
                (unsafe-flvector-set!
                 y i
                 ;; A(j,i)'s denominator increases by i+j+2.
                 (let ([initial-d (unsafe-fx+ (unsafe-fxrshift (unsafe-fx* i (unsafe-fx+ i 1)) 1) 1)]
                       [initial-delta (unsafe-fx+ i 2)])
                   (let L ([a 0.0] [j 0] [d initial-d] [delta initial-delta])
                     (if (unsafe-fx= j N) a
                         (L (unsafe-fl+ a (unsafe-fl* (unsafe-flvector-ref x j)
                                                     (unsafe-fl/ 1.0 (unsafe-fx->fl d))))
                            (unsafe-fx+ j 1) (unsafe-fx+ d delta) (unsafe-fx+ delta 1)))))))
              (for ([i (in-range N)])
                (unsafe-flvector-set!
                 y i
                 (let L ([a 0.0] [j 0])
                   (if (unsafe-fx= j N) a
                       (L (unsafe-fl+ a (unsafe-fl* (unsafe-flvector-ref x j) (A j i)))
                          (unsafe-fx+ j 1))))))))]
       [AtAv (lambda (x y t N) (Av x t N) (Atv t y N))]
       [N (command-line #:args (n) (parse-count n))]
       [u (make-flvector N 1.0)]
       [v (make-flvector N)]
       [t (make-flvector N)])
  (for ([i (in-range 10)])
    (AtAv u v t N)
    (AtAv v u t N))
  (displayln (real->decimal-string 
              (unsafe-flsqrt 
               (let L ([vBv 0.0] [vv 0.0] [i 0])
                 (if (unsafe-fx= i N) (unsafe-fl/ vBv vv)
                     (let ([ui (unsafe-flvector-ref u i)] [vi (unsafe-flvector-ref v i)])
                       (L (unsafe-fl+ vBv (unsafe-fl* ui vi))
                          (unsafe-fl+ vv (unsafe-fl* vi vi))
                          (unsafe-fx+ i 1))))))
              9)))
