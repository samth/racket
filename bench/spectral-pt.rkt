#lang racket/base
;; spectralnorm-par.rkt with parallel threads in place of futures, C = 4
(require racket/future
         racket/fixnum
         racket/flonum)
(#%declare #:unsafe)

(define C 4)
(define pool (make-parallel-thread-pool C))

(define-syntax-rule (for/par k ([i N]) b)
  (let ([stride (fxquotient N k)])
    (define ths
      (for/list ([n k])
        (thread #:pool pool
                #:keep 'results
                (λ ()
                  (for ([i (in-range (fx* n stride) (fxmin N (fx* (fx+ n 1) stride)))])
                    b)))))
    (for-each thread-wait ths)))

(define N (string->number (or (getenv "N") "5500")))

(define (A i j)
  (let ([ij (fx+ i j)])
    (fl/ 1.0 (fl+ (fl* (fl* (fx->fl ij) (fx->fl (fx+ ij 1))) 0.5) (fx->fl (fx+ i 1))))))
(define (Av x y N)
  (for/par C
           ([i N])
           (flvector-set! y
                          i
                          (let L ([a 0.0]
                                  [j 0])
                            (if (fx= j N)
                                a
                                (L (fl+ a (fl* (flvector-ref x j) (A i j))) (fx+ j 1)))))))
(define (Atv x y N)
  (for/par C
           ([i N])
           (flvector-set! y
                          i
                          (let L ([a 0.0]
                                  [j 0])
                            (if (fx= j N)
                                a
                                (L (fl+ a (fl* (flvector-ref x j) (A j i))) (fx+ j 1)))))))
(define (AtAv x y t N)
  (Av x t N)
  (Atv t y N))
(define u (make-flvector N 1.0))
(define v (make-flvector N))
(define t (make-flvector N))
(define t0 (current-inexact-monotonic-milliseconds))
(for ([i (in-range 10)])
  (AtAv u v t N)
  (AtAv v u t N))
(define t1 (current-inexact-monotonic-milliseconds))
(printf "~a ms ~a\n"
        (round (- t1 t0))
        (real->decimal-string
         (flsqrt (let L ([vBv 0.0]
                         [vv 0.0]
                         [i 0])
                   (if (fx= i N)
                       (fl/ vBv vv)
                       (let ([ui (flvector-ref u i)]
                             [vi (flvector-ref v i)])
                         (L (fl+ vBv (fl* ui vi)) (fl+ vv (fl* vi vi)) (fx+ i 1))))))
         9))
