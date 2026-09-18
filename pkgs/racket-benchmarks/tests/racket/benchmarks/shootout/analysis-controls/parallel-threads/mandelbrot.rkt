#lang racket/base
;; Parallel-thread analysis control; requires Racket CS 8.18.0.2 or newer.
;; Not an additional submission candidate. See ../README.md.

;; The Computer Language Benchmarks Game
;; https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
;; contributed by Eli Barzilay
;; parallelized by Sam Tobin-Hochstadt

(require racket/require (for-syntax racket/base) racket/future
         (filtered-in (lambda (n) (regexp-replace #rx"unsafe-" n ""))
                       racket/unsafe/ops)
         (only-in racket/flonum make-flvector)
         racket/cmdline)

(define LIMIT-SQR 4.0)
(define ITERATIONS 50)
(define N (command-line #:args (n) (string->number n)))
(unless (and (exact-positive-integer? N) (fixnum? (* N N)) (fixnum? (* 2 N)))
  (raise-argument-error 'mandelbrot "positive fixnum-sized bitmap dimension" N))
(define N.0 (fx->fl N))
(define 2/N (fl/ 2.0 N.0))
(define Crs
  (let ([v (make-flvector N)])
    (for ([x (in-range N)])
      (flvector-set! v x (fl- (fl/ (fx->fl (fx* 2 x)) N.0) 1.5)))
    v))

(define bpr (ceiling (/ N 8)))
(define bitmap (make-bytes (* N bpr)))

(define-syntax (let-n s)
  (syntax-case s ()
    [(_ N bs E)
     (for/fold ([E #'E]) ([_ (syntax-e #'N)]) #`(let bs #,E))]))

(define-syntax-rule (M Cr Ci)
  (let loop ([i 0] [Zr 0.0] [Zi 0.0])
    (let ([Zr2 (fl* Zr Zr)] [Zi2 (fl* Zi Zi)])
      (cond [(fl> (fl+ Zr2 Zi2) LIMIT-SQR) 0]
            [(fx= i ITERATIONS) 1]
            [else (loop (fx+ i 1)
                        (fl+ (fl- Zr2 Zi2) Cr)
                        (fl+ (fl* 2.0 (fl* Zr Zi)) Ci))]))))

;; Keep the pixel kernel outside the worker's row loop. This avoids carrying
;; row-scheduler state through the floating recurrence's register allocation.
(define (render-row y)
  (define Ci (fl- (fl* 2/N (fx->fl y)) 1.0))
  (let loop-x ([x 0] [bitnum 0] [byteacc 0]
               [aindex (fx* bpr (fx- N y))])
    (cond [(fx< x N)
           (define Cr (flvector-ref Crs x))
           (define byteacc* (fx+ (fxlshift byteacc 1) (M Cr Ci)))
           (cond [(fx= bitnum 7)
                  (bytes-set! bitmap aindex byteacc*)
                  (loop-x (fx+ x 1) 0 0 (fx+ aindex 1))]
                 [else (loop-x (fx+ x 1) (fx+ bitnum 1) byteacc* aindex)])]
          [else
           (when (fx> bitnum 0)
             (bytes-set! bitmap aindex
                         (fxlshift byteacc (fx- 8 (fxand N #x7)))))])))

(define workers (min 4 N (processor-count)))
(define pool (make-parallel-thread-pool workers))
(define threads
  (for/list ([worker (in-range workers)])
    (thread
     #:pool pool #:keep 'results
     (lambda ()
       ;; Cyclic rows balance the expensive middle rows; each output byte
       ;; belongs to exactly one worker. Coordinates and recurrence unchanged.
       (for ([y (in-range (- N worker) 0 (- workers))])
         (render-row y))))))
(parallel-thread-pool-close pool)
(for ([worker (in-list threads)])
  (thread-wait worker (lambda () (error 'mandelbrot "parallel worker failed"))))
(printf "P4\n~a ~a\n" N N)
(void (write-bytes bitmap))
