#lang racket/base
;; Parallel-thread analysis control; requires Racket CS 8.18.0.2 or newer.
;; Not an additional submission candidate. See ../README.md.
(define (parse-count text)
  (define n (string->number text))
  (unless (and (exact-integer? n) (<= 3 n 12))
    (raise-user-error 'fannkuch-redux "expected an integer permutation size from 3 through 12; received ~a" text))
  n)

;;; The Computer Language Benchmarks Game
;;; https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

;; Written by Dima Dorfman, 2004
;; Slightly improved by Sven Hartrumpf, 2005-2006
;; Ever-so-slightly tweaked for MzScheme by Brent Fulgham
;; PLT-ized for v4.0 by Matthew
;; Updated by Danny Yoo and Matthias Felleisen
;; Optimized and Parallelized by Gustavo Massaccesi, 2013

(require (for-syntax (only-in racket/base 
                              lambda 
                              syntax 
                              syntax-case
                              make-rename-transformer
                              #%app)))
(require racket/unsafe/ops racket/fixnum racket/performance-hint
         racket/future)
(require racket/cmdline)

(define-sequence-syntax unsafe-in-fxrange 
  (lambda () #'in-fxrange/proc) 
  (lambda (stx) 
    (syntax-case stx () 
      [[(d) (_ nat)] 
       #'[(d) 
          (:do-in ([(n) nat])
                  #f 
                  ([i 0])
                  (unsafe-fx< i n)
                  ([(d) i])
                  #t
                  #t
                  [(unsafe-fx+ 1 i)])]]))) 

(define (unsafe-in-fxrange/proc n) 
  (make-do-sequence (lambda () (values (lambda (x) x)
                                       (lambda (x) (unsafe-fx+ 1 x))
                                       0
                                       (lambda (x) (unsafe-fx< x n))
                                       #f
                                       #f)))) 


(define-syntax-rule (define/0st-bool (name arg0 rest ...) body ...)
  (begin
    (define-syntax-rule (name arg0/v rest ...)
      (if arg0/v (name/t rest ...) (name/f rest ...)))
    (define (name/t rest ...) (let ([arg0 #t]) body ...))
    (define (name/f rest ...) (let ([arg0 #f]) body ...))
    ))

(define (fannkuch n)
  (define workers (min 4 n (processor-count)))
  (define pool (make-parallel-thread-pool workers))
  (define n-1 (unsafe-fx- n 1))
  (define block-size (unsafe-fx- n 2))
  (define slices
    (for/list ([worker (in-range workers)])
      (thread
       #:pool pool #:keep 'results
       (lambda ()
         ;; Split each outer rotation into n-1 complete prefix rotations.
         ;; n*(n-1) blocks of (n-2)! permutations still visit every permutation.
         ;; Persistent workers reuse scratch space and interleave the blocks.
         (define pi (make-fxvector n))
         (define tmp (make-fxvector n))
         (define count (make-fxvector n-1))
         (for/fold ([flips 0] [checksum 0])
                   ([block (in-range worker (* n n-1) workers)])
           (define k (quotient block n-1))
           (define m (remainder block n-1))
           (for ([i (unsafe-in-fxrange n)])
             (define j (if (unsafe-fx= i n-1) i
                           (unsafe-fxmodulo (unsafe-fx+ i m) n-1)))
             (unsafe-fxvector-set! pi i (unsafe-fxmodulo (unsafe-fx+ j k) n)))
           (define-values (flips2 checksum2)
             (fannkuch/slice n block-size (or (> n 3) (even? m)) pi tmp count))
           (values (unsafe-fxmax flips flips2) (unsafe-fx+ checksum checksum2)))))))
  (parallel-thread-pool-close pool)
  (for/fold ([flips 0] [checksum 0]) ([worker (in-list slices)])
    (define-values (flips2 checksum2)
      (thread-wait worker
                   (lambda () (error 'fannkuch-redux "parallel worker failed"))))
    (values (unsafe-fxmax flips flips2) (unsafe-fx+ checksum checksum2))))

(define (fannkuch/slice n block-size even-parity? pi tmp count)
  (define/0st-bool (loop even-parity? flips r checksum limit pi tmp count)
    (for ([i (unsafe-in-fxrange r)])
      (unsafe-fxvector-set! count i (unsafe-fx+ 1 i)))
    (let* ([next-flips (count-flips pi tmp n)]
           [flips2 (unsafe-fxmax next-flips flips)]
           [next-checksum (if even-parity? 
                              (unsafe-fx+ checksum  next-flips)
                              (unsafe-fx- checksum next-flips))])
      (let loop2 ([r 1])
        (if (unsafe-fx= r limit)
            (values flips2 next-checksum)
            (let ([perm0 (unsafe-fxvector-ref pi 0)])
              (for ([i (unsafe-in-fxrange r)])
                (unsafe-fxvector-set! pi i (unsafe-fxvector-ref pi (unsafe-fx+ 1 i))))
              (unsafe-fxvector-set! pi r perm0)
              (unsafe-fxvector-set! count r (unsafe-fx- (unsafe-fxvector-ref count r) 1))
              (if (unsafe-fx= (unsafe-fxvector-ref count r) 0)
                  (loop2 (unsafe-fx+ 1 r))
                  (loop (not even-parity?)
                        flips2
                        r
                        next-checksum
                        limit
                        pi
                        tmp
                        count)))))))
  (loop even-parity? 0 block-size 0 block-size pi tmp count))


(define-inline (count-flips pi rho n)
  (vector-copy-all! rho pi n)
  (let loop ([k 0])
    (if (unsafe-fx= (unsafe-fxvector-ref rho 0) 0)
        k
        (let loop2 ([i 0]
                    [j (unsafe-fxvector-ref rho 0)])
          (if (unsafe-fx> j i)
              (begin 
                (vector-swap! rho i j)
                (loop2 (unsafe-fx+ 1 i) (unsafe-fx- j 1)))
              (loop (unsafe-fx+ 1 k)))))))

(define-inline (vector-copy-all! dest src n) 
 (for ([i (unsafe-in-fxrange n)])
   (unsafe-fxvector-set! dest i (unsafe-fxvector-ref src i))))

(define-syntax-rule (vector-swap! v i j)
  (let ([t (unsafe-fxvector-ref v i)])
    (unsafe-fxvector-set! v i (unsafe-fxvector-ref v j))
    (unsafe-fxvector-set! v j t)))

; assume that n>=3
(command-line #:args (n)
              (define-values (answer checksum)
                (fannkuch (parse-count n)))
              (printf "~a\nPfannkuchen(~a) = ~a\n" 
                      checksum
                      n 
                      answer))
