#lang racket/base
;; One round of 4 pool threads, then sleep 2 s; task-clock beyond the
;; round shows CPU spent by idle workers
(require racket/fixnum
         racket/flonum)
(define pool (make-parallel-thread-pool 4))
(define (work n)
  (let L ([i 0]
          [a 0.0])
    (if (fx= i n)
        a
        (L (fx+ i 1) (fl+ a 1.0)))))
(define ths
  (for/list ([k 4])
    (thread #:pool pool #:keep 'results (lambda () (work 1000000)))))
(for-each thread-wait ths)
(define t0 (current-inexact-monotonic-milliseconds))
(define c0 (current-process-milliseconds))
(sleep 2)
(printf "round done; during 2 s sleep: process cpu ~a ms over ~a ms\n"
        (- (current-process-milliseconds) c0)
        (round (- (current-inexact-monotonic-milliseconds) t0)))
