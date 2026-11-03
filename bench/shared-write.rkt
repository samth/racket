#lang racket/base
;; Write-heavy sharing: W parallel threads each do 2M hash-set! on one
;; shared hasheq (1024 keys). Prints the time.
(require racket/fixnum)
(define W (string->number (or (getenv "W") "4")))
(define N 2000000)
(define ht (make-hasheq))
(for ([i 1024])
  (hash-set! ht i i))
(define t0 (current-inexact-monotonic-milliseconds))
(define ths
  (for/list ([w W])
    (thread #:pool 'own
            (lambda ()
              (let loop ([i 0])
                (when (fx< i N)
                  (hash-set! ht (fxand (fx+ (fx* i 7) w) 1023) i)
                  (loop (fx+ i 1))))))))
(for-each thread-wait ths)
(unless (= 1024 (hash-count ht))
  (error "bad count"))
(printf "writers ~a ms\n" (round (- (current-inexact-monotonic-milliseconds) t0)))
