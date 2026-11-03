#lang racket/base
;; k-nucleotide style: W parallel threads each count keys into a private
;; hasheqv. Prints the time.
(require racket/fixnum)
(define W (string->number (or (getenv "W") "4")))
(define N 5000000)
(define t0 (current-inexact-monotonic-milliseconds))
(define ths
  (for/list ([w W])
    (thread #:pool 'own
            (lambda ()
              (define counts (make-hasheqv))
              (let loop ([i 0]
                         [x w])
                (when (fx< i N)
                  (let ([k (fxand x 4095)]) (hash-set! counts k (fx+ 1 (hash-ref counts k 0))))
                  (loop (fx+ i 1) (fxand (fx+ (fx* x 1103515245) 12345) #xFFFFFFF))))
              (unless (= N (for/sum ([v (in-hash-values counts)]) v))
                (error "bad total"))))))
(for-each thread-wait ths)
(printf "private ~a ms\n" (round (- (current-inexact-monotonic-milliseconds) t0)))
