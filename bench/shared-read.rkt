#lang racket/base
;; R parallel readers do hash-ref on one shared hasheq; with WRITER=1 one
;; more parallel thread does a hash-set! about every 2000 iterations of
;; busy work until the readers finish. Prints the readers' time.
(require racket/fixnum)
(define R (string->number (or (getenv "R") "4")))
(define N 2000000)
(define ht (make-hasheq))
(for ([i 1024])
  (hash-set! ht i i))
(define done? #f)
(define writer
  (and (getenv "WRITER")
       (thread #:pool 'own
               (lambda ()
                 (let loop ([i 0]
                            [sets 0])
                   (cond
                     [done? sets]
                     [(fx= 0 (fxand i 2047))
                      (hash-set! ht (fxand (fxrshift i 11) 1023) i)
                      (loop (fx+ i 1) (fx+ sets 1))]
                     [else (loop (fx+ i 1) sets)]))))))
(define t0 (current-inexact-monotonic-milliseconds))
(define readers
  (for/list ([r R])
    (thread #:pool 'own
            (lambda ()
              (let loop ([i 0]
                         [found 0])
                (if (fx= i N)
                    (unless (fx= found N)
                      (error "missing key"))
                    (loop (fx+ i 1)
                          (if (hash-ref ht (fxand (fx* i 7) 1023) #f)
                              (fx+ found 1)
                              found))))))))
(for-each thread-wait readers)
(define t1 (current-inexact-monotonic-milliseconds))
(set! done? #t)
(when writer
  (thread-wait writer))
(printf "readers ~a ms\n" (round (- t1 t0)))
