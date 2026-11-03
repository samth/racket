#lang racket/base
;; Publication test for lock-free readers: one writer keeps inserting,
;; updating, and deleting keys with freshly allocated values (pairs and
;; 4-slot vectors), while 3 parallel readers check that every value they
;; find is fully initialized. Runs for SECS seconds.
(require racket/fixnum)
(define SECS (string->number (or (getenv "SECS") "3")))
(define ht (make-hasheq))
(define done? #f)
(define (well-formed? v)
  (cond
    [(pair? v) (eqv? (car v) (cdr v))]
    [(vector? v)
     (and (fx= 4 (vector-length v))
          (let ([x (vector-ref v 0)])
            (and (fixnum? x)
                 (eqv? x (vector-ref v 1))
                 (eqv? x (vector-ref v 2))
                 (eqv? x (vector-ref v 3)))))]
    [else #f]))
(define readers
  (for/list ([r 3])
    (thread #:pool 'own
            #:keep 'results
            (lambda ()
              (let loop ([i r]
                         [n 0])
                (cond
                  [done? n]
                  [else
                   (let ([v (hash-ref ht (fxand i 4095) #f)])
                     (when (and v (not (well-formed? v)))
                       (error "malformed value" v)))
                   (loop (fx+ i 7) (fx+ n 1))]))))))
(define end (+ (current-inexact-monotonic-milliseconds) (* 1000 SECS)))
(define writes
  (let loop ([i 0])
    (cond
      [(and (fx= 0 (fxand i 4095)) (> (current-inexact-monotonic-milliseconds) end)) i]
      [else
       (let ([k (fxand (fx* i 13) 4095)])
         (if (fx= 0 (fxand i 1))
             (hash-set! ht k (cons i i))
             (hash-set! ht k (make-vector 4 i)))
         (when (fx= 0 (fxand i 63))
           (hash-remove! ht (fxand (fx* i 5) 4095))))
       (loop (fx+ i 1))])))
(set! done? #t)
(define reads (apply + (map thread-wait readers)))
(printf "ok ~a writes ~a reads\n" writes reads)
