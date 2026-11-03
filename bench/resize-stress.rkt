#lang racket/base
;; Correctness stress for lock-free readers: a writer repeatedly grows a
;; hasheq to 20000 extra keys and deletes them again (forcing resizes up
;; and down) while 3 parallel readers check that the 64 stable keys are
;; always found with the right value, and that volatile keys, when found,
;; map to themselves.
(require racket/fixnum)
(define ROUNDS (string->number (or (getenv "ROUNDS") "60")))
(define ht (make-hasheq))
(for ([i 64])
  (hash-set! ht (fx- -1 i) i))
(define done? #f)
(define readers
  (for/list ([r 3])
    (thread #:pool 'own #:keep 'results
            (lambda ()
              (let loop ([i 0]
                         [n 0])
                (cond
                  [done? n]
                  [else
                   (let ([s (fxand i 63)])
                     (unless (eqv? s (hash-ref ht (fx- -1 s) #f))
                       (error "lost stable key" s)))
                   (let* ([k (fxand (fx* i 31) 32767)]
                          [v (hash-ref ht k #f)])
                     (unless (or (not v) (eqv? v k))
                       (error "wrong value" k v)))
                   (loop (fx+ i 1) (fx+ n 1))]))))))
(for ([r ROUNDS])
  (for ([k 20000])
    (hash-set! ht k k))
  (for ([k 20000])
    (hash-remove! ht k)))
(set! done? #t)
(define reads (apply + (map thread-wait readers)))
(unless (= (hash-count ht) 64)
  (error "bad count"))
(printf "ok ~a reads\n" reads)
