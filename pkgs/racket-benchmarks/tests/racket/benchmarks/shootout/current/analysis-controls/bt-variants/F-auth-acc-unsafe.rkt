#lang racket/base
;; binary-trees variant F-auth-acc-unsafe: authentic struct, accumulator check, unsafe fixnum ops
;; Sequential (no places) so the tree code is measured alone.
(require racket/cmdline racket/unsafe/ops)
(struct node (left right) #:authentic)
(define (make depth)
  (if (unsafe-fx= depth 0) (node #f #f)
      (let ([next (unsafe-fx- depth 1)]) (node (make next) (make next)))))
(define (check tree)
  (let loop ([tree tree] [count 0])
    (define left (node-left tree))
    (if left (loop (node-right tree) (loop left (unsafe-fx+ count 1))) (unsafe-fx+ count 1))))

(define (check-many depth iterations)
  (let loop ([remaining iterations] [count 0])
    (if (unsafe-fx= remaining 0)
        count
        (let ([tree (make depth)])
          (loop (unsafe-fx- remaining 1) (unsafe-fx+ count (check tree)))))))

(define (main n)
  (define max-depth (max 6 n))
  (define stretch-depth (add1 max-depth))
  (define gc0 (current-gc-milliseconds))
  (define t0 (current-inexact-milliseconds))
  (printf "stretch tree of depth ~a\t check: ~a\n" stretch-depth (check (make stretch-depth)))
  (define long-lived-tree (make max-depth))
  (for ([depth (in-range 4 (add1 max-depth) 2)])
    (define iterations (arithmetic-shift 1 (+ (- max-depth depth) 4)))
    (printf "~a\t trees of depth ~a\t check: ~a\n" iterations depth (check-many depth iterations)))
  (printf "long lived tree of depth ~a\t check: ~a\n" max-depth (check long-lived-tree))
  (eprintf "F-auth-acc-unsafe: total ~a ms, gc ~a ms\n"
           (round (- (current-inexact-milliseconds) t0)) (- (current-gc-milliseconds) gc0)))

(module+ main (command-line #:args (n) (main (string->number n))))
