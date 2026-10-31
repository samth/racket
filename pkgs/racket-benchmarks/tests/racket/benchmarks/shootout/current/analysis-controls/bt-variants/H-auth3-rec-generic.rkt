#lang racket/base
;; binary-trees variant H-auth3-rec-generic: authentic struct with an unused third field, recursive check, generic arithmetic
;; Sequential (no places) so the tree code is measured alone.
(require racket/cmdline racket/unsafe/ops)
(struct node (left right extra) #:authentic)
(define (make depth)
  (if (= depth 0) (node #f #f #f)
      (let ([next (- depth 1)]) (node (make next) (make next) #f))))
(define (check tree)
  (let ([left (node-left tree)])
    (if left (+ 1 (+ (check left) (check (node-right tree)))) 1)))

(define (check-many depth iterations)
  (let loop ([remaining iterations] [count 0])
    (if (= remaining 0)
        count
        (let ([tree (make depth)])
          (loop (- remaining 1) (+ count (check tree)))))))

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
  (eprintf "H-auth3-rec-generic: total ~a ms, gc ~a ms\n"
           (round (- (current-inexact-milliseconds) t0)) (- (current-gc-milliseconds) gc0)))

(module+ main (command-line #:args (n) (main (string->number n))))
