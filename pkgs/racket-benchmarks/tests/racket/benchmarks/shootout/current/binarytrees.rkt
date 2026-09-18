#lang racket/base

;;; The Computer Language Benchmarks Game
;;; https://benchmarksgame-team.pages.debian.net/benchmarksgame/
;;; Derived from the Chicken variant by Sven Hartrumpf.
;;; Contributed by Matthew Flatt; pair representation, accumulator traversal
;;; and unsafe operations by Phil Nguyen. Explicit local iteration accumulator.
;;; See LICENSE in this directory for the Benchmarks Game BSD license.

(require racket/cmdline racket/unsafe/ops)

;; Leaves and interior nodes both allocate one fresh pair. No sharing, pools,
;; closed-form checks, or GC tuning; every completed tree is walked.
(define (make depth)
  (if (unsafe-fx= depth 0)
      (cons #f #f)
      (let ([next (unsafe-fx- depth 1)])
        (cons (make next) (make next)))))

(define (check tree)
  (let loop ([tree tree] [count 0])
    (define left (unsafe-car tree))
    (if left
        (loop (unsafe-cdr tree) (loop left (unsafe-fx+ count 1)))
        (unsafe-fx+ count 1))))

(define (check-many depth iterations)
  (let loop ([remaining iterations] [count 0])
    (if (unsafe-fx= remaining 0)
        count
        (let ([tree (make depth)])
          (loop (unsafe-fx- remaining 1) (unsafe-fx+ count (check tree)))))))

(define (main n)
  (unless (exact-integer? n) (raise-argument-error 'binarytrees "exact-integer?" n))
  (define max-depth (max 6 n))
  ;; x86-64 fixnum proof: even all checks at one depth total less than2^(n+5).
  (unless (<= max-depth 50)
    (raise-argument-error 'binarytrees "integer with maximum depth at most50" n))
  (define stretch-depth (add1 max-depth))
  (printf "stretch tree of depth ~a\t check: ~a\n"
          stretch-depth (check (make stretch-depth)))
  (define long-lived-tree (make max-depth))
  (for ([depth (in-range 4 (add1 max-depth) 2)])
    (define iterations (arithmetic-shift 1 (+ (- max-depth depth) 4)))
    (printf "~a\t trees of depth ~a\t check: ~a\n"
            iterations depth (check-many depth iterations)))
  (printf "long lived tree of depth ~a\t check: ~a\n"
          max-depth (check long-lived-tree)))

(module+ main
  (command-line #:args (n) (main (string->number n))))
