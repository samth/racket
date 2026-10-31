#lang racket/base

;;; The Computer Language Benchmarks Game
;;; https://benchmarksgame-team.pages.debian.net/benchmarksgame/
;;; Derived from the Chicken variant by Sven Hartrumpf.
;;; Contributed by Matthew Flatt; places design by Gustavo Massaccesi.
;;; Bounded worker task groups. Natural version: authentic struct nodes,
;;; plain recursive check, safe generic arithmetic.
;;; See LICENSE in this directory for the Benchmarks Game BSD license.

(require racket/cmdline racket/place)

;; A leaf is a node with #f children. Every node is a fresh authentic struct.
(struct node (left right) #:authentic)

(define (make depth)
  (if (= depth 0)
      (node #f #f)
      (let ([next (- depth 1)])
        (node (make next) (make next)))))

(define (check tree)
  (let ([left (node-left tree)])
    (if left
        (+ 1 (check left) (check (node-right tree)))
        1)))

(define (check-many depth iterations)
  (let loop ([remaining iterations] [count 0])
    (if (= remaining 0)
        count
        (let ([tree (make depth)])
          (loop (- remaining 1) (+ count (check tree)))))))

(define (run-tasks tasks)
  (for/list ([task (in-list tasks)])
    (define depth (car task))
    (define iterations (cadr task))
    (list depth iterations (check-many depth iterations))))

(define (make-worker)
  (place channel
    (place-channel-put channel (run-tasks (place-channel-get channel)))))

(define (main n)
  (unless (exact-integer? n) (raise-argument-error 'binarytrees "exact-integer?" n))
  (define max-depth (max 6 n))
  (unless (<= max-depth 50)
    (raise-argument-error 'binarytrees "integer with maximum depth at most50" n))
  (define stretch-depth (add1 max-depth))
  (printf "stretch tree of depth ~a\t check: ~a\n"
          stretch-depth (check (make stretch-depth)))
  (define long-lived-tree (make max-depth))
  (define tasks
    (for/list ([depth (in-range 4 (add1 max-depth) 2)])
      (list depth (arithmetic-shift 1 (+ (- max-depth depth) 4)))))
  (define workers (min 4 (length tasks)))
  (define groups
    (for/list ([worker (in-range workers)])
      (for/list ([task (in-list tasks)] [i (in-naturals)]
                 #:when (= (modulo i workers) worker))
        task)))
  (define channels
    (for/list ([tasks (in-list (cdr groups))])
      (define channel (make-worker))
      (place-channel-put channel tasks)
      channel))
  (define own-results (run-tasks (car groups)))
  (define results
    (append own-results
            (apply append (for/list ([channel (in-list channels)])
                            (define result (place-channel-get channel))
                            (place-wait channel)
                            result))))
  (for ([row (in-list (sort results < #:key car))])
    (printf "~a\t trees of depth ~a\t check: ~a\n"
            (cadr row) (car row) (caddr row)))
  (printf "long lived tree of depth ~a\t check: ~a\n"
          max-depth (check long-lived-tree)))

(module+ main
  (command-line #:args (n) (main (string->number n))))
