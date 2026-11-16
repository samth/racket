#lang racket/base
;; Worst case for sorted-list scope sets: change the OLDEST scope of an
;; identifier that has N scopes. A hasheq set pays O(log N); a sorted
;; list copies all N pairs.
(define (run n iters)
  (define intros (for/list ([i (in-range n)]) (make-syntax-introducer)))
  (define id (for/fold ([id #'x]) ([intro (in-list intros)]) (intro id 'add)))
  (define oldest (car intros))
  (define newest (car (reverse intros)))
  (define (time-it who intro)
    (collect-garbage)
    (define b (current-memory-use 'cumulative))
    (define t (current-inexact-milliseconds))
    (for ([i (in-range iters)]) (intro id 'flip))
    (define ms (- (current-inexact-milliseconds) t))
    (printf "  N=~a ~a: ~a ns/op, ~a bytes/op\n" n who
            (round (/ (* ms 1e6) iters))
            (quotient (- (current-memory-use 'cumulative) b) iters)))
  (time-it "flip newest" newest)
  (time-it "flip oldest" oldest))
(for ([n '(4 16 64 256 1024)]) (run n 200000))
