#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [struct guided-config
    ([max-iterations exact-positive-integer?]
     [max-time-ms (>=/c 0)]
     [population-size exact-positive-integer?]
     [mutation-rate (real-in 0 1)]
     [seed exact-nonnegative-integer?]
     [verbose? boolean?])]
  [make-guided-config
   (->* []
        [#:max-iterations exact-positive-integer?
         #:max-time-ms (>=/c 0)
         #:population-size exact-positive-integer?
         #:mutation-rate (real-in 0 1)
         #:seed exact-nonnegative-integer?
         #:verbose? boolean?]
        guided-config?)]))

(struct guided-config
  (max-iterations max-time-ms population-size mutation-rate seed verbose?)
  #:transparent)

(define (make-guided-config
         #:max-iterations [max-iterations 10000]
         #:max-time-ms [max-time-ms 30000]
         #:population-size [population-size 100]
         #:mutation-rate [mutation-rate 0.5]
         #:seed [seed (modulo (current-inexact-milliseconds) (expt 2 31))]
         #:verbose? [verbose? #f])
  (guided-config max-iterations
                 max-time-ms
                 population-size
                 mutation-rate
                 (inexact->exact (floor seed))
                 verbose?))
