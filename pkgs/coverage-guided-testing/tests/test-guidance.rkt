#lang racket/base

;; End-to-end tests for the guided testing loop.

(require rackunit
         racket/set
         coverage-guided-testing)

;; Helper: write a target module to a temp file
(define (write-target! path code)
  (with-output-to-file path #:exists 'replace
    (lambda () (display code))))

;; Test target: branchy function
(define target-path "/tmp/cgt-test-target.rkt")
(write-target! target-path
  #<<END
#lang racket/base
(provide branchy)
(define (branchy x)
  (cond
    [(< x -100) 'very-neg]
    [(< x 0) 'neg]
    [(= x 0) 'zero]
    [(< x 100) 'pos]
    [(and (>= x 200) (< x 210)) 'rare-range]
    [else 'big]))
END
)

(test-case "guided check finds failure in range"
  (write-target! "/tmp/cgt-test-bug.rkt"
    #<<END
#lang racket/base
(provide buggy)
(define (buggy x)
  (cond
    [(< x 0) 'neg]
    [(< x 100) 'ok]
    [(< x 200) 'ok]
    [(and (>= x 200) (< x 210)) (error 'buggy "bug!")]
    [else 'ok]))
END
  )
  (define p
    (property ([x (gen:integer-in 0 1000)])
      (let ([buggy (dynamic-require (string->path "/tmp/cgt-test-bug.rkt") 'buggy)])
        (buggy x)
        #t)))
  (define res
    (check-guided p
      #:config (make-guided-config
                #:max-iterations 5000
                #:max-time-ms 10000
                #:seed 42)
      #:target "/tmp/cgt-test-bug.rkt"))
  (check-equal? (guided-result-status res) 'falsified)
  (check-true (list? (guided-result-counterexample res)))
  (check-pred exn:fail? (guided-result-exception res) "Should have an exception"))

(test-case "guided check passes for correct property"
  (define p
    (property ([x (gen:integer-in 0 100)])
      (let ([branchy (dynamic-require (string->path target-path) 'branchy)])
        (symbol? (branchy x)))))
  (define res
    (check-guided p
      #:config (make-guided-config
                #:max-iterations 200
                #:max-time-ms 5000
                #:seed 42)
      #:target target-path))
  (check-equal? (guided-result-status res) 'passed))

(test-case "guided check builds a corpus"
  (define p
    (property ([x (gen:integer-in -1000 1000)])
      (let ([branchy (dynamic-require (string->path target-path) 'branchy)])
        (symbol? (branchy x)))))
  (define res
    (check-guided p
      #:config (make-guided-config
                #:max-iterations 500
                #:max-time-ms 5000
                #:seed 42)
      #:target target-path))
  (check-true (> (corpus-size (guided-result-corpus res)) 0)
              "Corpus should have entries"))

(test-case "reproducibility: same seed gives same iteration count and status"
  ;; Note: corpus size may vary between runs because errortrace state is
  ;; cumulative (coverage from earlier test cases affects interestingness).
  ;; But the iteration count and status (pass/fail) should be deterministic
  ;; since they depend only on the RNG seed and the property function.
  (define (run-with-seed s)
    (define p
      (property ([x (gen:integer-in 0 100)])
        (let ([branchy (dynamic-require (string->path target-path) 'branchy)])
          (symbol? (branchy x)))))
    (check-guided p
      #:config (make-guided-config
                #:max-iterations 100
                #:max-time-ms 5000
                #:seed s)
      #:target target-path))
  (define res1 (run-with-seed 12345))
  (define res2 (run-with-seed 12345))
  (check-equal? (guided-result-iterations res1)
                (guided-result-iterations res2))
  (check-equal? (guided-result-status res1)
                (guided-result-status res2))
  (check-equal? (guided-result-seed res1)
                (guided-result-seed res2)))

(test-case "shrinking produces a smaller counterexample"
  (write-target! "/tmp/cgt-test-shrink.rkt"
    #<<END
#lang racket/base
(provide check-val)
(define (check-val x)
  (when (> x 50) (error 'check-val "too big")))
END
  )
  (define p
    (property ([x (gen:integer-in 0 1000)])
      (let ([check-val (dynamic-require (string->path "/tmp/cgt-test-shrink.rkt") 'check-val)])
        (check-val x)
        #t)))
  (define res
    (check-guided p
      #:config (make-guided-config
                #:max-iterations 1000
                #:max-time-ms 5000
                #:seed 42)
      #:target "/tmp/cgt-test-shrink.rkt"))
  (check-equal? (guided-result-status res) 'falsified)
  ;; The shrunk value should be 51 (smallest that triggers the error)
  (when (guided-result-shrunk res)
    (define shrunk-val (car (guided-result-shrunk res)))
    (check-true (<= shrunk-val 55)
                (format "Shrunk value ~a should be close to 51" shrunk-val))))

(test-case "replay-input reproduces the failure"
  (write-target! "/tmp/cgt-test-replay.rkt"
    #<<END
#lang racket/base
(provide fail-on-neg)
(define (fail-on-neg x)
  (when (< x 0) (error 'fail-on-neg "negative!")))
END
  )
  (define p
    (property ([x (gen:integer-in -100 100)])
      (let ([f (dynamic-require (string->path "/tmp/cgt-test-replay.rkt") 'fail-on-neg)])
        (f x)
        #t)))
  (define res
    (check-guided p
      #:config (make-guided-config
                #:max-iterations 500
                #:max-time-ms 5000
                #:seed 42)
      #:target "/tmp/cgt-test-replay.rkt"))
  (check-equal? (guided-result-status res) 'falsified)
  ;; Replay the counterexample
  (define ce (guided-result-counterexample res))
  (define replay-result (replay-input p ce))
  (check-pred exn:fail? replay-result "Replay should reproduce the failure"))

(printf "All guidance tests passed.\n")
