#lang racket/base

;; Benchmark: compare coverage-guided testing vs plain rackcheck
;; on branchy functions where guidance should help.

(require rackcheck
         racket/set
         coverage-guided-testing
         (only-in (submod rackcheck/prop private)
                  check result-status result-tests-run)
         (only-in coverage-guided-testing/private/coverage
                  setup-errortrace! snapshot-coverage))

;; Write benchmark targets
(define (write-target! path code)
  (with-output-to-file path #:exists 'replace
    (lambda () (display code))))

(define bug-target "/tmp/bench-bug-target.rkt")
(write-target! bug-target
  #<<END
#lang racket/base
(provide buggy)
(define (buggy x)
  (cond
    [(< x -500) 'very-neg]
    [(< x -100) 'neg]
    [(< x 0) 'small-neg]
    [(= x 0) 'zero]
    [(< x 100) 'small-pos]
    [(< x 500) 'pos]
    [(and (>= x 777) (<= x 780)) (error 'buggy "found the rare bug!")]
    [else 'big]))
END
)

(define deep-target "/tmp/bench-deep-target.rkt")
(write-target! deep-target
  #<<END
#lang racket/base
(provide deep)
(define (deep x y)
  (cond
    [(and (> x 50) (> y 50))
     (cond
       [(and (> x 90) (> y 90)) 'both-high]
       [(> x 75) 'x-medium-high]
       [else 'both-above-50])]
    [(and (< x -50) (< y -50))
     (cond
       [(and (< x -90) (< y -90)) 'both-very-neg]
       [else 'both-below-neg50])]
    [(= x y) 'equal]
    [(and (> x 0) (< y 0)) 'mixed-pos-neg]
    [(and (< x 0) (> y 0)) 'mixed-neg-pos]
    [else 'other]))
END
)

;; Benchmark 1: Time to first failure (rare bug)
(define (bench-unguided-failure max-tests)
  (define buggy (dynamic-require (string->path bug-target) 'buggy))
  (define start (current-inexact-milliseconds))
  (define p
    (property ([x (gen:integer-in -1000 1000)])
      (buggy x) #t))
  (define res
    (check (make-config #:tests max-tests #:deadline +inf.0) p))
  (define elapsed (- (current-inexact-milliseconds) start))
  (values (eq? (result-status res) 'falsified)
          (result-tests-run res)
          elapsed))

(define (bench-guided-failure max-iters)
  (define p
    (property ([x (gen:integer-in -1000 1000)])
      (let ([buggy (dynamic-require (string->path bug-target) 'buggy)])
        (buggy x) #t)))
  (define start (current-inexact-milliseconds))
  (define res
    (check-guided p
      #:config (make-guided-config
                #:max-iterations max-iters
                #:max-time-ms 30000
                #:seed 42)
      #:target bug-target))
  (values (eq? (guided-result-status res) 'falsified)
          (guided-result-iterations res)
          (- (current-inexact-milliseconds) start)
          (corpus-size (guided-result-corpus res))
          (guided-result-new-points-found res)))

;; Benchmark 2: Coverage reached after fixed budget
(define (bench-unguided-coverage budget)
  (setup-errortrace!)
  (define deep (dynamic-require (string->path deep-target) 'deep))
  (define before (snapshot-coverage))
  (for ([i (in-range budget)])
    (define rng (make-pseudo-random-generator))
    (parameterize ([current-pseudo-random-generator rng])
      (random-seed (+ 5000 i)))
    (define x (- (random 201 rng) 100))
    (define y (- (random 201 rng) 100))
    (deep x y))
  (define after (snapshot-coverage))
  ;; Count coverage points in the target file
  (define target-points
    (for/sum ([(k v) (in-hash after)]
              #:when (and (list? k) (string? (car k))
                          (regexp-match? #rx"bench-deep" (car k))))
      1))
  target-points)

(define (bench-guided-coverage budget)
  (define p
    (property ([x (gen:integer-in -100 100)]
               [y (gen:integer-in -100 100)])
      (let ([deep (dynamic-require (string->path deep-target) 'deep)])
        (symbol? (deep x y)))))
  (define res
    (check-guided p
      #:config (make-guided-config
                #:max-iterations budget
                #:max-time-ms 30000
                #:seed 42)
      #:target deep-target))
  (values (hash-count (guided-result-coverage-summary res))
          (corpus-size (guided-result-corpus res))
          (guided-result-new-points-found res)))

(module+ main
  (printf "=== Coverage-Guided Testing Benchmark ===\n\n")

  ;; Benchmark 1: Finding a rare bug
  (printf "--- Benchmark 1: Time to first failure ---\n")
  (printf "Target: buggy function with error in range [777, 780]\n")
  (printf "Generator range: [-1000, 1000]\n\n")

  (printf "Running unguided (plain rackcheck)...\n")
  (define-values (ug-found? ug-iters ug-time)
    (bench-unguided-failure 10000))
  (printf "  Found: ~a\n" ug-found?)
  (printf "  Tests: ~a\n" ug-iters)
  (printf "  Time: ~a ms\n\n" (exact->inexact (round ug-time)))

  (printf "Running guided...\n")
  (define-values (g-found? g-iters g-time g-corpus g-new-pts)
    (bench-guided-failure 10000))
  (printf "  Found: ~a\n" g-found?)
  (printf "  Tests: ~a\n" g-iters)
  (printf "  Time: ~a ms\n" (exact->inexact (round g-time)))
  (printf "  Corpus size: ~a\n" g-corpus)
  (printf "  New coverage points: ~a\n\n" g-new-pts)

  ;; Benchmark 2: Coverage after fixed budget
  (printf "--- Benchmark 2: Coverage reached after 500 tests ---\n")
  (printf "Target: deeply nested branchy function with 8 branches\n\n")

  (printf "Running unguided (random inputs)...\n")
  (define ug-cov (bench-unguided-coverage 500))
  (printf "  Coverage points in target: ~a\n\n" ug-cov)

  (printf "Running guided...\n")
  (define-values (g-total-cov g-corpus2 g-new-pts2)
    (bench-guided-coverage 500))
  (printf "  Total coverage points: ~a\n" g-total-cov)
  (printf "  Corpus size: ~a\n" g-corpus2)
  (printf "  New coverage points: ~a\n" g-new-pts2)

  (printf "\n=== Benchmark complete ===\n"))
