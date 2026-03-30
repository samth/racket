#lang racket/base

;; Demo: coverage-guided testing on branchy functions.
;;
;; Run with: racket pkgs/coverage-guided-testing/examples/demo.rkt

(require coverage-guided-testing)

;; --- Demo 1: Finding a rare bug ---
(define bug-target "/tmp/demo-bug-target.rkt")
(with-output-to-file bug-target #:exists 'replace
  (lambda ()
    (displayln "#lang racket/base")
    (displayln "(provide buggy)")
    (displayln "(define (buggy x)")
    (displayln "  (cond")
    (displayln "    [(< x 0) 'negative]")
    (displayln "    [(< x 50) 'small]")
    (displayln "    [(< x 100) 'medium]")
    (displayln "    [(< x 150) 'large]")
    (displayln "    [(< x 200) 'very-large]")
    (displayln "    [(and (>= x 200) (< x 210)) (error 'buggy \"rare bug triggered!\")]")
    (displayln "    [else 'huge]))")))

(define find-bug
  (property ([x (gen:integer-in 0 1000)])
    (let ([buggy (dynamic-require (string->path bug-target) 'buggy)])
      (buggy x)
      #t)))

(module+ main
  (printf "=== Coverage-Guided Testing Demo ===\n\n")

  (printf "--- Demo 1: Finding a rare bug ---\n")
  (printf "Target: function with error in range [200, 210)\n")
  (printf "Generator: gen:integer-in 0 1000\n\n")

  (define res1
    (check-guided find-bug
      #:config (make-guided-config
                #:max-iterations 5000
                #:max-time-ms 10000
                #:seed 42
                #:verbose? #t)
      #:target bug-target))

  (newline)
  (print-guided-result res1)
  (newline)

  ;; --- Demo 2: Coverage exploration ---
  (define branch-target "/tmp/demo-branch-target.rkt")
  (with-output-to-file branch-target #:exists 'replace
    (lambda ()
      (displayln "#lang racket/base")
      (displayln "(provide classify)")
      (displayln "(define (classify x y)")
      (displayln "  (cond")
      (displayln "    [(and (> x 0) (> y 0))")
      (displayln "     (cond")
      (displayln "       [(and (> x 50) (> y 50)) 'both-high]")
      (displayln "       [(> x 25) 'x-medium]")
      (displayln "       [else 'both-low-positive])]")
      (displayln "    [(and (< x 0) (< y 0))")
      (displayln "     (cond")
      (displayln "       [(and (< x -50) (< y -50)) 'both-very-neg]")
      (displayln "       [else 'both-negative])]")
      (displayln "    [(= x y) 'equal]")
      (displayln "    [else 'mixed]))")))

  (define explore-branches
    (property ([x (gen:integer-in -100 100)]
               [y (gen:integer-in -100 100)])
      (let ([classify (dynamic-require (string->path branch-target) 'classify)])
        (symbol? (classify x y)))))

  (printf "--- Demo 2: Coverage exploration ---\n")
  (printf "Target: nested branching function with 8 branches\n\n")

  (define res2
    (check-guided explore-branches
      #:config (make-guided-config
                #:max-iterations 1000
                #:max-time-ms 5000
                #:seed 42
                #:verbose? #t)
      #:target branch-target))

  (newline)
  (print-guided-result res2)

  (printf "\nCorpus (first 10 entries):\n")
  (for ([e (in-list (corpus-entries (guided-result-corpus res2)))]
        [i (in-range 10)])
    (printf "  iter=~a input=~s\n"
            (corpus-entry-iteration e)
            (corpus-entry-input e))))
