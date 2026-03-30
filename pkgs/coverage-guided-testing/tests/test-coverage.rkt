#lang racket/base

;; Tests for coverage collection and diffing.

(require rackunit
         racket/set
         coverage-guided-testing/private/coverage)

(test-case "setup-errortrace! enables instrumentation"
  (setup-errortrace!)
  ;; Should not error on repeated calls
  (setup-errortrace!))

(test-case "snapshot-coverage returns a hash"
  (setup-errortrace!)
  (define snap (snapshot-coverage))
  (check-pred hash? snap))

(test-case "diff-coverage computes positive deltas"
  (define before (hash '("f" 1 5) 3 '("f" 2 3) 0))
  (define after (hash '("f" 1 5) 5 '("f" 2 3) 2 '("f" 3 1) 1))
  (define d (diff-coverage before after))
  (check-equal? (hash-ref d '("f" 1 5)) 2)
  (check-equal? (hash-ref d '("f" 2 3)) 2)
  (check-equal? (hash-ref d '("f" 3 1)) 1))

(test-case "diff-coverage ignores zero/negative deltas"
  (define before (hash '("f" 1 5) 10))
  (define after (hash '("f" 1 5) 10))
  (define d (diff-coverage before after))
  (check-equal? (hash-count d) 0))

(test-case "coverage-signature extracts hit points"
  (define d (hash '("f" 1 5) 2 '("f" 2 3) 1))
  (define sig (coverage-signature d))
  (check-equal? (set-count sig) 2)
  (check-true (set-member? sig '("f" 1 5)))
  (check-true (set-member? sig '("f" 2 3))))

(test-case "new-coverage? detects novel points"
  (define sig (set '("f" 1 5) '("f" 2 3)))
  (define global (set '("f" 1 5)))
  (check-true (new-coverage? sig global))
  (check-false (new-coverage? sig (set '("f" 1 5) '("f" 2 3)))))

(test-case "count-crosses-threshold? detects power-of-2 crossings"
  (define before (hash '("f" 1 5) 1))
  (define after (hash '("f" 1 5) 2))
  (check-true (count-crosses-threshold? before after))
  (define before2 (hash '("f" 1 5) 2))
  (define after2 (hash '("f" 1 5) 3))
  (check-false (count-crosses-threshold? before2 after2))
  (define after3 (hash '("f" 1 5) 4))
  (check-true (count-crosses-threshold? before2 after3)))

(test-case "coverage-sig-hash is deterministic"
  (define sig (set '("f" 1 5) '("f" 2 3)))
  (define h1 (coverage-sig-hash sig))
  (define h2 (coverage-sig-hash sig))
  (check-equal? h1 h2))

(test-case "load-instrumented loads a module"
  (setup-errortrace!)
  (with-output-to-file "/tmp/cov-test-mod.rkt" #:exists 'replace
    (lambda ()
      (displayln "#lang racket/base")
      (displayln "(provide foo)")
      (displayln "(define (foo x) (if (> x 0) 'pos 'neg))")))
  (define mod-path (load-instrumented "/tmp/cov-test-mod.rkt"))
  (define foo (dynamic-require mod-path 'foo))
  (define before (snapshot-coverage))
  (foo 5)
  (define after (snapshot-coverage))
  (define d (diff-coverage before after))
  (check-true (> (hash-count d) 0) "Should have coverage after calling foo"))

(printf "All coverage tests passed.\n")
