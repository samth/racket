#lang racket/base

;; Tests for corpus management and interestingness.

(require rackunit
         racket/set
         coverage-guided-testing/private/corpus
         coverage-guided-testing/private/coverage)

(test-case "make-corpus creates empty corpus"
  (define c (make-corpus))
  (check-equal? (corpus-size c) 0)
  (check-equal? (corpus-entries c) '())
  (check-true (set-empty? (corpus-global-coverage c))))

(test-case "corpus-add! adds entries"
  (define c (make-corpus))
  (define sig (set '("f" 1 5)))
  (define entry (corpus-entry '(42) #t sig (coverage-sig-hash sig) 0 #f))
  (corpus-add! c entry)
  (check-equal? (corpus-size c) 1)
  (check-equal? (set-count (corpus-global-coverage c)) 1))

(test-case "corpus-interesting? detects new coverage"
  (define c (make-corpus))
  ;; Add entry covering point A
  (define sig-a (set '("f" 1 5)))
  (corpus-add! c (corpus-entry '(1) #t sig-a (coverage-sig-hash sig-a) 0 #f))

  ;; Diff that covers point B (new)
  (define diff-b (hash '("f" 2 3) 1))
  (check-true (corpus-interesting? c diff-b (hash)))

  ;; Diff that covers only point A (not new)
  (define diff-a (hash '("f" 1 5) 1))
  ;; Still interesting because of novel sig hash
  (check-true (corpus-interesting? c diff-a (hash))))

(test-case "corpus-pick returns an entry"
  (define c (make-corpus))
  (define sig (set '("f" 1 5)))
  (corpus-add! c (corpus-entry '(1) #t sig 0 0 #f))
  (corpus-add! c (corpus-entry '(2) #t sig 1 1 #f))
  (define rng (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator rng])
    (random-seed 42))
  (define picked (corpus-pick c rng))
  (check-true (corpus-entry? picked)))

(test-case "corpus-pick returns #f for empty corpus"
  (define c (make-corpus))
  (define rng (make-pseudo-random-generator))
  (check-false (corpus-pick c rng)))

(test-case "corpus-best-entries returns sorted entries"
  (define c (make-corpus))
  (define sig1 (set '("f" 1 5)))
  (define sig2 (set '("f" 1 5) '("f" 2 3) '("f" 3 1)))
  (corpus-add! c (corpus-entry '(1) #t sig1 0 0 #f))
  (corpus-add! c (corpus-entry '(2) #t sig2 1 1 #f))
  (define best (corpus-best-entries c 1))
  (check-equal? (length best) 1)
  (check-equal? (corpus-entry-input (car best)) '(2)))

(printf "All corpus tests passed.\n")
