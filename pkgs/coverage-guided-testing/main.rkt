#lang racket/base

;; Coverage-guided property-based testing for Racket.
;;
;; This library extends rackcheck with a coverage-guided feedback loop:
;; inputs that trigger new code coverage are saved to a corpus and used
;; to guide future generation via mutation.

(require racket/contract/base
         racket/set
         racket/format
         (only-in rackcheck
                  property define-property
                  gen:natural gen:integer-in gen:real gen:boolean
                  gen:char gen:char-in gen:char-letter gen:char-digit
                  gen:char-alphanumeric
                  gen:string gen:bytes gen:symbol
                  gen:list gen:vector gen:tuple gen:hash
                  gen:one-of gen:frequency
                  gen:const gen:map gen:bind gen:filter gen:choice
                  gen:sized gen:resize gen:scale gen:no-shrink gen:with-shrink
                  make-gen gen? sample
                  make-config config?
                  property? property-name
                  label!)
         (submod rackcheck/prop private)
         "private/config.rkt"
         "private/coverage.rkt"
         "private/corpus.rkt"
         "private/mutation.rkt"
         "private/guidance.rkt")

;; Re-export rackcheck essentials so users only need one require
(provide
 ;; From rackcheck
 property define-property property? property-name
 gen:natural gen:integer-in gen:real gen:boolean
 gen:char gen:char-in gen:char-letter gen:char-digit gen:char-alphanumeric
 gen:string gen:bytes gen:symbol
 gen:list gen:vector gen:tuple gen:hash
 gen:one-of gen:frequency
 gen:const gen:map gen:bind gen:filter gen:choice
 gen:sized gen:resize gen:scale gen:no-shrink gen:with-shrink
 make-gen gen? sample
 label!

 ;; Guided testing API
 (contract-out
  ;; Configuration
  [make-guided-config
   (->* []
        [#:max-iterations exact-positive-integer?
         #:max-time-ms (>=/c 0)
         #:population-size exact-positive-integer?
         #:mutation-rate (real-in 0 1)
         #:seed exact-nonnegative-integer?
         #:verbose? boolean?]
        guided-config?)]
  [guided-config? (-> any/c boolean?)]

  ;; Running guided checks
  [check-guided
   (->* [property?]
        [#:config guided-config?
         #:target path-string?]
        guided-result?)]

  ;; Results
  [guided-result? (-> any/c boolean?)]
  [guided-result-status (-> guided-result? symbol?)]
  [guided-result-counterexample (-> guided-result? any/c)]
  [guided-result-shrunk (-> guided-result? any/c)]
  [guided-result-exception (-> guided-result? any/c)]
  [guided-result-iterations (-> guided-result? exact-nonneg-integer?)]
  [guided-result-corpus (-> guided-result? corpus?)]
  [guided-result-seed (-> guided-result? exact-nonneg-integer?)]
  [guided-result-coverage-summary (-> guided-result? hash?)]
  [guided-result-new-points-found (-> guided-result? exact-nonneg-integer?)]

  ;; Corpus inspection
  [corpus? (-> any/c boolean?)]
  [corpus-entries (-> corpus? list?)]
  [corpus-size (-> corpus? exact-nonneg-integer?)]
  [corpus-entry? (-> any/c boolean?)]
  [corpus-entry-input (-> corpus-entry? any/c)]
  [corpus-entry-coverage-sig (-> corpus-entry? set?)]
  [corpus-entry-iteration (-> corpus-entry? exact-nonneg-integer?)]
  [corpus-entry-parent (-> corpus-entry? (or/c #f corpus-entry?))]

  ;; Replay
  [replay-input (-> property? list? any/c)]

  ;; Reporting
  [print-guided-result (-> guided-result? void?)]

  ;; rackunit integration
  [check-guided-property
   (->* [property?]
        [#:config guided-config?
         #:target path-string?]
        void?)]))

(define (exact-nonneg-integer? v)
  (and (exact-integer? v) (>= v 0)))

;; Run a guided property check.
(define (check-guided prop
                      #:config [config (make-guided-config)]
                      #:target [target #f])
  (run-guided config prop (and target (if (path? target) target (string->path target)))))

;; Replay a specific input against a property.
;; Returns the property result (truthy or falsy).
(define (replay-input p args)
  (define f (prop-f p))
  (with-handlers ([exn:fail? (lambda (e) e)])
    (apply f args)))

;; Print a human-readable summary of a guided result.
(define (print-guided-result res)
  (define status (guided-result-status res))
  (printf "Coverage-guided testing result:\n")
  (printf "  Status: ~a\n" status)
  (printf "  Iterations: ~a\n" (guided-result-iterations res))
  (printf "  Seed: ~a\n" (guided-result-seed res))
  (printf "  Corpus size: ~a\n" (corpus-size (guided-result-corpus res)))
  (printf "  New coverage points found: ~a\n" (guided-result-new-points-found res))
  (printf "  Total coverage points: ~a\n"
          (hash-count (guided-result-coverage-summary res)))

  (case status
    [(falsified)
     (printf "  Counterexample: ~s\n" (guided-result-counterexample res))
     (when (guided-result-shrunk res)
       (printf "  Shrunk: ~s\n" (guided-result-shrunk res)))
     (when (guided-result-exception res)
       (printf "  Exception: ~a\n" (exn-message (guided-result-exception res))))]
    [(passed)
     (printf "  All iterations passed.\n")]
    [(timed-out)
     (printf "  Timed out.\n")]))

;; rackunit integration: fails the test if the property is falsified.
(define (check-guided-property prop
                               #:config [config (make-guided-config)]
                               #:target [target #f])
  (define res (check-guided prop #:config config #:target (and target target)))
  (case (guided-result-status res)
    [(falsified)
     (define shrunk (or (guided-result-shrunk res)
                        (guided-result-counterexample res)))
     (error 'check-guided-property
            "property ~a falsified after ~a iterations\n  counterexample: ~s\n  shrunk: ~s"
            (property-name prop)
            (guided-result-iterations res)
            (guided-result-counterexample res)
            shrunk)]
    [(timed-out)
     (printf "  ~ property ~a timed out after ~a iterations\n"
             (property-name prop)
             (guided-result-iterations res))]
    [(passed)
     (printf "  ✓ property ~a passed ~a guided iterations (corpus: ~a, new coverage: ~a)\n"
             (property-name prop)
             (guided-result-iterations res)
             (corpus-size (guided-result-corpus res))
             (guided-result-new-points-found res))]))
