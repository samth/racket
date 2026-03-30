#lang racket/base

;; The core coverage-guided testing loop.
;;
;; This is an alternative to rackcheck's built-in `check` function that adds
;; a coverage feedback loop: inputs that trigger new code coverage are saved
;; to a corpus and used to guide future generation via mutation.
;;
;; Uses only rackcheck's public API — no private submodule access.

(require racket/contract/base
         racket/match
         racket/set
         racket/random
         racket/stream
         "../prop.rkt"
         "../gen/shrink-tree.rkt"
         "config.rkt"
         "coverage.rkt"
         "corpus.rkt"
         "mutation.rkt"
         "shrinking.rkt")

(provide
 (contract-out
  [struct guided-result
    ([status symbol?]
     [iterations exact-nonnegative-integer?]
     [counterexample any/c]
     [shrunk any/c]
     [exception any/c]
     [corpus corpus?]
     [seed exact-nonnegative-integer?]
     [coverage-summary hash?]
     [new-points-found exact-nonnegative-integer?])]
  [run-guided (-> guided-config? property? (or/c #f path-string?) guided-result?)]))

(struct guided-result
  (status iterations counterexample shrunk exception
   corpus seed coverage-summary new-points-found)
  #:transparent)

;; Main entry point for guided testing.
;; `target-path` is the path to the module being tested (for instrumentation).
;; If #f, coverage feedback is collected for whatever is already instrumented.
(define (run-guided gconfig p target-path)
  (match-define (guided-config max-iters max-time-ms pop-size
                               mutation-rate seed verbose?) gconfig)

  ;; Extract the generator and test function via public accessors
  (define g (property-gen p))
  (define f (property-proc p))

  ;; Set up instrumentation
  (setup-errortrace!)
  (when target-path
    (load-instrumented target-path))

  ;; Set up RNG
  (define rng (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator rng])
    (random-seed seed))

  ;; Caller's RNG for property evaluation (rackcheck convention)
  (define caller-rng (current-pseudo-random-generator))

  (define corp (make-corpus))
  (define start-time (current-inexact-milliseconds))
  (define total-new-points 0)

  ;; Helper: run property on a list of arguments.
  ;; Returns (values passed? exception-or-#f)
  (define (test-input args)
    (with-handlers ([exn:fail? (lambda (e) (values #f e))])
      (parameterize ([current-pseudo-random-generator caller-rng])
        (if (apply f args)
            (values #t #f)
            (values #f #f)))))

  ;; Helper: generate a fresh input from the property's generator.
  ;; Returns (values args shrink-tree)
  (define (generate-fresh size)
    (define tree (g rng size))
    (define args (shrink-tree-val tree))
    (values args tree))

  ;; Helper: mutate a corpus entry's input.
  (define (mutate-from-corpus)
    (define entry (corpus-pick corp rng))
    (cond
      [entry
       (define old-input (corpus-entry-input entry))
       (define new-input
         (cond
           [(and (list? old-input) (not (null? old-input)))
            (define idx (random 0 (length old-input) rng))
            (define mutated (mutate-value (list-ref old-input idx) rng))
            (append (take-n old-input idx)
                    (list mutated)
                    (drop-n old-input (add1 idx)))]
           [else (mutate-value old-input rng)]))
       (values new-input entry)]
      [else (values #f #f)]))

  ;; Helper: splice two corpus entries
  (define (splice-from-corpus)
    (define e1 (corpus-pick corp rng))
    (define e2 (corpus-pick corp rng))
    (cond
      [(and e1 e2
            (list? (corpus-entry-input e1))
            (list? (corpus-entry-input e2)))
       (define in1 (corpus-entry-input e1))
       (define in2 (corpus-entry-input e2))
       (define spliced
         (for/list ([a (in-list in1)]
                    [b (in-list in2)])
           (splice-values a b rng)))
       (values spliced e1)]
      [else (values #f #f)]))

  ;; Helper: compute size for iteration n
  (define (iter-size n)
    (min 1000 (expt (add1 (modulo n 50)) 2)))

  ;; The main loop
  (define (loop iteration last-tree)
    (cond
      [(>= iteration max-iters)
       (make-result 'passed iteration)]
      [(>= (current-inexact-milliseconds) (+ start-time max-time-ms))
       (make-result 'timed-out iteration)]
      [else
       (when (and verbose? (zero? (modulo iteration 100)))
         (eprintf "guided: iteration ~a, corpus size ~a, coverage points ~a\n"
                  iteration (corpus-size corp) (set-count (corpus-global-coverage corp))))

       (define use-mutation?
         (and (> (corpus-size corp) 0)
              (< (random rng) mutation-rate)))

       (define-values (args parent-entry current-tree)
         (cond
           [use-mutation?
            (define-values (mutated parent)
              (if (and (< (random rng) 0.2) (>= (corpus-size corp) 2))
                  (splice-from-corpus)
                  (mutate-from-corpus)))
            (if mutated
                (values mutated parent #f)
                (let-values ([(a t) (generate-fresh (iter-size iteration))])
                  (values a #f t)))]
           [else
            (define-values (a t) (generate-fresh (iter-size iteration)))
            (values a #f t)]))

       (define before (snapshot-coverage))
       (define-values (passed? exn) (test-input args))
       (define after (snapshot-coverage))
       (define diff (diff-coverage before after))
       (define sig (coverage-signature diff))
       (define sh (coverage-sig-hash sig))

       (when (and (not (set-empty? sig))
                  (corpus-interesting? corp diff before))
         (define new-points (set-subtract sig (corpus-global-coverage corp)))
         (define entry
           (corpus-entry args
                         (if passed? #t #f)
                         sig sh iteration
                         parent-entry))
         (corpus-add! corp entry)
         (set! total-new-points (+ total-new-points (set-count new-points)))
         (when verbose?
           (eprintf "  interesting input at iteration ~a (new coverage: ~a points)\n"
                    iteration (set-count new-points))))

       (cond
         [(not passed?)
          (when verbose?
            (eprintf "guided: failure found at iteration ~a\n" iteration))
          (define shrunk
            (cond
              [current-tree
               (descend-shrinks (shrink-tree-shrinks current-tree)
                                args
                                (lambda (a) (let-values ([(p _) (test-input a)]) p)))]
              [else
               (shrink-failing-input
                args
                (lambda (a) (let-values ([(p _) (test-input a)]) (not p)))
                100)]))
          (make-result 'falsified iteration args shrunk exn)]
         [else
          (loop (add1 iteration) current-tree)])]))

  (define (make-result status iteration [args #f] [shrunk #f] [exn #f])
    (guided-result status iteration args shrunk exn
                   corp seed
                   (snapshot-coverage)
                   total-new-points))

  (loop 0 #f))

;; Descend a rackcheck shrink tree to find the smallest failing input.
(define (descend-shrinks trees last-failing-value pass?)
  (cond
    [(stream-empty? trees) last-failing-value]
    [else
     (define tree (stream-first trees))
     (define value (shrink-tree-val tree))
     (if (pass? value)
         (descend-shrinks (stream-rest trees) last-failing-value pass?)
         (descend-shrinks (shrink-tree-shrinks tree) value pass?))]))

(define (take-n lst n)
  (cond [(or (zero? n) (null? lst)) '()]
        [else (cons (car lst) (take-n (cdr lst) (sub1 n)))]))

(define (drop-n lst n)
  (cond [(or (zero? n) (null? lst)) lst]
        [else (drop-n (cdr lst) (sub1 n))]))
