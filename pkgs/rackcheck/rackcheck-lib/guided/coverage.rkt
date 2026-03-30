#lang racket/base

;; Coverage collection and diffing using errortrace's execute-counts API.
;;
;; Strategy: errortrace's execute counts are cumulative and cannot be reset.
;; We snapshot counts before running a test, snapshot again after, and diff
;; to determine what the single test execution contributed.
;;
;; Coverage points are normalized to (list source-path position span) for
;; stability — we do not rely on syntax object identity.

(require racket/contract/base
         racket/set
         errortrace/errortrace-lib)

(provide
 (contract-out
  [setup-errortrace! (-> void?)]
  [load-instrumented (-> path-string? any)]
  [snapshot-coverage (-> hash?)]
  [diff-coverage (-> hash? hash? hash?)]
  [coverage-signature (-> hash? set?)]
  [new-coverage? (-> set? set? boolean?)]
  [count-crosses-threshold? (-> hash? hash? boolean?)]
  [coverage-sig-hash (-> set? exact-integer?)]))

;; Enable errortrace instrumentation for execute counts.
;; Must be called before loading the target module.
(define errortrace-setup-done? #f)

(define (setup-errortrace!)
  (unless errortrace-setup-done?
    (execute-counts-enabled #t)
    (current-compile (make-errortrace-compile-handler))
    (set! errortrace-setup-done? #t)))

;; Load a module with errortrace instrumentation active.
;; Returns the module path for use with dynamic-require.
(define (load-instrumented path)
  (define mod-path (if (path? path) path (string->path path)))
  (dynamic-require mod-path #f)
  mod-path)

;; Normalize a syntax object to a stable coverage key.
(define (stx->coverage-key stx)
  (define src (syntax-source stx))
  (define pos (syntax-position stx))
  (define span (syntax-span stx))
  (and src pos span
       (list (if (path? src) (path->string src) (format "~a" src))
             pos
             span)))

;; Snapshot the current execute counts as a hash from coverage-key to count.
(define (snapshot-coverage)
  (define counts (get-execute-counts))
  (for/fold ([h (hash)])
            ([entry (in-list counts)])
    (define key (stx->coverage-key (car entry)))
    (if key
        ;; Multiple syntax objects can map to the same key; take the max count
        (hash-set h key (max (cdr entry) (hash-ref h key 0)))
        h)))

;; Compute the difference between two snapshots.
;; Returns a hash from coverage-key to delta (only positive deltas).
(define (diff-coverage before after)
  (for/fold ([h (hash)])
            ([(key count) (in-hash after)])
    (define prev (hash-ref before key 0))
    (define delta (- count prev))
    (if (> delta 0)
        (hash-set h key delta)
        h)))

;; Extract the set of coverage keys that were exercised (had positive delta).
(define (coverage-signature diff)
  (list->set (hash-keys diff)))

;; Does a coverage signature contain any point not in the global set?
(define (new-coverage? sig global-coverage)
  (not (subset? sig global-coverage)))

;; Did any execution count cross a power-of-2 boundary?
;; This detects inputs that exercise a loop or branch more deeply.
;; We check if (prev-count, new-count] contains a power of 2.
(define (count-crosses-threshold? before after)
  (for/or ([(key count) (in-hash after)])
    (define prev (hash-ref before key 0))
    (and (> count prev)
         (let loop ([p 1])
           (cond
             [(> p count) #f]
             [(and (> p prev) (<= p count)) #t]
             [else (loop (* p 2))])))))

;; Compute a stable hash for a coverage signature (for novelty comparison).
(define (coverage-sig-hash sig)
  (define sorted (sort (set->list sig)
                       (lambda (a b)
                         (string<? (format "~a" a) (format "~a" b)))))
  (equal-hash-code sorted))
