#lang racket/base

;; In-memory corpus of interesting test inputs.

(require racket/contract/base
         racket/set
         racket/random
         "coverage.rkt")

(provide
 (struct-out corpus-entry)
 make-corpus
 corpus?
 corpus-add!
 corpus-entries
 corpus-size
 corpus-global-coverage
 corpus-interesting?
 corpus-pick
 corpus-best-entries)

(struct corpus-entry
  (input outcome coverage-sig sig-hash iteration parent)
  #:transparent)

(struct corpus
  (entries-box global-coverage-box sig-hashes-box)
  #:transparent)

(define (make-corpus)
  (corpus (box '()) (box (set)) (box (set))))

(define (corpus-entries c)
  (unbox (corpus-entries-box c)))

(define (corpus-size c)
  (length (corpus-entries c)))

(define (corpus-global-coverage c)
  (unbox (corpus-global-coverage-box c)))

(define (corpus-add! c entry)
  (set-box! (corpus-entries-box c) (cons entry (unbox (corpus-entries-box c))))
  (set-box! (corpus-global-coverage-box c)
            (set-union (unbox (corpus-global-coverage-box c))
                       (corpus-entry-coverage-sig entry)))
  (set-box! (corpus-sig-hashes-box c)
            (set-add (unbox (corpus-sig-hashes-box c))
                     (corpus-entry-sig-hash entry))))

;; Check if a coverage diff is "interesting" relative to the corpus.
(define (corpus-interesting? c coverage-diff coverage-before)
  (define sig (coverage-signature coverage-diff))
  (define global (unbox (corpus-global-coverage-box c)))
  (define sig-hashes (unbox (corpus-sig-hashes-box c)))
  (define sh (coverage-sig-hash sig))
  (or
   (new-coverage? sig global)
   (not (set-member? sig-hashes sh))
   (count-crosses-threshold? coverage-before
                             (for/fold ([h coverage-before])
                                       ([(k v) (in-hash coverage-diff)])
                               (hash-set h k (+ v (hash-ref h k 0)))))))

;; Pick a corpus entry for mutation, favoring recent entries.
(define (corpus-pick c rng)
  (define entries (corpus-entries c))
  (cond
    [(null? entries) #f]
    [else
     (define total-weight
       (for/sum ([e (in-list entries)])
         (add1 (corpus-entry-iteration e))))
     (define target (random 0 total-weight rng))
     (let loop ([entries entries] [acc 0])
       (cond
         [(null? (cdr entries)) (car entries)]
         [else
          (define w (add1 (corpus-entry-iteration (car entries))))
          (if (< target (+ acc w))
              (car entries)
              (loop (cdr entries) (+ acc w)))]))]))

;; Return the N entries with the largest coverage signatures.
(define (corpus-best-entries c n)
  (define sorted
    (sort (corpus-entries c) >
          #:key (lambda (e) (set-count (corpus-entry-coverage-sig e)))))
  (define len (length sorted))
  (if (<= len n)
      sorted
      (let loop ([lst sorted] [i 0])
        (if (= i n) '()
            (cons (car lst) (loop (cdr lst) (add1 i)))))))
