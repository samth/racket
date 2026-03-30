#lang racket/base

;; Type-aware value mutation for coverage-guided testing.
;;
;; Mutations operate at the level of Racket values (not bytes), which is
;; a key advantage of building on a PBT framework rather than a byte-level
;; fuzzer. Each type has its own set of mutation strategies.

(require racket/contract/base
         racket/random
         racket/list)

(provide
 (contract-out
  [mutate-value (-> any/c pseudo-random-generator? any/c)]
  [splice-values (-> any/c any/c pseudo-random-generator? any/c)]))

;; Dispatch mutation by type.
(define (mutate-value val rng)
  (cond
    [(boolean? val) (mutate-boolean val rng)]
    [(exact-integer? val) (mutate-integer val rng)]
    [(real? val) (mutate-real val rng)]
    [(char? val) (mutate-char val rng)]
    [(string? val) (mutate-string val rng)]
    [(bytes? val) (mutate-bytes val rng)]
    [(list? val) (mutate-list val rng)]
    [(vector? val) (mutate-vector val rng)]
    [(pair? val) (mutate-pair val rng)]
    ;; Fallback: return unchanged (we can't mutate arbitrary structs generically)
    [else val]))

;; Boolean mutation: always flip.
(define (mutate-boolean val rng)
  (not val))

;; Integer mutation strategies.
(define (mutate-integer val rng)
  (define strategies
    (list
     (lambda () 0)                              ; try zero
     (lambda () (add1 val))                     ; increment
     (lambda () (sub1 val))                     ; decrement
     (lambda () (- val))                        ; negate
     (lambda () (quotient val 2))               ; halve
     (lambda () (* val 2))                      ; double
     (lambda () (+ val (random -10 11 rng)))    ; small perturbation
     (lambda () (+ val (random -100 101 rng)))  ; medium perturbation
     (lambda () (random -1000 1001 rng))        ; random in moderate range
     (lambda () (expt 2 (random 0 32 rng)))     ; power of 2
     (lambda () (sub1 (expt 2 (random 1 32 rng)))) ; 2^n - 1
     (lambda () -1)                             ; boundary
     (lambda () 1)))                            ; boundary
  (define strategy (random-ref strategies rng))
  (strategy))

;; Real number mutation.
(define (mutate-real val rng)
  (define strategies
    (list
     (lambda () 0.0)
     (lambda () (+ val (* 0.01 (- (random rng) 0.5))))
     (lambda () (+ val (* 0.1 (- (random rng) 0.5))))
     (lambda () (- val))
     (lambda () (* val (+ 0.5 (random rng))))
     (lambda () (random rng))))
  ((random-ref strategies rng)))

;; Character mutation.
(define (mutate-char val rng)
  (define n (char->integer val))
  (define strategies
    (list
     (lambda () #\nul)
     (lambda () #\space)
     (lambda () #\newline)
     (lambda () (integer->char (modulo (add1 n) 256)))
     (lambda () (integer->char (modulo (sub1 n) 256)))
     (lambda () (integer->char (random 0 128 rng)))
     (lambda () (integer->char (random 0 256 rng)))))
  ((random-ref strategies rng)))

;; String mutation strategies.
(define (mutate-string val rng)
  (define len (string-length val))
  (define strategies
    (list
     ;; Empty string
     (lambda () "")
     ;; Insert a random char at a random position
     (lambda ()
       (define pos (random 0 (add1 len) rng))
       (define ch (integer->char (random 32 127 rng)))
       (string-append (substring val 0 pos)
                      (string ch)
                      (substring val pos)))
     ;; Delete a char (if non-empty)
     (lambda ()
       (if (zero? len) val
           (let ([pos (random 0 len rng)])
             (string-append (substring val 0 pos)
                            (substring val (add1 pos))))))
     ;; Replace a char (if non-empty)
     (lambda ()
       (if (zero? len) val
           (let ([pos (random 0 len rng)]
                 [ch (integer->char (random 32 127 rng))])
             (string-append (substring val 0 pos)
                            (string ch)
                            (substring val (add1 pos))))))
     ;; Duplicate string
     (lambda () (string-append val val))
     ;; Truncate
     (lambda ()
       (if (<= len 1) val
           (substring val 0 (random 1 len rng))))))
  ((random-ref strategies rng)))

;; Bytes mutation (similar to string but for byte strings).
(define (mutate-bytes val rng)
  (define len (bytes-length val))
  (define strategies
    (list
     (lambda () #"")
     ;; Insert a byte
     (lambda ()
       (define pos (random 0 (add1 len) rng))
       (define b (random 0 256 rng))
       (bytes-append (subbytes val 0 pos)
                     (bytes b)
                     (subbytes val pos)))
     ;; Delete a byte (if non-empty)
     (lambda ()
       (if (zero? len) val
           (let ([pos (random 0 len rng)])
             (bytes-append (subbytes val 0 pos)
                           (subbytes val (add1 pos))))))
     ;; Replace a byte (if non-empty)
     (lambda ()
       (if (zero? len) val
           (let ([pos (random 0 len rng)]
                 [b (random 0 256 rng)])
             (bytes-append (subbytes val 0 pos)
                           (bytes b)
                           (subbytes val (add1 pos))))))))
  ((random-ref strategies rng)))

;; List mutation strategies.
(define (mutate-list val rng)
  (define len (length val))
  (define strategies
    (list
     ;; Empty list
     (lambda () '())
     ;; Insert a mutated element at a random position
     (lambda ()
       (if (null? val) val
           (let* ([pos (random 0 (add1 len) rng)]
                  [elem (mutate-value (random-ref val rng) rng)]
                  [front (list-take val pos)]
                  [back (list-drop val pos)])
             (append front (list elem) back))))
     ;; Delete an element (if non-empty)
     (lambda ()
       (if (null? val) val
           (let ([pos (random 0 len rng)])
             (append (list-take val pos)
                     (list-drop val (add1 pos))))))
     ;; Replace an element with a mutation (if non-empty)
     (lambda ()
       (if (null? val) val
           (let ([pos (random 0 len rng)])
             (append (list-take val pos)
                     (list (mutate-value (list-ref val pos) rng))
                     (list-drop val (add1 pos))))))
     ;; Shuffle
     (lambda () (shuffle val rng))
     ;; Repeat an element
     (lambda ()
       (if (null? val) val
           (let ([elem (random-ref val rng)])
             (append val (list elem)))))))
  ((random-ref strategies rng)))

;; Vector mutation: convert to list, mutate, convert back.
(define (mutate-vector val rng)
  (list->vector (mutate-list (vector->list val) rng)))

;; Pair mutation (non-list pair): mutate car or cdr.
(define (mutate-pair val rng)
  (if (zero? (random 0 2 rng))
      (cons (mutate-value (car val) rng) (cdr val))
      (cons (car val) (mutate-value (cdr val) rng))))

;; Splice two values together (used for cross-corpus mutation).
;; For lists: take prefix of one, suffix of another.
;; For other types: randomly pick one and mutate it.
(define (splice-values a b rng)
  (cond
    [(and (list? a) (list? b) (not (null? a)) (not (null? b)))
     (define split-a (random 0 (add1 (length a)) rng))
     (define split-b (random 0 (add1 (length b)) rng))
     (append (list-take a split-a) (list-drop b split-b))]
    [(and (string? a) (string? b))
     (define split-a (random 0 (add1 (string-length a)) rng))
     (define split-b (random 0 (add1 (string-length b)) rng))
     (string-append (substring a 0 split-a) (substring b split-b))]
    [(and (bytes? a) (bytes? b))
     (define split-a (random 0 (add1 (bytes-length a)) rng))
     (define split-b (random 0 (add1 (bytes-length b)) rng))
     (bytes-append (subbytes a 0 split-a) (subbytes b split-b))]
    [else (mutate-value (if (zero? (random 0 2 rng)) a b) rng)]))

;; Helper: safe list take/drop
(define (list-take lst n)
  (cond
    [(or (zero? n) (null? lst)) '()]
    [else (cons (car lst) (list-take (cdr lst) (sub1 n)))]))

(define (list-drop lst n)
  (cond
    [(or (zero? n) (null? lst)) lst]
    [else (list-drop (cdr lst) (sub1 n))]))

;; Shuffle with explicit rng
(define (shuffle lst rng)
  (define vec (list->vector lst))
  (define len (vector-length vec))
  (for ([i (in-range (sub1 len) 0 -1)])
    (define j (random 0 (add1 i) rng))
    (define tmp (vector-ref vec i))
    (vector-set! vec i (vector-ref vec j))
    (vector-set! vec j tmp))
  (vector->list vec))
