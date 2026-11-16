#lang racket/base
;; Nested local-expanding macros: each level flips its introduction
;; scope over a result that already carries newer scopes from inside.
(require (for-syntax racket/base))
(define-syntax (le stx)
  (syntax-case stx ()
    [(_ e) (local-expand #'(let ([x 1]) e) 'expression null)]))
(define-syntax (nest stx)
  (syntax-case stx ()
    [(_ 0 e) #'e]
    [(_ n e) (with-syntax ([m (sub1 (syntax-e #'n))]) #'(le (nest m e)))]))
(define-syntax (wide stx)
  (syntax-case stx ()
    [(_ k d) #`(begin #,@(for/list ([i (in-range (syntax-e #'k))]) #'(void (nest d (list 1 2 3 4 5 6 7 8)))))]))
(wide 4 100)
(printf "cumulative MB ~a, ms ~a\n" (quotient (current-memory-use (quote cumulative)) 1000000) (current-process-milliseconds))
