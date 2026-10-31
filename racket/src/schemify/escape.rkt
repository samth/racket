#lang racket/base
(require "wrap.rkt"
         "match.rkt")

;; EXPERIMENT: find top-level identifiers whose value can escape,
;; meaning that the identifier is used other than as the operator of a
;; direct call. The result is a hasheq of escaping symbols. Exported
;; identifiers are not included; callers check `exports` separately.
;; Identifiers are compared by symbol, so a local binding that shadows
;; a definition can only make the analysis more conservative.

(provide escaping-ids
         current-private-id?)

;; #f or a procedure that reports whether an identifier is a private,
;; non-escaping definition
(define current-private-id? (make-parameter #f))

(define (escaping-ids l)
  (define escapes (make-hasheq))
  (define (escape! id) (hash-set! escapes (unwrap id) #t))
  (define (walk* es) (for ([e (in-list es)]) (walk e)))
  (define (walk-body body) (walk* (unwrap-list body)))
  (define (walk-clauses rhss) (walk* rhss))
  (define (walk e)
    (match e
      [`(quote . ,_) (void)]
      [`(lambda ,_ . ,body) (walk-body body)]
      [`(case-lambda [,_ . ,bodys] ...) (for ([body (in-list bodys)]) (walk-body body))]
      [`(let-values ([,_ ,rhss] ...) . ,body) (walk-clauses rhss) (walk-body body)]
      [`(letrec-values ([,_ ,rhss] ...) . ,body) (walk-clauses rhss) (walk-body body)]
      [`(if ,tst ,thn ,els) (walk tst) (walk thn) (walk els)]
      [`(begin . ,body) (walk-body body)]
      [`(begin0 . ,body) (walk-body body)]
      [`(begin-unsafe . ,body) (walk-body body)]
      [`(set! ,id ,rhs) (escape! id) (walk rhs)]
      [`(with-continuation-mark ,k ,v ,body) (walk k) (walk v) (walk body)]
      [`(#%variable-reference . ,ids) (for ([id (in-list ids)]) (escape! id))]
      [`(,rator . ,rands)
       (if (symbol? (unwrap rator))
           (void)
           (walk rator))
       (walk* (unwrap-list rands))]
      [`,_
       (when (symbol? (unwrap e))
         (escape! e))]))
  (for ([form (in-list l)])
    (match form
      [`(define-values ,_ ,rhs) (walk rhs)]
      [`,_ (walk form)]))
  escapes)
