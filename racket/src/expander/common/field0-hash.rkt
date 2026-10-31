#lang racket/base
(require (only-in '#%linklet primitive-table)
         racket/private/place-local
         racket/fixnum)

;; When the host supports it, an instance of a struct type registered
;; with `register-field0-eq-hash-code!` uses the value of its first
;; field as its `eq-hash-code`. That field must be immutable and hold
;; a fixnum (or, for scopes, a ratio whose numerator is a fixnum). It
;; doesn't have to be unique, just well distributed. This avoids the
;; host's table of hash codes for structs that are heavily used as
;; keys in `eq?`-based tables.

(provide register-field0-eq-hash-code!
         next-field0-hash!)

(define set-field0-eq-hash-code!
  (let ([t (primitive-table '#%expander-internal)])
    (and t (hash-ref t 'unsafe-set-struct-type-field0-eq-hash-code! #f))))

(define (register-field0-eq-hash-code! struct-type)
  (when set-field0-eq-hash-code!
    (set-field0-eq-hash-code! struct-type)))

;; Races are harmless, since a hash code doesn't have to be unique
(define-place-local counter (box 0))
(define (next-field0-hash!)
  (define n (unbox* counter))
  (set-box*! counter (fxand (fx+ n 1) #x3FFFFFFF))
  n)
