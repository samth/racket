#lang racket/base

(module+ test
  (require rackunit
           "left-to-right.rkt"
           "gensym.rkt"
           "simple.rkt"
           "schemify.rkt"
           "../cs/known.rkt")

  (define prim-knowns (get-prim-knowns))
  (define binds '([(loop) (lambda (x y) (if x (loop #f y) y))]))
  (define (convert bs rator rands)
    (with-deterministic-gensym
      (left-to-right/letrec bs rator rands
                           prim-knowns (hasheq) (hasheq) (hasheq) (make-hasheq) #f)))
  (define (hoisted? e) (eq? (car e) 'let-values))

  ;; Mutable reads and no-prompt effects are allowed, with explicit ordering.
  (for ([args '(((unsafe-flvector-ref v 0) (unsafe-flvector-ref v 1))
                ((unsafe-vector*-ref v 0) (unsafe-vector*-ref v 1))
                ((unsafe-vector*-set! v 0 5) (unsafe-vector*-ref v 0))
                ((vector 1) (vector 2))
                ((begin (set! x 1) x) x))])
    (check-true (hoisted? (convert binds 'loop args))))

  ;; Unknown calls, callbacks, capture, and wrong value counts stay inside.
  (for ([args '(((unknown) 0)
                ((call/cc callback) 0)
                ((set! x (call/cc callback)) 0)
                ((begin (set! x (unknown)) x) 0)
                ((vector-ref possibly-impersonated 0) 0)
                ((values 1 2) 0))])
    (check-equal? (convert binds 'loop args) `(letrec-values ,binds (loop ,@args))))

  ;; Only an immediate lambda returned from a singleton letrec qualifies.
  (for ([bs '(([(loop) (begin (effect) (lambda (x y) y))])
              ([(loop) (lambda (x y) y)] [(other) (lambda () 0)]))])
    (check-false (hoisted? (convert bs 'loop '(1 2)))))
  (check-false (hoisted? (convert binds 'other '(1 2))))
  (check-true (hoisted? (convert '([(loop) (case-lambda [(x y) y])]) 'loop '(1 2))))

  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns])
    (namespace-require 'racket/unsafe/ops)
    (namespace-require 'racket/flonum))
  (define (run e) (eval e ns))
  ;; Racket's letrec has the sequential initialization of Scheme's letrec*.
  (run '(define-syntax-rule (letrec* binds body ...)
          (letrec binds body ...)))
  (define ordered-binds '([(loop) (lambda (x y) (list x y))]))
  (define ordered-args
    '((unsafe-vector*-ref v 0)
      (begin (unsafe-vector*-set! v 0 2) (unsafe-vector*-ref v 0))))
  (check-equal? (run `(let ([v (vector 1)])
                       ,(convert ordered-binds 'loop ordered-args)))
                '(1 2))
  (check-equal? (run `(let ([v (vector 1)])
                       ((letrec-values ,ordered-binds loop) ,@ordered-args)))
                '(1 2))

  ;; `simple?` must not discard its answer for a set! RHS.
  (define (simple/no-prompt? e)
    (simple? e prim-knowns (hasheq) (hasheq) (hasheq) (make-hasheq) #f #:pure? #f))
  (check-false (simple/no-prompt? '(set! x (call/cc callback))))
  (check-false (simple/no-prompt? '(set! x (unknown))))
  (check-false (simple/no-prompt? '(set! x (values 1 2))))
  (check-true (simple/no-prompt? '(set! x (unsafe-flvector-ref v 0))))

  ;; Capturing an argument continuation must not reallocate the loop closure.
  (define capturing-binds
    '([(loop) (lambda (x)
                (if x (begin (set-box! token x) (loop #f)) loop))]))
  (define capturing-args '((call/cc (lambda (k) (set! saved k) #f))))
  (define (identity-check call)
    (run `(let ([saved #f] [first #f] [again? #f] [token (box 0)])
            (let ([p ,call])
              (if again?
                  (eq? first p)
                  (begin (set! first p) (set! again? #t) (saved #f)))))))
  (check-true (identity-check `((letrec-values ,capturing-binds loop) ,@capturing-args)))
  (check-true (identity-check (convert capturing-binds 'loop capturing-args)))
  (check-false
   (identity-check `(let-values ([(arg) ,(car capturing-args)])
                      (letrec-values ,capturing-binds (loop arg)))))

  ;; Exercise the actual schemify call site, not only the ordering helper.
  (define input
    '((lambda (v)
        ((letrec-values ([(loop) (lambda (x y) (if x (loop #f y) y))]) loop)
         (unsafe-flvector-ref v 0)
         (unsafe-flvector-ref v 1)))))
  (define output
    (schemify-body input prim-knowns (get-primitives) (hasheq) (hasheq)
                   'system #f #t #f))
  ;; The entry bindings must surround, not occur within, the recursive scope.
  (check-equal? (caaddr (car output)) 'let)
  (check-equal? (run `(let ([f ,(car output)]) (f (flvector 1.0 2.0)))) 2.0))
