#lang racket/base

(module+ test
  (require rackunit
           "simple.rkt"
           "schemify.rkt"
           "../cs/known.rkt")

  (define prim-knowns (get-prim-knowns))
  (define (no-prompt? e [mutated (hasheq)])
    (simple? e prim-knowns (hasheq) (hasheq) mutated (make-hasheq) #f
             #:pure? #f))

  (check-true
   (no-prompt? '(let-values ([(f) (lambda (x) (unsafe-fx+ x 1))])
                  (f 1))))
  (check-true
   (no-prompt? '(let ([f (lambda (x) (unsafe-fx+ x 1))]) (f 1))))
  (check-true
   (no-prompt? '(letrec-values ([(f) (lambda (n)
                                           (if (unsafe-fx= n 0)
                                               n
                                               (f (unsafe-fx- n 1))))])
                  (f 3))))
  (check-true
   (no-prompt? '(letrec* ([f (lambda (n)
                                     (if (unsafe-fx= n 0)
                                         n
                                         (f (unsafe-fx- n 1))))])
                  (f 3))))
  (check-true
   (no-prompt? '(letrec-values ([(even?) (lambda (n)
                                               (if (unsafe-fx= n 0)
                                                   #t
                                                   (odd? (unsafe-fx- n 1))))]
                                      [(odd?) (lambda (n)
                                              (if (unsafe-fx= n 0)
                                                  #f
                                                  (even? (unsafe-fx- n 1))))])
                  (even? 4))))
  (check-true
   (no-prompt? '((letrec-values ([(iterate) (lambda (i z)
                                                   (if (unsafe-fx< i 4)
                                                       (iterate (unsafe-fx+ i 1) z)
                                                       i))])
                   iterate)
                 0 1.0)))
  (check-true
   (no-prompt? '(letrec-values ([(f) (case-lambda
                                          [(x) (f x)]
                                          [(x y) (unsafe-fx+ x y)])])
                  (f 1 2))))

  (check-false
   (no-prompt? '(letrec-values ([(f) (lambda (n) (unknown n))]) (f 1))))
  (check-false
   (no-prompt? '(letrec-values ([(f) (lambda (n) (call/cc (lambda (k) n)))])
                  (f 1))))
  (check-false
   (no-prompt? '(letrec-values ([(f) (lambda (n) (g n))]
                                  [(g) (lambda (n) (call/cc (lambda (k) n)))])
                  (f 1))))
  (check-false
   (no-prompt? '(letrec-values ([(f) (lambda (n) n)]) (f 1))
               (hasheq 'f 'set!ed)))
  (check-false
   (no-prompt? '(letrec-values ([(f) (lambda (n) n)]) (f 1 2))))
  (check-false
   (no-prompt? '(letrec-values ([(f) (lambda (n) (n 0))]) (f 1))))
  (check-false
   (no-prompt? '(letrec-values ([(f) (lambda (f) (f 0))]) (f unknown))))
  (check-false
   (no-prompt? '(letrec-values ([(f) (lambda (n)
                                        (let-values ([(f) n]) (f 0)))])
                  (f 1))))
  (check-false
   (no-prompt? '(letrec-values ([(f) (lambda (n)
                                        (set! f (lambda (x)
                                                  (call/cc (lambda (k) x))))
                                        (f n))])
                  (f 1))))
  (check-false
   (no-prompt? '(letrec-values ([(f) (lambda (n) n)])
                  (begin (set! f (lambda (n)
                                   (call/cc (lambda (k) n))))
                         (f 1)))))

  (define (schemify-one e)
    (car (schemify-body (list e) prim-knowns (get-primitives)
                        (hasheq) (hasheq) 'system #f #t #f)))
  (define named-loop
    '(lambda (c-rl c-im)
       (letrec-values ([() (begin 'syntax (values))]
                       [(iters)
                        ((letrec-values ([(iterate)
                                          (lambda (i z-rl z-im)
                                            (let-values ([(z-rl-2)
                                                          (unsafe-fl* z-rl z-rl)])
                                              (if (if (unsafe-fx< i 4)
                                                      (unsafe-fl<=
                                                       (unsafe-fl+ z-rl-2
                                                                   (unsafe-fl* z-im z-im))
                                                       4.0)
                                                      #f)
                                                  (iterate (fx+ i 1)
                                                           (unsafe-fl+ z-rl-2 c-rl)
                                                           (unsafe-fl+ (unsafe-fl* z-rl z-im)
                                                                       c-im))
                                                  i)))])
                           iterate)
                         0 c-rl c-im)]
                       [(lut-offset) (unsafe-fx* iters 4)])
         (unsafe-fx+ lut-offset 1))))
  (define (contains? e x)
    (or (eq? e x)
        (and (pair? e)
             (or (contains? (car e) x)
                 (contains? (cdr e) x)))))
  (check-false (contains? (schemify-one named-loop) 'unsafe-undefined))
  (check-false (contains? (schemify-one named-loop) 'set!))

  (define capturing-loop
    '(lambda ()
       (letrec-values ([(iters)
                        ((letrec-values ([(iterate)
                                          (lambda (i)
                                            (call/cc (lambda (k) i)))])
                           iterate)
                         0)]
                       [(lut-offset) (unsafe-fx* iters 4)])
         (unsafe-fx+ lut-offset 1))))
  (check-true (contains? (schemify-one capturing-loop) 'unsafe-undefined)))
