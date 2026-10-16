#lang racket/base

(module+ test
  (require "simple.rkt"
           "../cs/known.rkt")

  (define-syntax-rule (check-true e)
    (unless (eq? #t e) (error 'check-true "failed: ~s" 'e)))
  (define-syntax-rule (check-false e)
    (unless (eq? #f e) (error 'check-false "failed: ~s" 'e)))

  (define prim-knowns (get-prim-knowns))
  (define (no-prompt? e
                      #:mutated [mutated (hasheq)]
                      #:mutations-checked? [mutations-checked? #t])
    (simple? e prim-knowns (hasheq) (hasheq) mutated (make-hasheq) #f
             #:pure? #f
             #:mutations-checked? mutations-checked?))

  (define loop
    '((letrec-values ([(loop) (lambda (i acc)
                                (if (unsafe-fx< i 10)
                                    (loop (unsafe-fx+ i 1) (unsafe-fx+ acc i))
                                    acc))])
        loop)
      0 0))

  ;; an immediately called named loop whose body makes only no-prompt calls
  (check-true (no-prompt? loop))
  ;; ... but only once mutation discovery has visited it
  (check-false (no-prompt? loop #:mutations-checked? #f))
  ;; ... and not if the loop variable is mutated
  (check-false (no-prompt? loop #:mutated (hasheq 'loop 'set!ed)))
  ;; wrong number of arguments
  (check-false
   (no-prompt? '((letrec-values ([(loop) (lambda (i) (if (unsafe-fx< i 10) (loop (unsafe-fx+ i 1)) i))])
                   loop)
                 0 1)))
  ;; an unknown call in the body
  (check-false
   (no-prompt? '((letrec-values ([(loop) (lambda (i) (if (unsafe-fx< i 10) (loop (unknown i)) i))])
                   loop)
                 0)))
  ;; a call to the loop that is not in the loop's own binding
  (check-false
   (no-prompt? '((letrec-values ([(loop) (lambda (i) (if (unsafe-fx< i 10) (loop (unsafe-fx+ i 1)) i))])
                   other)
                 0)))
  ;; the arguments are checked, too, outside the loop's scope
  (check-false
   (no-prompt? '((letrec-values ([(loop) (lambda (i) (if (unsafe-fx< i 10) (loop (unsafe-fx+ i 1)) i))])
                   loop)
                 (unknown 0))))
  ;; a body that escapes
  (check-false
   (no-prompt? '((letrec-values ([(loop) (lambda (i) (call/cc (lambda (k) (loop i))))])
                   loop)
                 0)))
  ;; nested loops
  (check-true
   (no-prompt? '((letrec-values ([(outer) (lambda (i)
                                            (if (unsafe-fx< i 10)
                                                (outer ((letrec-values ([(inner) (lambda (j)
                                                                                   (if (unsafe-fx< j i)
                                                                                       (inner (unsafe-fx+ j 1))
                                                                                       j))])
                                                          inner)
                                                        0))
                                                i))])
                   outer)
                 0))))
