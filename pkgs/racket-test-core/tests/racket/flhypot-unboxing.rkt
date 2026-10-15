#lang racket/base
(require rackunit racket/flonum racket/unsafe/ops)

;; Use variables so these calls exercise the runtime path, not constant folding.
(define (hypot x y) (unsafe-flhypot x y))
(for* ([x '(0.0 -0.0 3.0 -4.0 3e200 4e-200 +inf.0 -inf.0 +nan.0)]
       [y '(0.0 -0.0 4.0 -3.0 4e200 3e-200 +inf.0 -inf.0 +nan.0)])
  (check-equal? (hypot x y) (flhypot x y)))

;; The inlined call must preserve argument evaluation order and evaluate once.
(check-true (eq? unsafe-flhypot unsafe-flhypot))
(check-equal? (procedure-arity unsafe-flhypot) 2)
(check-equal? (apply unsafe-flhypot '(3.0 4.0)) 5.0)
(check-exn exn:fail:contract:arity? (lambda () (unsafe-flhypot 3.0)))
(check-exn exn:fail:contract:arity? (lambda () (unsafe-flhypot 3.0 4.0 5.0)))
(define effects null)
(define (arg n)
  (set! effects (cons n effects))
  (if (= n 1) 3.0 4.0))
(check-equal? (unsafe-flhypot (arg 1) (arg 2)) 5.0)
(check-equal? effects '(2 1))
(check-exn exn:fail:contract? (lambda () (flhypot 3 4.0)))
(check-exn exn:fail:contract? (lambda () (flhypot 3.0 4)))
(check-equal? (procedure-arity flhypot) 2)
(check-equal? (apply flhypot '(3.0 4.0)) 5.0)
(check-exn exn:fail:contract:arity? (lambda () (flhypot 3.0)))
(check-exn exn:fail:contract:arity? (lambda () (flhypot 3.0 4.0 5.0)))

(define-syntax-rule (define-run name hypot)
  (define (name n)
    (let loop ([i 0] [x 0.3] [y 0.4])
      (if (unsafe-fx< i n)
          (let ([m (hypot x y)])
            (loop (unsafe-fx+ i 1) (unsafe-fl* m 0.5) (unsafe-fl+ x 0.1)))
          (unsafe-fl< x y)))))
(define-run run-unsafe unsafe-flhypot)
(define-run run-checked flhypot)

;; Returning a boolean avoids testing allocation of the final flonum itself.
;; Allow generous fixed overhead, but reject the old 48 bytes per iteration.
(when (and (eq? (system-type 'vm) 'chez-scheme)
           (not (getenv "PLT_CS_INTERP"))
           (not (getenv "PLT_CS_COMPILE_LIMIT")))
  (for ([run (list run-unsafe run-checked)] [name '(unsafe-flhypot flhypot)])
    (run 1000)
    (collect-garbage)
    (define before (current-memory-use 'cumulative))
    (define result (run 100000))
    (define allocated (- (current-memory-use 'cumulative) before))
    (check-true result)
    (check-true (< allocated 1000000)
                (format "~a loop allocated ~a bytes" name allocated))))
