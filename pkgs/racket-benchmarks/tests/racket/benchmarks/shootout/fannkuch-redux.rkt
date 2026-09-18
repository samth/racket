#lang racket/base

(require racket/unsafe/ops
         racket/fixnum
         racket/performance-hint)

;; fannkuch benchmark for The Computer Language Shootout
;; Written by Dima Dorfman, 2004
;; Slightly improved by Sven Hartrumpf, 2005-2006
;; Ever-so-slightly tweaked for MzScheme by Brent Fulgham
;; PLT-ized for v4.0 by Matthew

(require racket/cmdline)

(define (list->fxvector xs) (apply fxvector xs))

(define (fannkuch n)
  (let ([pi (list->fxvector
             (for/list ([i (in-range n)]) i))]
        [tmp (make-fxvector n)]
        [count (make-fxvector n)])
    (let loop ([flips 0]
               [perms 0]
               [r n]
               [checksum 0]
               [even-parity? #t])
      (let initialize ([i 0])
        (when (unsafe-fx< i r)
          (unsafe-fxvector-set! count i (unsafe-fx+ 1 i))
          (initialize (unsafe-fx+ i 1))))
      (let* ((next-flips (count-flips pi tmp))
             (flips2 (unsafe-fxmax next-flips flips))
             (next-checksum (unsafe-fx+ checksum (if even-parity? next-flips (unsafe-fx- 0 next-flips)))))
        (let loop2 ([r 1])
          (if (unsafe-fx= r n)
              (values flips2 next-checksum)
              (let ((perm0 (unsafe-fxvector-ref pi 0)))
                (let rotate ([i 0])
                  (when (unsafe-fx< i r)
                    (unsafe-fxvector-set! pi i (unsafe-fxvector-ref pi (unsafe-fx+ 1 i)))
                    (rotate (unsafe-fx+ i 1))))
                (unsafe-fxvector-set! pi r perm0)
                (unsafe-fxvector-set! count r (unsafe-fx- (unsafe-fxvector-ref count r) 1))
                (cond
                  [(unsafe-fx<= (unsafe-fxvector-ref count r) 0)
                   (loop2 (unsafe-fx+ 1 r))]
                  [else (loop flips2 
                              (unsafe-fx+ 1 perms)
                              r 
                              next-checksum
                              (not even-parity?))]))))))))

(define-inline (count-flips pi rho)
  (let copy ([j 0])
    (unless (unsafe-fx= j (unsafe-fxvector-length pi))
      (unsafe-fxvector-set! rho j (unsafe-fxvector-ref pi j))
      (copy (unsafe-fx+ j 1))))
  (let loop ([i 0])
    (if (unsafe-fx= (unsafe-fxvector-ref rho 0) 0)
        i
        (begin
          (vector-reverse-slice! rho 0 (unsafe-fx+ 1 (unsafe-fxvector-ref rho 0)))
          (loop (unsafe-fx+ 1 i))))))

(define-inline (vector-reverse-slice! v i j)
  (let loop ([i i]
             [j (unsafe-fx- j 1)])
    (when (unsafe-fx> j i)
      (vector-swap! v i j)
      (loop (unsafe-fx+ 1 i) (unsafe-fx- j 1)))))

(define-syntax-rule (vector-swap! v i j)
  (let ((t (unsafe-fxvector-ref v i)))
    (unsafe-fxvector-set! v i (unsafe-fxvector-ref v j))
    (unsafe-fxvector-set! v j t)))

(command-line #:args (n)
              (define-values (answer checksum)
                (fannkuch (string->number n)))
              (printf "~a\nPfannkuchen(~a) = ~a\n" 
                      checksum
                      n 
                      answer))
