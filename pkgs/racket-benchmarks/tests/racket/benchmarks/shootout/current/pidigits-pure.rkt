#lang racket/base
(define (parse-count text)
  (define n (string->number text))
  (unless (exact-positive-integer? n)
    (raise-user-error 'pidigits "expected a positive integer digit count; received ~a" text))
  n)

;; The Computer Language Shootout
;; http://shootout.alioth.debian.org/
;; Based on the MLton version of the benchmark
;; contributed by Scott Cruzen

(require racket/cmdline)

;; The LFT's lower-left coefficient stays zero. Keep both candidate
;; extractions and use exact shifts for the power-of-two multipliers.
(define (digit k q r t n row col)
  (if (> n 0)
      (let ([y (quotient (+ (* q 3) r) t)])
        (if (= y (quotient (+ (arithmetic-shift q 2) r) t))
            (let ([q (* 10 q)] [r (* 10 (- r (* y t)))])
              (if (= col 10)
                  (let ([row (+ row 10)])
                    (printf "\t:~a\n~a" row y)
                    (digit k q r t (sub1 n) row 1))
                  (begin
                    (printf "~a" y)
                    (digit k q r t (sub1 n) row (add1 col)))))
            (let ([k2 (add1 (* 2 k))])
              (digit (add1 k)
                     (* q k)
                     (* (+ r (arithmetic-shift q 1)) k2)
                     (* t k2)
                     n row col))))
      (printf "~a\t:~a\n"
              (make-string (- 10 col) #\space)
              (+ row col))))

(define (digits n)
  (digit 1 1 0 1 n 0 0))

(digits (command-line #:args (n) (parse-count n)))
