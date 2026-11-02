#lang racket/base

(provide (all-defined-out))

(define (s-exp<? a b)
  (string<? (format "~s" a) (format "~s" b)))

;; Same order as sorting with `s-exp<?`, but prints each element once
(define (sort-s-exps l)
  (sort l string<? #:key (lambda (a) (format "~s" a)) #:cache-keys? #t))

(define (format-time sec)
  (let ([d (seconds->date sec)])
    (format "~a-~a-~a ~a:~a:~a"
            (date-year d) (date-month d) (date-day d)
            (date-hour d) (date-minute d) (date-second d))))

(define (ormap-strict f l)
  (cond
    [(null? l) #f]
    [else
     (define a (f (car l)))
     (define b (ormap-strict f (cdr l)))
     (or a b)]))
