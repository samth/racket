#lang racket/base

(require racket/match srfi/13 racket/list)

(define (string-append* . args)
  (apply string-append (filter values args)))

(define (section v title [quote? #f])
  (and (not (equal? "" (string-trim-both v)))
       (string-append* (format "\n##### ~a:\n\n" title)
                       (if quote?
                           (force-quote-block v)
                           v)
                       "\n")))

;; forcibly break a string as far right as possible so that it's left of 90
(define (force-break s)
  (define l (string-length s))
  (cond [(<= l 90) s]
        [else
         (define idx
           (for/first ([i (in-range 90 0 -1)]
                       #:when (member (string-ref s i) '(#\space #\tab)))
             i))
         (if idx
             (string-append (substring s 0 idx) "\n" (force-break (substring s idx l)))
             s)]))

(define (force-quote-block v)
  (define l (regexp-split "\n" (string-trim-both v)))
  (define l2 (map force-break l))
  (define v* (apply string-append (add-between l2 "\n")))
  (string-append "\n```\n" v* "\n```\n"))