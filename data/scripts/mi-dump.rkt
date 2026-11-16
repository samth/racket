#lang racket/base
;; Usage: racket mi-dump.rkt <source.rkt> <out.rktd>
;; Compiles <source.rkt> to machine-independent form and writes, for each
;; (submodule path . bundle key), the printed form of that linklet or value.
(require compiler/zo-parse syntax/modread
         racket/port)

(define src (path->complete-path (vector-ref (current-command-line-arguments) 0)))
(define out (path->complete-path (vector-ref (current-command-line-arguments) 1)))

(define-values (dir name _dir?) (split-path (path->complete-path src)))
(define code
  (parameterize ([current-namespace (make-base-namespace)]
                 [current-load-relative-directory dir]
                 [current-directory dir]
                 [read-accept-reader #t]
                 [read-accept-lang #t]
                 [current-compile-target-machine #f])
    (define stx
      (call-with-input-file src
                            (lambda (in)
                              (port-count-lines! in)
                              (with-module-reading-parameterization (lambda ()
                                                                      (read-syntax src in))))))
    (compile stx)))

(define zo (with-output-to-bytes (lambda () (write code))))
(define top (zo-parse (open-input-bytes zo)))
(define (entries b prefix)
  (for/list ([(k v) (in-hash (linkl-bundle-table b))])
    (cons (cons prefix k) v)))
(define all
  (cond
    [(linkl-bundle? top) (entries top '())]
    [else
     (apply append
            (for/list ([(p b) (in-hash (linkl-directory-table top))])
              (entries b p)))]))
(with-output-to-file
 out
 #:exists 'truncate
 (lambda ()
   (for ([e (in-list (sort all string<? #:key (lambda (e) (format "~s" (car e)))))])
     (printf "~s\n~s\n" (car e) (cdr e)))))
