#lang racket/base
;; Expand and compile a few large collection modules from source, in a
;; fresh namespace each time, and report the time (real ms)
(define files
  (for/list ([p '(("for.rkt" "racket/private") ("gen-match.rkt" "racket/match")
                  ("arrow-val-first.rkt" "racket/contract/private")
                  ("class-internal.rkt" "racket/private"))])
    (collection-file-path (car p) (cadr p))))
(define (compile-file f)
  (parameterize ([current-namespace (make-base-namespace)]
                 [read-accept-reader #t]
                 [current-load-relative-directory (let-values ([(d n _) (split-path f)])
                                                    d)])
    (define stx
      (call-with-input-file f
                            (lambda (in)
                              (port-count-lines! in)
                              (read-syntax f in))))
    (compile stx)))
(collect-garbage)
(define-values (r cpu real gc) (time-apply (lambda () (for-each compile-file files)) '()))
(printf "compile ~a ms (gc ~a)\n" real gc)
