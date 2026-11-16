#lang racket/base
;; Usage: racket bench.rkt <runs> <file.rkt> ...
;; For each file, expands its module form <runs> times (after one warm-up expansion that
;; loads the language) and prints the minimum time and the
;; allocation of one expansion.
(require syntax/modread)

(define runs (string->number (vector-ref (current-command-line-arguments) 0)))
(define files (cdr (vector->list (current-command-line-arguments))))

(define (read-module f)
  (call-with-input-file f
                        (lambda (in)
                          (port-count-lines! in)
                          (with-module-reading-parameterization (lambda () (read-syntax f in))))))

(for ([f (in-list files)])
  (define stx (read-module f))
  (define-values (dir name _) (split-path (path->complete-path f)))
  ;; One namespace per file, so the language is loaded only by the warm-up expansion
  (define ns (make-base-namespace))
  (define (expand-once)
    (parameterize ([current-namespace ns]
                   [current-load-relative-directory dir])
      (expand stx)))
  (expand-once)
  (define results
    (for/list ([i (in-range runs)])
      (collect-garbage)
      (define a0 (current-memory-use 'cumulative))
      (define t0 (current-inexact-monotonic-milliseconds))
      (expand-once)
      (define t1 (current-inexact-monotonic-milliseconds))
      (define a1 (current-memory-use 'cumulative))
      (cons (- t1 t0) (- a1 a0))))
  (printf "~a ~a ms ~a MB\n"
          name
          (real->decimal-string (apply min (map car results)) 1)
          (real->decimal-string (/ (apply min (map cdr results)) 1048576.0) 1)))
