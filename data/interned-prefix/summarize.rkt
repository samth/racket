#lang racket/base
;; Usage: racket summarize.rkt <results-dir> <workload> ...
;; A workload `a-b` (written `a:b`) subtracts b's histograms from a's, to isolate the part of
;; run a that run b does not also do.
;; Reads <workload>.stats (one PREFIX-STATS line of histograms) and prints a Markdown
;; table: per operation kind, number of operations, fraction whose set holds any interned
;; scope, and mean / p50 / p99 / max of interned scopes held and walked past.
(require racket/list racket/string)

(define dir (vector-ref (current-command-line-arguments) 0))
(define workloads (cdr (vector->list (current-command-line-arguments))))

(define (load1 w)
  (define line (call-with-input-file (build-path dir (string-append w ".stats")) read-line))
  (read (open-input-string (substring line (string-length "PREFIX-STATS ")))))

(define (subtract a b)
  (for/list ([ka (in-list a)])
    (define kb (or (assq (car ka) b) (list (car ka))))
    (cons (car ka)
          (for/list ([ma (in-list (cdr ka))])
            (define mb (or (assq (car ma) (cdr kb)) (list (car ma))))
            (cons (car ma)
                  (for*/list ([na (in-list (cdr ma))]
                              [c (in-value (- (cdr na) (cond [(assv (car na) (cdr mb)) => cdr] [else 0])))]
                              #:when (positive? c))
                    (cons (car na) c)))))))

(define (load w)
  (define parts (string-split w ":"))
  (if (= 2 (length parts))
      (subtract (load1 (car parts)) (load1 (cadr parts)))
      (load1 w)))

(define (summary hist) ; hist: list of (n . count)
  (define sorted (sort hist < #:key car))
  (define total (for/sum ([p sorted]) (cdr p)))
  (define (pct q)
    (let loop ([l sorted] [acc 0])
      (define acc2 (+ acc (cdar l)))
      (if (or (null? (cdr l)) (>= acc2 (* q total))) (caar l) (loop (cdr l) acc2))))
  (values total
          (/ (for/sum ([p sorted]) (* (car p) (cdr p))) (max 1 total) 1.0)
          (pct 0.5) (pct 0.99) (car (last sorted))
          (/ (for/sum ([p sorted] #:when (positive? (car p))) (cdr p)) (max 1 total) 1.0)))

(define (fmt x) (if (integer? x) (number->string x) (real->decimal-string x 2)))

(printf "| workload | operation | ops | with interned | held mean / p50 / p99 / max | walked to a normal scope: mean / p50 / p99 / max |\n")
(printf "|---|---|---|---|---|---|\n")
(for ([w workloads])
  (define st (load w))
  (for ([kind '(update member apply subset equal)])
    (define h (cond [(assq kind st) => cdr] [else #f]))
    (when (and h (pair? (cdr (assq 'held h))))
      (define held (cdr (assq 'held h)))
      (define-values (n mean p50 p99 mx frac) (summary held))
      (define walked
        (cond
          [(let ([p (assq 'walked-to-normal h)]) (and p (pair? (cdr p)) p))
           => (lambda (p)
                (define-values (wn wmean wp50 wp99 wmx wfrac) (summary (cdr p)))
                (format "~a / ~a / ~a / ~a (~a ops)" (fmt wmean) wp50 wp99 wmx wn))]
          [else "—"]))
      (printf "| ~a | ~a | ~a | ~a% | ~a / ~a / ~a / ~a | ~a |\n"
              w kind n (real->decimal-string (* 100 frac) 1) (fmt mean) p50 p99 mx walked))))
