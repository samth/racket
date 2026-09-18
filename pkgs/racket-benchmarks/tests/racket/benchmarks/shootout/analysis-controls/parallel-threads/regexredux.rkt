#lang racket/base
;; Parallel-thread analysis control; requires Racket CS 8.18.0.2 or newer.
;; Not an additional submission candidate. See ../README.md.

;;; The Computer Language Benchmarks Game
;;; https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

;;; based on a version by by Anthony Borla
;;; regex-dna program contributed by Matthew Flatt
;;; converted from regex-dna program
;;; Parallelized by Gustavo Massaccesi, 2018


(require racket/port
         racket/future
         racket/list)

;; -------------------------------

(define VARIANTS
  '(#"agggtaaa|tttaccct" #"[cgt]gggtaaa|tttaccc[acg]" #"a[act]ggtaaa|tttacc[agt]t"
    #"ag[act]gtaaa|tttac[agt]ct" #"agg[act]taaa|ttta[agt]cct" #"aggg[acg]aaa|ttt[cgt]ccct"
    #"agggt[cgt]aa|tt[acg]accct" #"agggta[cgt]a|t[acg]taccct" #"agggtaa[cgt]|[acg]ttaccct"))


(define IUBS
  '((#"tHa[Nt]" #"<4>") (#"aND|caN|Ha[DS]|WaS" #"<3>") (#"a[NSt]|BY" #"<2>")
    (#"<[^>]*>" #"|") (#"\\|[^|][^|]*\\|" #"-")))

;; -------------------------------

(define (ci-byte-regexp s)
  (byte-regexp (bytes-append #"(?i:" s #")")))

;; -------------------------------

(define (match-count str rx offset cnt)
  (let ([m (regexp-match-positions rx str offset)])
    (if m
        (match-count str rx (cdar m) (add1 cnt))
        cnt)))

;; -------------------------------

(module+ main
  (define-values (original-length filtered)
    (let* ([orig (port->bytes)]
           [stripped (regexp-replace* #rx#"(?:>.*?\n)|\n" orig #"")])
      (values (bytes-length orig) stripped)))
  ;; Compile before launching readers. The filtered bytes are never mutated;
  ;; replacement passes construct separate byte strings.
  (define patterns (map ci-byte-regexp VARIANTS))
  (define pool (make-parallel-thread-pool (min 2 (processor-count))))
  (define (start group)
    (thread #:pool pool #:keep 'results
            (lambda () (for/list ([rx (in-list group)])
                         (match-count filtered rx 0 0)))))
  (define worker1 (start (drop-right patterns 4)))
  (define worker2 (start (take-right patterns 4)))
  (parallel-thread-pool-close pool)
  (define replaced
    (for/fold ([sequence filtered]) ([IUB (in-list IUBS)])
      (regexp-replace* (byte-regexp (car IUB)) sequence (cadr IUB))))
  (define counts
    (append
     (thread-wait worker1 (lambda () (error 'regexredux "parallel worker failed")))
     (thread-wait worker2 (lambda () (error 'regexredux "parallel worker failed")))))
  (for ([pattern (in-list VARIANTS)] [count (in-list counts)])
    (printf "~a ~a\n" pattern count))
  (printf "\n~a\n~a\n~a\n"
          original-length (bytes-length filtered) (bytes-length replaced)))
