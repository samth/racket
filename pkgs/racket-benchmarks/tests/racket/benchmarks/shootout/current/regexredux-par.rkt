#lang racket/base

;;; The Computer Language Benchmarks Game
;;; https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

;;; based on a version by by Anthony Borla
;;; regex-dna program contributed by Matthew Flatt
;;; converted from regex-dna program
;;; Parallelized by Gustavo Massaccesi, 2018


(require racket/port
         racket/place
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

;; Literal replacement using the same regex engine and patterns. All benchmark
;; patterns consume at least one byte and none uses replacement backreferences.
;; Keep a logical length so the next pass need not copy a trimmed byte string.
(define (replace-into rx source length replacement destination)
  (define replacement-length (bytes-length replacement))
  (define buffer destination)
  (define (reserve! needed)
    (when (> needed (bytes-length buffer))
      (define larger (make-bytes (max needed (* 2 (bytes-length buffer)))))
      (bytes-copy! larger 0 buffer)
      (set! buffer larger)))
  (let loop ([start 0] [written 0])
    (define positions (regexp-match-positions rx source start length))
    (cond
      [positions
       (define match (car positions))
       (define end (+ written (- (car match) start)))
       (define next (+ end replacement-length))
       (reserve! next)
       (bytes-copy! buffer written source start (car match))
       (bytes-copy! buffer end replacement)
       (loop (cdr match) next)]
      [else
       (define end (+ written (- length start)))
       (reserve! end)
       (bytes-copy! buffer written source start length)
       (values buffer end)])))

(define (strip-input original)
  (define-values (buffer length)
    (replace-into #rx#"(?:>.*?\n)|\n" original (bytes-length original)
                  #"" (make-bytes (bytes-length original))))
  (subbytes buffer 0 length))

(define (replace-sequence filtered)
  (define buffer1 (make-bytes (bytes-length filtered)))
  (define buffer2 (make-bytes (bytes-length filtered)))
  (let loop ([rules IUBS] [source filtered] [length (bytes-length filtered)]
             [destination buffer1] [spare buffer2])
    (cond
      [(null? rules) (values source length)]
      [else
       (define-values (result next-length)
         (replace-into (byte-regexp (caar rules)) source length (cadar rules) destination))
       (loop (cdr rules) result next-length spare result)])))

(define (make-counting-place)
  (place ch
    (define filtered (place-channel-get ch))
    (define in (place-channel-get ch))
    (define out (map (lambda (i)
                       (match-count filtered (ci-byte-regexp i) 0 0))
                     in))
    (place-channel-put ch out)))

;; -------------------------------
;; -------------------------------

(module+ main
   ;; Load sequence
  (define-values (original-length filtered)
    (let* ([orig (port->bytes)]
           [stripped (strip-input orig)]
           [shared (make-shared-bytes (bytes-length stripped))])
      (bytes-copy! shared 0 stripped)
      (values (bytes-length orig) shared)))

  ;; Buffer reuse makes the replacements short enough to balance three
  ;; groups of counting passes against the parent-side replacement chain.
  (define workers
    (for/list ([start (in-range 0 9 3)])
      (define worker (make-counting-place))
      (place-channel-put worker filtered)
      (place-channel-put worker (take (drop VARIANTS start) 3))
      worker))
    
  ;; Perform regexp replacements while the places are running
  (define-values (replaced replaced-length) (replace-sequence filtered))

  ;; Collect the results of the regexp counts
  (define counts
    (apply append (for/list ([worker (in-list workers)])
                    (place-channel-get worker))))
  (for ([worker (in-list workers)]) (place-wait worker))


  ;; Print regexp counts
  (for ([i (in-list VARIANTS)]
        [j (in-list counts)])
    (printf "~a ~a\n" i j))

  ;; Print statistics
  (printf "\n~a\n~a\n~a\n"
          original-length (bytes-length filtered) replaced-length)

  )
