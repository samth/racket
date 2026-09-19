#lang racket/base
;; Parallel-thread analysis control; requires Racket CS 8.18.0.2 or newer.
;; Not an additional submission candidate. See ../README.md.
(require racket/unsafe/ops racket/fixnum racket/future)

;;; The Computer Language Benchmarks Game
;;; https://benchmarksgame-team.pages.debian.net/benchmarksgame/
;;; Based on the Racket program contributed by Matthew Flatt.
;;; Compact fixnum keys, full reading-frame traversal, THREE parsing and lexical ties.
;;; See LICENSE in this directory for the Benchmarks Game BSD license.

(define (read-three in)
  (let seek ()
    (define line (read-bytes-line in 'any))
    (cond
      [(eof-object? line) #""]
      [(regexp-match? #rx#"^>THREE([ \t]|$)" line)
       (define out (open-output-bytes))
       (let collect ()
         (define line (read-bytes-line in 'any))
         (unless (or (eof-object? line)
                     (and (positive? (bytes-length line))
                          (= (bytes-ref line 0) 62)))
           (unless (and (positive? (bytes-length line))
                        (= (bytes-ref line 0) 59))
             (write-bytes line out))
           (collect)))
       (get-output-bytes out)]
      [else (seek)])))

;; Current rules explicitly permit compact DNA codes. This control still
;; counts every fragment in every reading frame in a built-in hash table.
(define (encode-dna dna)
  (unless (<= (bytes-length dna) (- (most-positive-fixnum) 18))
    (error 'knucleotide "input too long for the x86-64 index fast path"))
  (define encoded (make-bytes (bytes-length dna)))
  (for ([b (in-bytes dna)] [i (in-naturals)])
    (bytes-set! encoded i
                (case b [(65 97) 0] [(67 99) 1] [(71 103) 2] [(84 116) 3]
                  [else (error 'knucleotide "expected A, C, G or T in record THREE")])))
  encoded)

(define (encode-key key)
  (for/fold ([n 0]) ([b (in-bytes (encode-dna key))])
    (unsafe-fx+ (unsafe-fxlshift n 2) b)))

(define (key->bytes key len)
  (define result (make-bytes len))
  (let loop ([i (sub1 len)] [n key])
    (unless (< i 0)
      (bytes-set! result i (bytes-ref #"ACGT" (bitwise-and n 3)))
      (loop (sub1 i) (arithmetic-shift n -2))))
  result)

(define (count-frame! table dna len frame)
  (define stop (unsafe-fx- (bytes-length dna) len))
  (let next-fragment ([start frame])
    (unless (unsafe-fx> start stop)
      (define end (unsafe-fx+ start len))
      (define key
        (let encode ([i start] [key 0])
          (if (unsafe-fx= i end)
              key
              (encode (unsafe-fx+ i 1)
                      (unsafe-fx+ (unsafe-fxlshift key 2)
                                  (unsafe-bytes-ref dna i))))))
      (define counter (hash-ref table key #f))
      (if counter
          (set-box! counter (unsafe-fx+ (unbox counter) 1))
          (hash-set! table key (box 1)))
      (next-fragment (unsafe-fx+ start len)))))

(define (all-counts len dna)
  (unless (and (exact-integer? len) (<= 1 len 18))
    (raise-argument-error 'all-counts "integer in1..18" len))
  (define table (make-hasheq))
  (for ([frame (in-range len)]) (count-frame! table dna len frame))
  table)

(define (write-frequencies table len)
  (define entries
    (hash-map table (lambda (key count) (cons (key->bytes key len) (unbox count)))))
  (define total (for/sum ([entry (in-list entries)]) (cdr entry)))
  (define (before? a b)
    (or (> (cdr a) (cdr b))
        (and (= (cdr a) (cdr b)) (bytes<? (car a) (car b)))))
  (for ([entry (in-list (sort entries before?))])
    (printf "~a ~a\n" (car entry)
            (real->decimal-string (* 100.0 (/ (cdr entry) total)) 3))))

(define lengths '(1 2 3 4 6 12 18))

;; One complete built-in histogram per reading frame, as in the current
;; Java submission. Start the longer frame jobs first; the parent participates.
(define jobs
  (list->vector
   (for*/list ([len (in-list lengths)] [frame (in-range len)])
     (cons len frame))))

(define (parallel-counts dna)
  (define results (make-vector (vector-length jobs)))
  (define next (box 0))
  (define (work)
    (let loop ()
      (define i (unbox next))
      (when (< i (vector-length jobs))
        (when (box-cas! next i (add1 i))
          (define job (vector-ref jobs i))
          (define table (make-hasheq))
          (count-frame! table dna (car job) (cdr job))
          (vector-set! results i table))
        (loop))))
  (define workers (min 4 (processor-count)))
  (define pool (make-parallel-thread-pool (max 1 (sub1 workers))))
  (define threads
    (for/list ([i (in-range (sub1 workers))])
      (thread #:pool pool #:keep 'results work)))
  (parallel-thread-pool-close pool)
  (work)
  (for ([worker (in-list threads)])
    (thread-wait worker (lambda () (error 'knucleotide "parallel worker failed"))))
  results)

(define (tables-for results len)
  (for/list ([job (in-vector jobs)] [table (in-vector results)]
             #:when (= (car job) len))
    table))

(define (merge-counts tables)
  (define result (make-hasheq))
  (for* ([table (in-list tables)] [(key count) (in-hash table)])
    (define previous (hash-ref result key #f))
    (if previous
        (set-box! previous (+ (unbox previous) (unbox count)))
        (hash-set! result key (box (unbox count)))))
  result)

(define (produce-row dna len [tables (list (all-counts len dna))])
  (define out (open-output-bytes))
  (parameterize ([current-output-port out])
    (if (<= len 2)
        (begin (write-frequencies (merge-counts tables) len) (newline))
        (let* ([key (case len
                      [(3) #"GGT"] [(4) #"GGTA"] [(6) #"GGTATT"]
                      [(12) #"GGTATTTTAATT"] [(18) #"GGTATTTTAATTTATAGT"])]
               [encoded (encode-key key)]
               [count (for/sum ([table (in-list tables)])
                        (define counter (hash-ref table encoded #f))
                        (if counter (unbox counter) 0))])
          (printf "~a\t~a\n" count key))))
  (get-output-bytes out))

(define (main [in (current-input-port)] [out (current-output-port)])
  ;; Fully initialize once before starting readers. Each frame table is private.
  (define dna (encode-dna (read-three in)))
  (define results (parallel-counts dna))
  (for ([len (in-list lengths)])
    (write-bytes (produce-row dna len (tables-for results len)) out))
  (void))

(module+ main (main))
