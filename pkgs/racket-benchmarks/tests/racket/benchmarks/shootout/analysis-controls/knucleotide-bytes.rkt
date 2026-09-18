#lang racket/base
(require racket/unsafe/ops)

;;; The Computer Language Benchmarks Game
;;; https://benchmarksgame-team.pages.debian.net/benchmarksgame/
;;; Based on the Racket program contributed by Matthew Flatt.
;;; Byte-key reuse, reading-frame traversal, THREE parsing and lexical ties.
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
       (define dna (get-output-bytes out))
       (for ([i (in-range (bytes-length dna))])
         (define b (bytes-ref dna i))
         (when (<= 97 b 122) (bytes-set! dna i (- b 32))))
       dna]
      [else (seek)])))

(define (count-frame! table dna len frame lookup-key)
  (for ([start (in-range frame (add1 (- (bytes-length dna) len)) len)])
    (unsafe-bytes-copy! lookup-key 0 dna start (+ start len))
    (define counter (hash-ref table lookup-key #f))
    (if counter
        (unsafe-set-box! counter (add1 (unsafe-unbox counter)))
        (hash-set! table (bytes->immutable-bytes lookup-key) (box 1)))))

(define (all-counts len dna)
  (define table (make-hash))
  (define lookup-key (make-bytes len))
  (for ([frame (in-range len)])
    (count-frame! table dna len frame lookup-key))
  table)

(define (write-frequencies table)
  (define entries (hash-map table (lambda (key count) (cons key (unbox count)))))
  (define total (for/sum ([entry (in-list entries)]) (cdr entry)))
  (define (before? a b)
    (or (> (cdr a) (cdr b))
        (and (= (cdr a) (cdr b)) (bytes<? (car a) (car b)))))
  (for ([entry (in-list (sort entries before?))])
    (printf "~a ~a\n" (car entry)
            (real->decimal-string (* 100.0 (/ (cdr entry) total)) 3))))

(define (main [in (current-input-port)] [out (current-output-port)])
  (define dna (read-three in))
  (parameterize ([current-output-port out])
    (write-frequencies (all-counts 1 dna))
    (newline)
    (write-frequencies (all-counts 2 dna))
    (newline)
    (for ([key '(#"GGT" #"GGTA" #"GGTATT" #"GGTATTTTAATT" #"GGTATTTTAATTTATAGT")])
      (define table (all-counts (bytes-length key) dna))
      (define counter (hash-ref table key #f))
      (printf "~a\t~a\n" (if counter (unbox counter) 0) key))))

(module+ main (main))
