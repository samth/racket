#lang racket/base
(require racket/unsafe/ops racket/fixnum)

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
       (define dna (get-output-bytes out))
       (for ([i (in-range (bytes-length dna))])
         (define b (bytes-ref dna i))
         (when (<= 97 b 122) (bytes-set! dna i (- b 32))))
       dna]
      [else (seek)])))

;; Current rules explicitly permit compact DNA codes. This control still
;; counts every fragment in every reading frame in a built-in hash table.
(define (encode-dna dna)
  (unless (<= (bytes-length dna) (- (most-positive-fixnum) 18))
    (error 'knucleotide "input too long for the x86-64 index fast path"))
  (define encoded (make-bytes (bytes-length dna)))
  (for ([b (in-bytes dna)] [i (in-naturals)])
    (bytes-set! encoded i
                (case b [(65) 0] [(67) 1] [(71) 2] [(84) 3]
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

(define (main [in (current-input-port)] [out (current-output-port)])
  (define dna (encode-dna (read-three in)))
  (parameterize ([current-output-port out])
    (write-frequencies (all-counts 1 dna) 1)
    (newline)
    (write-frequencies (all-counts 2 dna) 2)
    (newline)
    (for ([key '(#"GGT" #"GGTA" #"GGTATT" #"GGTATTTTAATT" #"GGTATTTTAATTTATAGT")])
      (define table (all-counts (bytes-length key) dna))
      (define counter (hash-ref table (encode-key key) #f))
      (printf "~a\t~a\n" (if counter (unbox counter) 0) key))))

(module+ main (main))
