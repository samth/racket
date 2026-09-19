#lang racket/base
(require racket/unsafe/ops racket/fixnum racket/place)

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
  (define encoded (make-shared-bytes (bytes-length dna)))
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

(define (produce-row dna len)
  (define table (all-counts len dna))
  (define out (open-output-bytes))
  (parameterize ([current-output-port out])
    (if (<= len 2)
        (begin (write-frequencies table len) (newline))
        (let* ([key (case len
                      [(3) #"GGT"] [(4) #"GGTA"] [(6) #"GGTATT"]
                      [(12) #"GGTATTTTAATT"] [(18) #"GGTATTTTAATTTATAGT"])]
               [counter (hash-ref table (encode-key key) #f)])
          (printf "~a\t~a\n" (if counter (unbox counter) 0) key))))
  (get-output-bytes out))

(define (produce-rows dna lengths)
  (apply bytes-append
         (for/list ([len (in-list lengths)]) (produce-row dna len))))

(define (make-worker)
  (place channel
    (define message (place-channel-get channel))
    (place-channel-put channel (produce-rows (car message) (cadr message)))))

(define (main [in (current-input-port)] [out (current-output-port)])
  ;; The shared input is completely initialized before any worker receives it;
  ;; thereafter all four places only read it. Each hash table stays private.
  (define dna (encode-dna (read-three in)))
  (define workers
    (for/list ([lengths '((3 4 6) (12) (18))])
      (define worker (make-worker))
      (place-channel-put worker (list dna lengths))
      worker))
  (write-bytes (produce-rows dna '(1 2)) out)
  (for ([worker (in-list workers)])
    (write-bytes (place-channel-get worker) out)
    (place-wait worker))
  (void))

(module+ main (main))
