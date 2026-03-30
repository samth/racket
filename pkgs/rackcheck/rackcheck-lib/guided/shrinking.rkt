#lang racket/base

;; Shrinking for coverage-guided testing.
;;
;; When a failure is found via mutation (no shrink tree from rackcheck),
;; we use type-aware shrinking to minimize the input while preserving failure.

(require racket/contract/base)

(provide
 shrink-failing-input)

;; Shrink a failing input using type-aware strategies.
;; `input` is the value (possibly a list of argument values).
;; `fails?` should return #t if the input still causes failure.
;; `max-attempts` limits the total shrink attempts.
;; Returns the smallest failing input found.
(define (shrink-failing-input input fails? max-attempts)
  (let loop ([current input] [attempts 0])
    (cond
      [(>= attempts max-attempts) current]
      [else
       (define candidates (shrink-candidates current))
       (define found
         (for/first ([c (in-list candidates)]
                     #:when (with-handlers ([exn:fail? (lambda (_) #t)])
                              (fails? c)))
           c))
       (if found
           (loop found (add1 attempts))
           current)])))

;; Generate shrink candidates for a value.
(define (shrink-candidates val)
  (cond
    [(list? val) (shrink-list-candidates val)]
    [(exact-integer? val) (shrink-integer-candidates val)]
    [(string? val) (shrink-string-candidates val)]
    [(bytes? val) (shrink-bytes-candidates val)]
    [(boolean? val) (if val (list #f) '())]
    [(char? val) (shrink-char-candidates val)]
    [(vector? val)
     (map list->vector (shrink-list-candidates (vector->list val)))]
    [(pair? val)
     (append
      (map (lambda (c) (cons c (cdr val))) (shrink-candidates (car val)))
      (map (lambda (c) (cons (car val) c)) (shrink-candidates (cdr val))))]
    [else '()]))

;; Shrink an integer toward 0 using halving.
(define (shrink-integer-candidates n)
  (cond
    [(zero? n) '()]
    [(positive? n)
     (cons 0 (let loop ([delta (quotient n 2)])
               (if (zero? delta) '()
                   (cons (- n delta) (loop (quotient delta 2))))))]
    [else
     (cons 0 (cons (- n)
                   (let loop ([delta (quotient (abs n) 2)])
                     (if (zero? delta) '()
                         (cons (+ n delta) (loop (quotient delta 2)))))))]))

;; Shrink a string by removing characters.
(define (shrink-string-candidates s)
  (define len (string-length s))
  (cond
    [(zero? len) '()]
    [(= len 1) (list "")]
    [else
     (cons ""
           (append
            (for/list ([i (in-range len)])
              (string-append (substring s 0 i) (substring s (add1 i))))
            (list (substring s 0 (quotient len 2))
                  (substring s (quotient len 2)))))]))

;; Shrink bytes similarly to strings.
(define (shrink-bytes-candidates b)
  (define len (bytes-length b))
  (cond
    [(zero? len) '()]
    [(= len 1) (list #"")]
    [else
     (cons #""
           (append
            (for/list ([i (in-range len)])
              (bytes-append (subbytes b 0 i) (subbytes b (add1 i))))
            (list (subbytes b 0 (quotient len 2))
                  (subbytes b (quotient len 2)))))]))

;; Shrink a char toward #\nul.
(define (shrink-char-candidates c)
  (define n (char->integer c))
  (if (zero? n) '()
      (list #\nul (integer->char (quotient n 2)))))

;; Shrink a list by removing elements or shrinking individual elements.
(define (shrink-list-candidates lst)
  (define len (length lst))
  (cond
    [(null? lst) '()]
    [else
     (append
      (list '())
      ;; Remove chunks of decreasing size
      (let loop ([k (quotient len 2)])
        (cond
          [(zero? k) '()]
          [else (append (removes k len lst) (loop (quotient k 2)))]))
      ;; Shrink individual elements
      (for*/list ([i (in-range len)]
                  [c (in-list (shrink-candidates (list-ref lst i)))])
        (append (take-n lst i)
                (list c)
                (drop-n lst (add1 i)))))]))

(define (removes k n lst)
  (cond
    [(> k n) '()]
    [(= k n) (list '())]
    [else
     (define front (take-n lst k))
     (define back (drop-n lst k))
     (cons back
           (map (lambda (rest) (append front rest))
                (removes k (- n k) back)))]))

(define (take-n lst n)
  (cond [(or (zero? n) (null? lst)) '()]
        [else (cons (car lst) (take-n (cdr lst) (sub1 n)))]))

(define (drop-n lst n)
  (cond [(or (zero? n) (null? lst)) lst]
        [else (drop-n (cdr lst) (sub1 n))]))
