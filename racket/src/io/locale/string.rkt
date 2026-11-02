#lang racket/base
(require racket/fixnum
         "../common/check.rkt"
         "../string/convert.rkt"
         "../string/utf-8-decode.rkt"
         "../converter/main.rkt"
         "cache.rkt"
         "parameter.rkt"
         "ucs-4.rkt")

(provide string->bytes/locale
         bytes->string/locale)

(define/who (string->bytes/locale str [err-byte #f] [start 0] [end (and (string? str) (string-length str))])
  (check who string? str)
  (check who byte? #:or-false err-byte)
  (check who exact-nonnegative-integer? start)
  (check who exact-nonnegative-integer? end)
  (check-range who start end (string-length str) str)
  (cond
    [(and (string-ascii? str start end)
          (locale-encoding-is-ascii-compatible?))
     ;; Every ASCII-compatible encoding, including UTF-8, maps these
     ;; characters to the same bytes
     (string->bytes/latin-1 str #f start end)]
    [(locale-encoding-is-utf-8?)
     (string->bytes/utf-8 str err-byte start end)]
    [else
     (define c #f)
     (define enc (locale-string-encoding))
     (dynamic-wind
      (lambda ()
        (set! c (bytes-open-converter/cached-to enc)))
      (lambda ()
        (define in-bstr (string->bytes/ucs-4 str start end))
        (let loop ([pos 0])
          (define-values (bstr in-used status)
            (bytes-convert c in-bstr pos))
          (cond
            [(eq? status 'complete)
             (if (eqv? pos 0)
                 bstr
                 (list bstr))]
            [(not err-byte)
             (raise-arguments-error who "string cannot be encoded for the current locale"
                                    "string" str)]
            [else
             (define err-bstr (bytes err-byte))
             (cond
               [(eq? status 'aborts)
                (if (eqv? pos 0)
                    (bytes-append bstr err-bstr)
                    (list bstr err-bstr))]
               [else
                ;; Skip the next character; we're assuming that
                ;; `in-used` is a multiple of 4
                (define r (loop (+ pos in-used 4)))
                (if (eqv? pos 0)
                    (apply bytes-append (cons bstr (cons err-bstr r)))
                    (cons bstr (cons err-bstr r)))])])))
      (lambda ()
        (bytes-close-converter/cached-to c enc)))]))

(define/who (bytes->string/locale in-bstr [err-char #f] [start 0] [end (and (bytes? in-bstr)
                                                                            (bytes-length in-bstr))])
  (check who bytes? in-bstr)
  (check who char? #:or-false err-char)
  (check who exact-nonnegative-integer? start)
  (check who exact-nonnegative-integer? end)
  (check-range who start end (bytes-length in-bstr) in-bstr)
  (cond
    [(and (eqv? start 0)
          (eqv? end (bytes-length in-bstr))
          (bytes-ascii? in-bstr)
          (locale-encoding-is-ascii-compatible?))
     ;; Every ASCII-compatible encoding, including UTF-8, maps these
     ;; bytes to the same characters
     (bytes->string/latin-1 in-bstr)]
    [(locale-encoding-is-utf-8?)
     (bytes->string/utf-8 in-bstr err-char start end)]
    [else
     (define c #f)
     (define enc (locale-string-encoding))
     (dynamic-wind
      (lambda ()
        (set! c (bytes-open-converter/cached-from enc)))
      (lambda ()
        (let loop ([pos 0])
          (define-values (bstr in-used status)
            (bytes-convert c in-bstr pos))
          (cond
            [(eq? status 'complete)
             (if (eqv? pos 0)
                 (bytes->string/utf-8 bstr)
                 (list bstr))]
            [(not err-char)
             (raise-arguments-error who "byte string is not a valid encoding for the current locale"
                                    "byte string" in-bstr)]
            [else
             
             (define err-bstr (string->bytes/utf-8 (string err-char)))
             (cond
               [(eq? status 'aborts)
                (if (eqv? pos 0)
                    (bytes->string/utf-8 (bytes-append bstr err-bstr))
                    (list bstr err-bstr))]
               [else
                ;; Skip the byte and try again
                (define r (loop (+ pos in-used 1)))
                (if (eqv? pos 0)
                    (bytes->string/utf-8 (apply bytes-append (cons bstr (cons err-bstr r))))
                    (cons bstr (cons err-bstr r)))])])))
      (lambda ()
        (bytes-close-converter/cached-from c enc)))]))

;; ----------------------------------------

(define (string-ascii? str start end)
  (let loop ([i start])
    (or (fx= i end)
        (and (fx< (char->integer (string-ref str i)) 128)
             (loop (fx+ i 1))))))

(define (bytes-ascii? bstr)
  (define len (bytes-length bstr))
  (let loop ([i 0])
    (or (fx= i len)
        (and (fx< (bytes-ref bstr i) 128)
             (loop (fx+ i 1))))))
