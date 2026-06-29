#lang racket/base

;; Minimal reproducer for the Windows iconv boundary regression.
;; Run with `racket iconv-partial-boundary.rkt`.
;; On platforms where iconv treats an incomplete UTF-8 sequence as
;; needing more input, the status will be 'aborts. On the Windows
;; Server 2025 builders, the status is 'error, which feeds the wrong
;; branch in `reencode-input-port`.

(require racket/port)

(define sample #"ap\303\251ple")

(define (convert-prefix prefix-end)
  (define conv (bytes-open-converter "UTF-8" "UTF-8"))
  (define dest (make-bytes 8 0))
  (define-values (produced consumed status)
    (bytes-convert conv sample 0 prefix-end dest 0 (bytes-length dest)))
  (bytes-close-converter conv)
  (values (subbytes dest 0 produced) consumed status))

(define-values (fragment consumed status) (convert-prefix 3))

(printf "partial fragment ~s consumed ~a status ~s\n"
        fragment consumed status)

(unless (eq? status 'aborts)
  (printf "expected status 'aborts; Windows iconv currently reports ~s instead\n"
          status))

(when (eq? status 'aborts)
  (define-values (rest-fragment rest-consumed rest-status)
    (let ([conv (bytes-open-converter "UTF-8" "UTF-8")]
          [dest (make-bytes 8 0)])
      ;; Reset and convert the whole buffer to demonstrate success.
      (define-values (produced consumed status)
        (bytes-convert conv sample 0 (bytes-length sample) dest 0 (bytes-length dest)))
      (bytes-close-converter conv)
      (values (subbytes dest 0 produced) consumed status)))
  (printf "full conversion fragment ~s consumed ~a status ~s\n"
          rest-fragment rest-consumed rest-status))
