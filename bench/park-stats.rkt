#lang racket/base
;; Run a benchmark module, then report the seqpark2b park counters
(require ffi/unsafe/vm
         racket/vector)
(define file (vector-ref (current-command-line-arguments) 0))
(dynamic-require (string->path file) #f)
(define (counter s)
  (vm-eval `(if (top-level-bound? ',s)
                (top-level-value ',s)
                0)))
(printf "parks ~a timeouts ~a late ~a\n" (counter (quote hash-lock-parks)) (counter (quote hash-lock-park-timeouts)) (counter (quote hash-lock-park-late)))
