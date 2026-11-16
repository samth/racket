#lang racket/base
;; Usage: racket mem-after-load.rkt <module>
;; Instantiates <module>, collects garbage, and prints the memory still in use (bytes)
(define m (string->symbol (vector-ref (current-command-line-arguments) 0)))
(dynamic-require m #f)
(collect-garbage)
(collect-garbage)
(collect-garbage)
(printf "~a\n" (current-memory-use))
