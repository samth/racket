#lang info

;; The programs are standalone command-line programs that read arguments
;; and standard input, so don't run them as tests directly. "check.rkt"
;; runs each of them on a small input instead.
(define test-omit-paths '("binarytrees.rkt" "fannkuch-redux.rkt" "fasta.rkt"
                          "knucleotide.rkt" "mandelbrot.rkt" "nbody.rkt"
                          "pidigits.rkt" "regexredux.rkt" "revcomp.rkt"
                          "spectralnorm.rkt"))
