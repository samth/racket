#lang racket/base
;; Runs each program on the small official input and compares its output
;; with the expected output in "test-data". The programs are standalone
;; command-line programs, so each one runs in a fresh Racket process.
(require racket/runtime-path
         racket/system
         racket/port
         racket/file)

(define-runtime-path here ".")

;; program, command-line argument, input file (or #f), expected output
(define checks
  '(("nbody.rkt" "1000" #f "nbody-output.txt")
    ("spectralnorm.rkt" "100" #f "spectralnorm-output.txt")
    ("fannkuch-redux.rkt" "7" #f "fannkuchredux-output.txt")
    ("binarytrees.rkt" "10" #f "binarytrees-output.txt")
    ("pidigits.rkt" "30" #f "pidigits-output.txt")
    ("fasta.rkt" "1000" #f "fasta-output.txt")
    ("knucleotide.rkt" "0" "knucleotide-input.txt" "knucleotide-output.txt")
    ("mandelbrot.rkt" "200" #f "mandelbrot-output.txt")
    ("regexredux.rkt" "0" "regexredux-input.txt" "regexredux-output.txt")
    ("revcomp.rkt" "0" "revcomp-input.txt" "revcomp-output.txt")))

(define racket (find-system-path 'exec-file))
(define test-data (build-path here "test-data"))

(define (run-one program arg input)
  (define out (open-output-bytes))
  (define ok?
    (parameterize ([current-output-port out]
                   [current-input-port (if input
                                           (open-input-file (build-path test-data input))
                                           (open-input-bytes #""))])
      (system* racket (build-path here program) arg)))
  (values ok? (get-output-bytes out)))

(define failures
  (for/sum ([c (in-list checks)])
    (define-values (program arg input expected) (apply values c))
    (cond
      [(and (equal? program "pidigits.rkt")
            (not (with-handlers ([exn:fail? (lambda (e) #f)])
                   ((dynamic-require 'ffi/unsafe 'ffi-lib) "libgmp" '("10" #f)))))
       (printf "SKIP ~a (no GMP)\n" program)
       0]
      [else
       (define-values (ok? actual) (run-one program arg input))
       (cond
         [(and ok? (equal? actual (file->bytes (build-path test-data expected))))
          (printf "PASS ~a\n" program)
          0]
         [else
          (printf "FAIL ~a\n" program)
          1])])))

(unless (zero? failures)
  (error 'check "~a program(s) failed" failures))
