#lang racket/base

;; Benchmark and correctness test comparing get-info/full (eval-based)
;; vs get-info/full/read (read-based parser)

(require setup/getinfo
         "getinfo-read.rkt"
         racket/path
         racket/list)

;; --- Gather all info.rkt directories ---

(define (find-info-dirs root)
  (define results '())
  (let loop ([dir root])
    (when (directory-exists? dir)
      (define info-file (build-path dir "info.rkt"))
      (when (file-exists? info-file)
        (set! results (cons dir results)))
      (for ([entry (in-list (directory-list dir))])
        (define p (build-path dir entry))
        (when (directory-exists? p)
          (loop p)))))
  (reverse results))

(printf "Scanning for info.rkt files...\n")
(define all-dirs
  (append (find-info-dirs (build-path (current-directory) "pkgs"))
          (find-info-dirs (build-path (current-directory) "racket" "collects"))))

(printf "Found ~a info.rkt directories\n\n" (length all-dirs))

;; --- Correctness comparison ---

(printf "=== Correctness Comparison ===\n")
(define correct-count 0)
(define error-count 0)
(define skipped-count 0)

;; Common keys found in info.rkt files
(define common-keys
  '(collection deps build-deps pkg-desc pkg-authors version license
    implies name scribblings racket-launcher-names
    racket-launcher-libraries gracket-launcher-names
    gracket-launcher-libraries test-omit-paths
    compile-omit-paths compile-omit-files
    binary-keep-files binary-lib-files update-implies
    test-command-line-arguments test-timeouts
    test-responsibles test-randoms
    language-family purpose))

(for ([dir (in-list all-dirs)])
  (define info-old
    (with-handlers ([exn:fail? (lambda (e) #f)])
      (get-info/full dir)))
  (define info-new
    (with-handlers ([exn:fail? (lambda (e)
                                 (printf "  READ ERROR in ~a: ~a\n" dir (exn-message e))
                                 #f)])
      (get-info/full/read dir)))
  (cond
    [(and info-old info-new)
     (define mismatches '())
     (for ([key (in-list common-keys)])
       (define v-old (info-old key (lambda () 'NOT-FOUND)))
       (define v-new (info-new key (lambda () 'NOT-FOUND)))
       (unless (equal? v-old v-new)
         (set! mismatches (cons (list key v-old v-new) mismatches))))
     (if (null? mismatches)
         (set! correct-count (add1 correct-count))
         (begin
           (set! error-count (add1 error-count))
           (printf "  MISMATCH in ~a:\n" dir)
           (for ([m (in-list mismatches)])
             (printf "    key=~a old=~e new=~e\n" (car m) (cadr m) (caddr m)))))]
    [(and (not info-old) (not info-new))
     (set! skipped-count (add1 skipped-count))]
    [else
     (set! error-count (add1 error-count))
     (printf "  ONE FAILED in ~a: old=~a new=~a\n" dir (and info-old #t) (and info-new #t))]))

(printf "\nCorrectness results: ~a correct, ~a errors, ~a skipped\n\n"
        correct-count error-count skipped-count)

;; --- Performance benchmark ---

(printf "=== Performance Benchmark ===\n")
(define iterations 20)

;; Warm up filesystem cache
(for ([dir (in-list all-dirs)])
  (with-handlers ([exn:fail? void])
    (get-info/full/read dir)))

(printf "Benchmarking over ~a info files, ~a iterations each...\n\n" (length all-dirs) iterations)

;; Benchmark read-based parser
(collect-garbage)
(collect-garbage)
(collect-garbage)
(define t0-new (current-inexact-milliseconds))
(for ([_ (in-range iterations)])
  (for ([dir (in-list all-dirs)])
    (with-handlers ([exn:fail? void])
      (get-info/full/read dir))))
(define t1-new (current-inexact-milliseconds))
(define time-new (- t1-new t0-new))

;; Benchmark eval-based parser
(collect-garbage)
(collect-garbage)
(collect-garbage)
(define t0-old (current-inexact-milliseconds))
(for ([_ (in-range iterations)])
  (for ([dir (in-list all-dirs)])
    (with-handlers ([exn:fail? void])
      (get-info/full dir))))
(define t1-old (current-inexact-milliseconds))
(define time-old (- t1-old t0-old))

(define n (* iterations (length all-dirs)))
(printf "Read-based parser: ~a ms total (~a ms per file)\n"
        (real->decimal-string time-new 1)
        (real->decimal-string (/ time-new n) 3))
(printf "Eval-based parser: ~a ms total (~a ms per file)\n"
        (real->decimal-string time-old 1)
        (real->decimal-string (/ time-old n) 3))
(printf "Speedup: ~ax\n"
        (real->decimal-string (/ time-old time-new) 2))
