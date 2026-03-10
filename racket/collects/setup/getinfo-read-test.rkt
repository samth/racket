#lang racket/base

;; Exhaustive correctness test: for every info.rkt in the installed
;; Racket tree, extract the actual defined keys from the source,
;; then compare the eval-based and read-based parsers on every key.

(require setup/getinfo
         "getinfo-read.rkt"
         syntax/modread
         racket/match
         racket/path
         racket/list
         racket/string)

;; --- Extract defined keys from an info.rkt by reading its source ---

(define (extract-keys-from-file file)
  ;; Read the module form and pull out the define'd identifiers
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (define content
      (with-input-from-file file
        (lambda ()
          (with-module-reading-parameterization read))))
    (match content
      [`(module info ,_lang (#%module-begin ,body ...))
       (for/list ([form (in-list body)]
                  #:when (match form
                           [`(define ,id ,_) (symbol? id)]
                           [_ #f]))
         (match form [`(define ,id ,_) id]))]
      [_ #f])))

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
  (append
   ;; Source tree
   (find-info-dirs (build-path (current-directory) "pkgs"))
   (find-info-dirs (build-path (current-directory) "racket" "collects"))
   ;; Installed Racket
   (find-info-dirs (build-path "/home/user/racket-9.1" "collects"))
   (find-info-dirs (build-path "/home/user/racket-9.1" "share" "pkgs"))))

;; Deduplicate by resolved path
(define seen (make-hash))
(define deduped-dirs
  (for/list ([dir (in-list all-dirs)]
             #:unless (let ([rp (simplify-path (path->complete-path dir))])
                        (begin0 (hash-has-key? seen rp)
                          (hash-set! seen rp #t))))
    dir))

(printf "Found ~a unique info.rkt directories\n\n" (length deduped-dirs))

;; --- Exhaustive correctness comparison ---

(printf "=== Exhaustive Correctness Test ===\n")
(printf "Testing every defined key in every info.rkt...\n\n")

(define total-files 0)
(define total-keys-tested 0)
(define correct-files 0)
(define error-files 0)
(define skipped-files 0)
(define mismatch-details '())  ; list of (dir key old new)

(for ([dir (in-list deduped-dirs)])
  (set! total-files (add1 total-files))
  (define info-file (build-path dir "info.rkt"))
  (define keys (extract-keys-from-file info-file))

  (cond
    [(not keys)
     ;; Can't extract keys (unusual #lang, etc.) — skip
     (set! skipped-files (add1 skipped-files))]
    [else
     (define info-old
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (get-info/full dir)))
     (define info-new
       (with-handlers ([exn:fail? (lambda (e)
                                    (printf "  READ-PARSER ERROR in ~a:\n    ~a\n"
                                            dir (exn-message e))
                                    #f)])
         (get-info/full/read dir)))
     (cond
       [(and info-old info-new)
        (define file-ok? #t)
        (for ([key (in-list keys)])
          (set! total-keys-tested (add1 total-keys-tested))
          (define v-old
            (with-handlers ([exn:fail? (lambda (e) (list 'ERROR (exn-message e)))])
              (info-old key (lambda () 'NOT-FOUND))))
          (define v-new
            (with-handlers ([exn:fail? (lambda (e) (list 'ERROR (exn-message e)))])
              (info-new key (lambda () 'NOT-FOUND))))
          (unless (equal? v-old v-new)
            (set! file-ok? #f)
            (set! mismatch-details
                  (cons (list dir key v-old v-new) mismatch-details))
            (printf "  MISMATCH ~a key=~a\n    eval: ~e\n    read: ~e\n"
                    dir key v-old v-new)))
        (if file-ok?
            (set! correct-files (add1 correct-files))
            (set! error-files (add1 error-files)))]
       [(and (not info-old) (not info-new))
        ;; Both fail — consistent
        (set! skipped-files (add1 skipped-files))]
       [else
        (set! error-files (add1 error-files))
        (printf "  ONE-SIDED FAILURE in ~a: eval=~a read=~a\n"
                dir (and info-old #t) (and info-new #t))])]))

(printf "\n=== Results ===\n")
(printf "Files tested:  ~a\n" total-files)
(printf "Keys tested:   ~a\n" total-keys-tested)
(printf "Files correct: ~a\n" correct-files)
(printf "Files with mismatches: ~a\n" error-files)
(printf "Files skipped: ~a\n" skipped-files)
(printf "Total key mismatches: ~a\n" (length mismatch-details))

(when (> error-files 0)
  (printf "\n=== Mismatch Summary ===\n")
  (for ([detail (in-list (reverse mismatch-details))])
    (match-define (list dir key v-old v-new) detail)
    (printf "  ~a : ~a\n" (path->string dir) key)))

(when (= error-files 0)
  (printf "\nAll ~a keys across ~a files match perfectly.\n"
          total-keys-tested correct-files))
