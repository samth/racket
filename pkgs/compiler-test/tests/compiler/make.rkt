#lang racket/base
(require racket/file
         racket/port
         racket/path
         racket/system
         compiler/compilation-path
         compiler/find-exe)

(parameterize ((current-directory (find-system-path 'temp-dir)))
  (define tmpdir (make-temporary-file "tmp~a" 'directory (current-directory)))
  (define tmppath (build-path tmpdir "tmp.rkt"))
  (with-output-to-file (build-path tmpdir "tmp.rkt")  #:exists 'replace
                       (lambda ()
                         (printf "#lang racket/base\n")))
  (define exec-path (find-exe))
  (define relpath (find-relative-path (current-directory) tmppath))

  (define ok? (system* exec-path "-l" "raco" "make" "-j" "2" (path->string relpath)))
  (delete-directory/files tmpdir)

  (unless ok?
    (error "`raco make -j 2 tmp.rkt` test failed"))
  (unless (system* exec-path "-l" "raco" "make" "-l" "racket/base")
    (error "`raco make -l racket/base` test failed")))

(define (write-file p content)
  (call-with-output-file* p
    #:exists 'replace
    (lambda (out) (display content out))))

(define (run* exe . args)
  (define out (open-output-string))
  (define err (open-output-string))
  (define ok?
    (parameterize ([current-output-port out]
                   [current-error-port err])
      (apply system* exe args)))
  (values ok? (get-output-string out) (get-output-string err)))

(define (raco-make! exe . args)
  (define-values (ok? out err)
    (apply run* exe "-l" "raco" "make" args))
  (unless ok?
    (error 'raco-make "failed\n  args: ~s\n  stdout: ~a\n  stderr: ~a"
           args
           out
           err)))

(define (run-racket! exe . args)
  (define-values (ok? out err)
    (apply run* exe args))
  (unless ok?
    (error 'racket "failed\n  args: ~s\n  stdout: ~a\n  stderr: ~a"
           args
           out
           err)))

(define (dependency-file p)
  (path-replace-extension (get-compilation-bytecode-file p) #".dep"))

(define (read-dependencies p)
  (call-with-input-file* (dependency-file p) read))

(define (contains-path? v p)
  (cond
    [(bytes? v) (contains-path? (bytes->path v) p)]
    [(path? v) (equal? (path->complete-path v)
                       (path->complete-path p))]
    [(pair? v) (or (contains-path? (car v) p)
                   (contains-path? (cdr v) p))]
    [(vector? v) (for/or ([v (in-vector v)])
                   (contains-path? v p))]
    [else #f]))

(let ([tmpdir (make-temporary-file "drmpi~a" 'directory)])
  (define a-path (build-path tmpdir "a.rkt"))
  (define a-dynamic-path (build-path tmpdir "a-dynamic.rkt"))
  (define b-path (build-path tmpdir "b.rkt"))
  (define exec-path (find-exe))

  (write-file
   a-path
   "#lang racket/base\n\
    (require racket/runtime-path)\n\
    (define-runtime-module-path-index b-index \"b.rkt\")\n\
    (provide b-index)\n")
  (write-file
   a-dynamic-path
   "#lang racket/base\n\
    (require racket/runtime-path\n\
             (for-syntax racket/base))\n\
    (define-runtime-module-path-index b-index\n\
      (let ([p \"b.rkt\"]) p))\n\
    (provide b-index)\n")
  (write-file
   b-path
   "#lang racket/base\n\
    (error 'b \"instantiated\")\n\
    (provide value)\n\
    (define value 1)\n")

  (raco-make! exec-path (path->string a-path))
  (unless (contains-path? (read-dependencies a-path) b-path)
    (error 'define-runtime-module-path-index
           "expected compile dependency on ~s in ~s"
           b-path
           (dependency-file a-path)))
  (run-racket! exec-path (path->string a-path))

  (define before (file-or-directory-modify-seconds
                  (get-compilation-bytecode-file a-path)))
  (sleep 2)
  (write-file
   b-path
   "#lang racket/base\n\
    (error 'b \"instantiated\")\n\
    (provide value)\n\
    (define value 2)\n")
  (raco-make! exec-path (path->string a-path))
  (define after (file-or-directory-modify-seconds
                 (get-compilation-bytecode-file a-path)))
  (unless (> after before)
    (error 'define-runtime-module-path-index
           "expected changing ~s to recompile ~s"
           b-path
           a-path))

  (raco-make! exec-path (path->string a-dynamic-path))
  (when (contains-path? (read-dependencies a-dynamic-path) b-path)
    (error 'define-runtime-module-path-index
           "dynamic form unexpectedly created compile dependency on ~s"
           b-path))

  (delete-directory/files tmpdir))
