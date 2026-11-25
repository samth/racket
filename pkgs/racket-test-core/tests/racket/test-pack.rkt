#lang racket/base
(require racket/file
         racket/system
         compiler/find-exe
         setup/dirs)

;; "pack.rktl" installs a collection into the main and user collection
;; directories and writes files with fixed names in the temporary
;; directory, so run it in a separate process whose main collection
;; directory, home directory, and temporary directory are fresh
;; directories, keeping other tests (such as the pkg tests, which run
;; `raco setup`) from seeing its changes

(define tmp-dir (make-temporary-directory "test-pack~a"))

(define (make-subdirectory name)
  (define dir (build-path tmp-dir name))
  (make-directory dir)
  dir)

(dynamic-wind
 void
 (lambda ()
   (define collects-dir (make-subdirectory "collects"))
   ;; An archive is unpacked into the main installation only if these
   ;; directories, which are found relative to the main collection
   ;; directory, are writable:
   (for-each make-subdirectory '("doc" "include" "lib"))
   (define config-dir (make-subdirectory "etc"))
   (define home-dir (make-subdirectory "home"))
   (define temp-dir (make-subdirectory "tmp"))
   ;; Without a configured "bin" directory, it would be found relative
   ;; to the new main collection directory, but "pack.rktl" runs `raco`
   ;; from there
   (call-with-output-file*
    (build-path config-dir "config.rktd")
    (lambda (o)
      (write (hash 'bin-dir (path->string (find-console-bin-dir))) o)))
   (define ok?
     (parameterize ([current-environment-variables
                     (environment-variables-copy (current-environment-variables))])
       (putenv "PLTCONFIGDIR" (path->string config-dir))
       ;; So that `raco` run by "pack.rktl" finds collections installed
       ;; in the new main collection directory, followed by the usual ones:
       (putenv "PLTCOLLECTS" (string-append (path->string collects-dir)
                                            (if (eq? (system-type) 'windows) ";" ":")))
       (putenv "PLTUSERHOME" (path->string home-dir))
       (putenv "TMPDIR" (path->string temp-dir))
       (system* (find-exe)
                ;; the main collection directory is empty, and the
                ;; installation's collections are found after it:
                "-X" collects-dir
                "-S" (find-collects-dir)
                "-f" (collection-file-path "pack.rktl" "tests/racket"))))
   (unless ok?
     (error 'test-pack "\"pack.rktl\" failed")))
 (lambda ()
   (delete-directory/files tmp-dir)))
