#lang racket
(require tests/eli-tester
         racket/file
         racket/runtime-path
         compiler/find-exe
         racket/cmdline)

(define fast? #f)
(command-line
 #:once-each
 [("--fast") "Skip slower tests"
             (set! fast? #t)]
 #:args ()
 (void))

(define-runtime-path tests "tests")

(define (slow-test? i)
  (case (path->string i)
    [("racket-5.rkt") #t]
    [else #f]))

(define (non-base-test? i)
  (case (path->string i)
    [("kernel-5.rkt") #t]
    [else #f]))

(define (get-pruned-expected i)
  (case (path->string i)
    [("base-effect-defn.rkt")
     "\"result\"\n"]
    [("base-assign.rkt")
     "used!\n\"stayed\"\n"]
    [else
     ;; #f means "same as non-pruned"
     #f]))

(define (capture-output command . args)
  (define o (open-output-string))
  (define e (open-output-string))
  (parameterize ([current-input-port (open-input-string "")]
                 [current-output-port o]
                 [current-error-port e])
    (apply system* command args))
  (values (get-output-string o) (get-output-string e)))

(define (test-on-program filename
                         #:flags [flags null]
                         #:excludes [exceptions null]
                         #:expected-output [expected-output #f])
  (define desc (string-join(append flags
                                   exceptions
                                   (list filename))))
  (printf "Checking ~a\n" desc)

  ;; run modular program, capture output
  (define-values (modular-output modular-error)
    (capture-output (find-exe) filename))
  
  (define demod-filename 
    (let-values ([(base filename dir?) (split-path filename)])
      (path->string
       (build-path
        (find-system-path 'temp-dir)
        (path-add-suffix filename #"_merged.zo")))))
  
  ;; demodularize
  (parameterize ([current-input-port (open-input-string "")])
    (apply system* (find-exe) "-l-" "raco" "demod" "-o" demod-filename
           "--work" (build-path tests "compiled" "demod")
           (append flags
                   exceptions
                   (list filename))))
  
  ;; run whole program
  (define-values (whole-output whole-error)
    (capture-output (find-exe) demod-filename))
  
  ;; compare output 
  (test
   #:failure-prefix (format "~a stdout" desc)
   whole-output => (or expected-output
                       modular-output)
   #:failure-prefix (format "~a stderr" desc)
   whole-error => modular-error)

  (when (null? exceptions)
    ;; try creating an executable
    (define exe-filename (build-path
                          (find-system-path 'temp-dir)
                          (if (eq? (system-type) 'windows)
                              "demod-exe.exe"
                              "demod-exe")))
    (system* (find-exe) "-l-" "raco" "exe" "-o" exe-filename demod-filename)
    (define-values (whole-exe-output whole-exe-error)
      (capture-output exe-filename))
    (test
     #:failure-prefix (format "~a exe stdout" desc)
     whole-exe-output => (or expected-output
                             modular-output)
     #:failure-prefix (format "~a exe stderr" desc)
     whole-exe-error => modular-error)))

(define (modular-program? filename)
  (and (not (regexp-match #rx"merged" filename))
       (regexp-match #rx"rkt$" filename)))

(define (test-issue-5465)
  (define dir (make-temporary-file "demod-issue-5465-~a" 'directory))
  (define demod-file (build-path dir "demod.rkt"))
  (define exe-file (build-path dir
                               (if (eq? (system-type) 'windows)
                                   "demod-issue-5465.exe"
                                   "demod-issue-5465")))
  (write-to-file '(module demod compiler/demod
                    "main.rkt"
                    #:dynamic
                    #:exclude
                    (#:module "structs.rkt"))
                 demod-file)
  (write-to-file '(module main racket/base
                    (require "mystructs.rkt"
                             "structs.rkt")
                    (displayln (thing 1 2))
                    (displayln (widget "a" 3)))
                 (build-path dir "main.rkt"))
  (write-to-file '(module mystructs racket/base
                    (require racket/serialize)
                    (provide (struct-out thing))
                    (serializable-struct thing (x y)))
                 (build-path dir "mystructs.rkt"))
  (write-to-file '(module structs racket/base
                    (require racket/serialize)
                    (provide (struct-out widget))
                    (serializable-struct widget (name value)))
                 (build-path dir "structs.rkt"))
  (parameterize ([current-input-port (open-input-string "")]
                 [current-directory dir])
    (unless (system* (find-exe) "-l-" "raco" "make" "demod.rkt")
      (error 'demod-test "issue 5465: raco make failed"))
    (unless (system* (find-exe) "-l-" "raco" "exe" "-o" exe-file "demod.rkt")
      (error 'demod-test "issue 5465: raco exe failed")))
  (define-values (out err)
    (capture-output exe-file))
  (test
   #:failure-prefix "issue 5465 stdout"
   out => "#<thing>\n#<widget>\n"
   #:failure-prefix "issue 5465 stderr"
   err => ""))

(test
 (for ([i (in-list (directory-list tests))]
       #:when (and (regexp-match? #rx"[.]rkt$" i)
                   (or (not fast?)
                       (not (slow-test? i)))))
   (define ip (build-path tests i))
   (define keep-syntax? (regexp-match? #rx"-lib" i))
   (define syntax-flags (if keep-syntax? '("-s") '()))
   (when (modular-program? ip)
     (test-on-program (path->string ip)
                      #:flags syntax-flags))
     (test-on-program (path->string ip)
                      #:flags (append syntax-flags '("-g"))
                      #:expected-output (get-pruned-expected i))
     (unless (non-base-test? i)
       (test-on-program (path->string ip)
                        #:flags syntax-flags
                        #:excludes
                        (list "-e"
                              (path->string
                               (collection-file-path "pre-base.rkt" "racket/private"))))))
 (test-issue-5465))

(module+ test
  (module config info
    (define timeout 600)))
