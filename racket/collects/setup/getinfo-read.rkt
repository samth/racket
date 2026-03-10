#lang racket/base

;; A read-based parser for info.rkt files.
;; Instead of evaluating info.rkt as a Racket module, this reads
;; the S-expression and directly interprets the restricted language.
;;
;; Performance gains come from avoiding namespace creation, module
;; evaluation, and dynamic-require. The reader security features
;; (with-module-reading-parameterization and reader guard) are
;; preserved — they add negligible cost.

(require racket/match
         syntax/modread)

(provide get-info/full/read)

;; These `require's ensure that the `#lang info' readers are loaded,
;; so that no reader guard will be invoked for the reader itself.
(require (only-in setup/infotab)
         (only-in info)
         (only-in setup/infotab/lang/reader)
         (only-in (submod info reader)))

;; get-info/full/read : path -> info/#f
;; Returns an info procedure or #f if no info file exists.
(define (get-info/full/read dir)
  (or (get-info/full/read/ext dir "rkt")
      (get-info/full/read/ext dir "ss")))

(define (get-info/full/read/ext dir ext)
  (define file (build-path dir (format "info.~a" ext)))
  (define (err fmt . args)
    (apply error 'get-info/read (string-append "info file " fmt " in ~a")
           (append args (list file))))
  (define content
    (with-handlers ([exn:fail:filesystem:errno?
                     (lambda (e) #f)])
      (with-input-from-file file
        (lambda ()
          (parameterize ([current-reader-guard
                          (lambda (x)
                            (if (or (eq? x 'setup/infotab/lang/reader)
                                    (eq? x 'info/lang/reader)
                                    (equal? x '(submod setup/infotab reader))
                                    (equal? x '(submod info reader)))
                              x
                              (err "has illegal #lang or #reader")))])
            (begin0
              (with-module-reading-parameterization read)
              (unless (eof-object? (read))
                (err "has multiple expressions"))))))))
  (and content
       (parse-module-form content file err)))

;; parse-module-form : sexp path (string ... -> error) -> info-proc
;; Validates the module structure and parses the body.
(define (parse-module-form content file err)
  (define info-lang?
    (match-lambda
      [(or '(lib "infotab.rkt" "setup")
           '(lib "infotab.ss" "setup")
           '(lib "setup/infotab.rkt")
           '(lib "setup/infotab.ss")
           '(lib "main.rkt" "info")
           'setup/infotab
           'info)
       #t]
      [_ #f]))
  (match content
    [(list 'module 'info (? info-lang?) (list '#%module-begin defns ...))
     (parse-info-body defns file err)]
    [(list 'module 'info (? info-lang?) defns ...)
     (parse-info-body defns file err)]
    [_ (err "does not contain a module of the right shape")]))

;; parse-info-body : (listof sexp) path (string ... -> error) -> info-proc
;; Parses the define forms and returns an info lookup procedure.
(define (parse-info-body exprs file err)
  ;; Build a hash table from definitions
  (define ht
    (for/fold ([ht #hasheq()]) ([expr (in-list exprs)])
      (match expr
        [`(define ,id ,rhs)
         (unless (symbol? id)
           (err "expected identifier in define, got ~e" id))
         (when (hash-has-key? ht id)
           (err "duplicate definition for ~a" id))
         (hash-set ht id (interpret-expr rhs ht file))]
        [_ (err "expected define, got ~e" expr)])))
  ;; Return the info procedure
  (lambda (key [default (lambda () (error 'info.rkt "no info for ~a" key))])
    (unless (and (procedure? default)
                 (procedure-arity-includes? default 0))
      (error 'info.rkt
             "expected second argument to be a procedure that takes no arguments, got: ~e"
             default))
    (hash-ref ht key default)))

;; interpret-expr : sexp hash path -> value
;; Interprets an expression in the restricted info language.
(define (interpret-expr expr env file)
  (define (interp e) (interpret-expr e env file))
  (define (interp* es) (map interp es))
  (match expr
    ;; Literals
    [(? string?) expr]
    [(? number?) expr]
    [(? boolean?) expr]
    [(? bytes?) expr]
    [(? char?) expr]
    [(? regexp?) expr]
    [(? pregexp?) expr]
    [(? byte-regexp?) expr]
    [(? byte-pregexp?) expr]
    ;; Vectors
    [(? vector?) (for/vector ([e (in-vector expr)]) (interp e))]
    ;; Quote
    [`(quote ,datum) datum]
    ;; Quasiquote
    [`(quasiquote ,tmpl) (interpret-quasiquote tmpl env file)]
    ;; If
    [`(if ,test ,then ,else-branch)
     (if (interp test) (interp then) (interp else-branch))]
    ;; List constructors
    [`(list . ,args) (interp* args)]
    [`(list* ,args ... ,tail)
     (apply list* (append (interp* args) (list (interp tail))))]
    [`(cons ,a ,b) (cons (interp a) (interp b))]
    [`(car ,a) (car (interp a))]
    [`(cdr ,a) (cdr (interp a))]
    [`(append . ,args) (apply append (interp* args))]
    [`(reverse ,a) (reverse (interp a))]
    ;; Equality
    [`(equal? ,a ,b) (equal? (interp a) (interp b))]
    ;; Hash operations
    [`(make-immutable-hash ,a) (make-immutable-hash (interp a))]
    [`(make-immutable-hash) (make-immutable-hash)]
    [`(hash . ,args) (apply hash (interp* args))]
    [`(hash-set ,h ,k ,v) (hash-set (interp h) (interp k) (interp v))]
    [`(hash-set* ,h . ,kvs) (apply hash-set* (interp h) (interp* kvs))]
    [`(hash-remove ,h ,k) (hash-remove (interp h) (interp k))]
    [`(hash-clear ,h) (hash-clear (interp h))]
    [`(hash-update ,h ,k ,updater)
     (hash-update (interp h) (interp k) (interp updater))]
    [`(hash-update ,h ,k ,updater ,default)
     (hash-update (interp h) (interp k) (interp updater) (interp default))]
    ;; String operations
    [`(string-append . ,args) (apply string-append (interp* args))]
    ;; Path operations
    [`(path->string ,a) (path->string (interp a))]
    [`(build-path . ,args) (apply build-path (interp* args))]
    [`(collection-path . ,args) (apply collection-path (interp* args))]
    [`(system-library-subpath) (system-library-subpath)]
    [`(system-library-subpath ,a) (system-library-subpath (interp a))]
    ;; Environment
    [`(getenv ,a) (getenv (interp a))]
    ;; Variable reference
    [(? symbol? id)
     (hash-ref env id (lambda () (error 'info.rkt "undefined identifier: ~a in ~a" id file)))]
    [_ (error 'get-info/read "unsupported expression form: ~e in ~a" expr file)]))

;; interpret-quasiquote : sexp hash path -> value
;; Interprets a quasiquote template.
(define (interpret-quasiquote tmpl env file)
  (match tmpl
    [(list 'unquote e) (interpret-expr e env file)]
    [(cons 'unquote-splicing _)
     (error 'get-info/read "unquote-splicing not allowed outside list context in ~a" file)]
    [(cons a b)
     (let ([va (interpret-qq-element a env file)]
           [vb (interpret-quasiquote b env file)])
       (if (and (pair? va) (eq? (car va) 'spliced))
           (append (cdr va) vb)
           (cons va vb)))]
    [(? vector?)
     (list->vector (interpret-quasiquote (vector->list tmpl) env file))]
    [_ tmpl]))

;; Returns either a value or (cons 'spliced list) for splicing
(define (interpret-qq-element tmpl env file)
  (match tmpl
    [(list 'unquote e) (interpret-expr e env file)]
    [(list 'unquote-splicing e) (cons 'spliced (interpret-expr e env file))]
    [(cons a b)
     (let ([va (interpret-qq-element a env file)]
           [vb (interpret-quasiquote b env file)])
       (if (and (pair? va) (eq? (car va) 'spliced))
           (append (cdr va) vb)
           (cons va vb)))]
    [_ tmpl]))
