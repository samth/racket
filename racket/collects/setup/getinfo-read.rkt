#lang racket/base

;; A read-based parser for info.rkt files.
;; Instead of evaluating info.rkt as a Racket module, this reads
;; the S-expression and directly interprets the restricted language.
;;
;; Performance gains come from avoiding namespace creation, module
;; evaluation, and dynamic-require. The reader security features
;; (with-module-reading-parameterization and reader guard) are
;; preserved — they add negligible cost.

(require syntax/modread)

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
  (and (file-exists? file)
       (let ()
         (define content
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
                     (err "has multiple expressions")))))))
         (parse-module-form content file err))))

;; parse-module-form : sexp path (string ... -> error) -> info-proc
;; Validates the module structure and parses the body.
(define (parse-module-form content file err)
  (unless (and (pair? content)
               (eq? (car content) 'module)
               (pair? (cdr content))
               (eq? (cadr content) 'info)
               (pair? (cddr content))
               (pair? (cdddr content)))
    (err "does not contain a module of the right shape"))
  (define lang (caddr content))
  (unless (or (equal? lang '(lib "infotab.rkt" "setup"))
              (equal? lang '(lib "infotab.ss" "setup"))
              (equal? lang '(lib "setup/infotab.rkt"))
              (equal? lang '(lib "setup/infotab.ss"))
              (equal? lang '(lib "main.rkt" "info"))
              (eq? lang 'setup/infotab)
              (eq? lang 'info))
    (err "does not contain a module of the right shape"))
  (define body (cdddr content))
  ;; Extract body from #%module-begin
  (unless (and (pair? body)
               (= 1 (length body))
               (pair? (car body))
               (eq? '#%module-begin (caar body)))
    (err "unexpected module body shape"))
  (define defns (cdar body))
  (parse-info-body defns file err))

;; parse-info-body : (listof sexp) path (string ... -> error) -> info-proc
;; Parses the define forms and returns an info lookup procedure.
(define (parse-info-body exprs file err)
  ;; Build a hash table from definitions
  (define ht
    (let loop ([exprs exprs] [ht #hasheq()])
      (if (null? exprs)
          ht
          (let ([expr (car exprs)])
            (if (and (pair? expr) (eq? (car expr) 'define)
                     (pair? (cdr expr)) (pair? (cddr expr))
                     (null? (cdddr expr)))
                (let ([id (cadr expr)]
                      [rhs (caddr expr)])
                  (unless (symbol? id)
                    (err "expected identifier in define, got ~e" id))
                  (when (hash-has-key? ht id)
                    (err "duplicate definition for ~a" id))
                  (loop (cdr exprs)
                        (hash-set ht id (interpret-expr rhs ht file))))
                (err "expected define, got ~e" expr))))))
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
;; Uses cond/eq? dispatch ordered by frequency for performance.
(define (interpret-expr expr env file)
  (cond
    ;; String literals (most common RHS type)
    [(string? expr) expr]
    ;; Compound expressions
    [(pair? expr)
     (let ([head (car expr)])
       (cond
         ;; quote — very common (quoted lists/symbols)
         [(eq? head 'quote) (cadr expr)]
         ;; list constructor
         [(eq? head 'list)
          (interpret-args (cdr expr) env file)]
         ;; quasiquote
         [(eq? head 'quasiquote)
          (interpret-quasiquote (cadr expr) env file)]
         ;; hash constructor
         [(eq? head 'hash)
          (apply hash (interpret-args (cdr expr) env file))]
         ;; cons
         [(eq? head 'cons)
          (cons (interpret-expr (cadr expr) env file)
                (interpret-expr (caddr expr) env file))]
         ;; list*
         [(eq? head 'list*)
          (let loop ([args (cdr expr)])
            (if (null? (cdr args))
                (interpret-expr (car args) env file)
                (cons (interpret-expr (car args) env file)
                      (loop (cdr args)))))]
         ;; append
         [(eq? head 'append)
          (apply append (interpret-args (cdr expr) env file))]
         ;; reverse
         [(eq? head 'reverse)
          (reverse (interpret-expr (cadr expr) env file))]
         ;; car/cdr
         [(eq? head 'car)
          (car (interpret-expr (cadr expr) env file))]
         [(eq? head 'cdr)
          (cdr (interpret-expr (cadr expr) env file))]
         ;; if
         [(eq? head 'if)
          (if (interpret-expr (cadr expr) env file)
              (interpret-expr (caddr expr) env file)
              (interpret-expr (cadddr expr) env file))]
         ;; equal?
         [(eq? head 'equal?)
          (equal? (interpret-expr (cadr expr) env file)
                  (interpret-expr (caddr expr) env file))]
         ;; Hash operations
         [(eq? head 'make-immutable-hash)
          (if (null? (cdr expr))
              (make-immutable-hash)
              (make-immutable-hash (interpret-expr (cadr expr) env file)))]
         [(eq? head 'hash-set)
          (hash-set (interpret-expr (cadr expr) env file)
                    (interpret-expr (caddr expr) env file)
                    (interpret-expr (cadddr expr) env file))]
         [(eq? head 'hash-set*)
          (apply hash-set* (interpret-expr (cadr expr) env file)
                 (interpret-args (cddr expr) env file))]
         [(eq? head 'hash-remove)
          (hash-remove (interpret-expr (cadr expr) env file)
                       (interpret-expr (caddr expr) env file))]
         [(eq? head 'hash-clear)
          (hash-clear (interpret-expr (cadr expr) env file))]
         [(eq? head 'hash-update)
          (if (null? (cddddr expr))
              (hash-update (interpret-expr (cadr expr) env file)
                           (interpret-expr (caddr expr) env file)
                           (interpret-expr (cadddr expr) env file))
              (hash-update (interpret-expr (cadr expr) env file)
                           (interpret-expr (caddr expr) env file)
                           (interpret-expr (cadddr expr) env file)
                           (interpret-expr (car (cddddr expr)) env file)))]
         ;; String operations
         [(eq? head 'string-append)
          (apply string-append (interpret-args (cdr expr) env file))]
         ;; Path operations
         [(eq? head 'path->string)
          (path->string (interpret-expr (cadr expr) env file))]
         [(eq? head 'build-path)
          (apply build-path (interpret-args (cdr expr) env file))]
         [(eq? head 'collection-path)
          (apply collection-path (interpret-args (cdr expr) env file))]
         [(eq? head 'system-library-subpath)
          (if (null? (cdr expr))
              (system-library-subpath)
              (system-library-subpath (interpret-expr (cadr expr) env file)))]
         ;; Environment
         [(eq? head 'getenv)
          (getenv (interpret-expr (cadr expr) env file))]
         [else
          (error 'get-info/read "unsupported expression form: ~e in ~a" expr file)]))]
    ;; Numbers
    [(number? expr) expr]
    ;; Booleans
    [(boolean? expr) expr]
    ;; Symbol (variable reference)
    [(symbol? expr)
     (hash-ref env expr
               (lambda () (error 'info.rkt "undefined identifier: ~a in ~a" expr file)))]
    ;; Other literal types
    [(bytes? expr) expr]
    [(char? expr) expr]
    [(regexp? expr) expr]
    [(pregexp? expr) expr]
    [(byte-regexp? expr) expr]
    [(byte-pregexp? expr) expr]
    ;; Vectors
    [(vector? expr)
     (for/vector ([e (in-vector expr)]) (interpret-expr e env file))]
    [else
     (error 'get-info/read "unsupported expression form: ~e in ~a" expr file)]))

;; interpret-args : (listof sexp) hash path -> (listof value)
;; Interprets a list of argument expressions.
(define (interpret-args args env file)
  (let loop ([args args] [acc '()])
    (if (null? args)
        (reverse acc)
        (loop (cdr args)
              (cons (interpret-expr (car args) env file) acc)))))

;; interpret-quasiquote : sexp hash path -> value
;; Interprets a quasiquote template.
(define (interpret-quasiquote tmpl env file)
  (cond
    [(and (pair? tmpl) (eq? (car tmpl) 'unquote)
          (pair? (cdr tmpl)) (null? (cddr tmpl)))
     (interpret-expr (cadr tmpl) env file)]
    [(and (pair? tmpl) (eq? (car tmpl) 'unquote-splicing))
     (error 'get-info/read "unquote-splicing not allowed outside list context in ~a" file)]
    [(pair? tmpl)
     (let ([va (interpret-qq-element (car tmpl) env file)]
           [vb (interpret-quasiquote (cdr tmpl) env file)])
       (if (and (pair? va) (eq? (car va) 'spliced))
           (append (cdr va) vb)
           (cons va vb)))]
    [(vector? tmpl)
     (list->vector
      (interpret-quasiquote (vector->list tmpl) env file))]
    [else tmpl]))

;; Returns either a value or (cons 'spliced list) for splicing
(define (interpret-qq-element tmpl env file)
  (cond
    [(and (pair? tmpl) (eq? (car tmpl) 'unquote)
          (pair? (cdr tmpl)) (null? (cddr tmpl)))
     (interpret-expr (cadr tmpl) env file)]
    [(and (pair? tmpl) (eq? (car tmpl) 'unquote-splicing)
          (pair? (cdr tmpl)) (null? (cddr tmpl)))
     (cons 'spliced (interpret-expr (cadr tmpl) env file))]
    [(pair? tmpl)
     (let ([va (interpret-qq-element (car tmpl) env file)]
           [vb (interpret-quasiquote (cdr tmpl) env file)])
       (if (and (pair? va) (eq? (car va) 'spliced))
           (append (cdr va) vb)
           (cons va vb)))]
    [else tmpl]))
