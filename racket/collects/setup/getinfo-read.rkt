#lang racket/base

;; A read-based parser for info.rkt files.
;; Instead of evaluating info.rkt as a Racket module, this reads
;; the S-expression and directly interprets the restricted language.

(require racket/match
         syntax/modread)

(provide get-info/full/read)

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
         (match content
           [(list 'module 'info
                  (or '(lib "infotab.rkt" "setup")
                      '(lib "infotab.ss" "setup")
                      '(lib "setup/infotab.rkt")
                      '(lib "setup/infotab.ss")
                      '(lib "main.rkt" "info")
                      'setup/infotab
                      'info)
                  expr ...)
            (parse-info-body expr file err)]
           [_ (err "does not contain a module of the right shape")]))))

;; parse-info-body : (listof sexp) path (string ... -> error) -> info-proc
;; Parses the module body and returns an info lookup procedure.
(define (parse-info-body exprs file err)
  (define body-exprs
    (match exprs
      [(list (list '#%module-begin defns ...))
       defns]
      [_ (err "unexpected module body shape")]))
  ;; Build a hash table from definitions
  (define ht
    (for/fold ([ht #hasheq()]) ([expr (in-list body-exprs)])
      (match expr
        [`(define ,id ,rhs)
         (unless (symbol? id)
           (err "expected identifier in define, got ~e" id))
         (when (hash-has-key? ht id)
           (err "duplicate definition for ~a" id))
         (hash-set ht id (interpret-expr rhs ht file))]
        [_ (err "expected define, got ~e" expr)])))
  ;; Return the info procedure
  (case-lambda
    [(key) ((get-info-proc ht) key)]
    [(key default)
     ((get-info-proc ht) key default)]))

(define (get-info-proc ht)
  (case-lambda
    [(key) (hash-ref ht key (lambda () (error 'info.rkt "no info for ~a" key)))]
    [(key default)
     (unless (and (procedure? default)
                  (procedure-arity-includes? default 0))
       (error 'info.rkt
              "expected second argument to be a procedure that takes no arguments, got: ~e"
              default))
     (hash-ref ht key default)]))

;; interpret-expr : sexp hash path -> value
;; Interprets an expression in the restricted info language.
(define (interpret-expr expr env file)
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
    ;; Vectors (literal)
    [(? vector?) (for/vector ([e (in-vector expr)]) (interpret-expr e env file))]
    ;; Quote
    [`(quote ,datum) datum]
    ;; Quasiquote
    [`(quasiquote ,tmpl) (interpret-quasiquote tmpl env file)]
    ;; If
    [`(if ,test ,then ,else-branch)
     (if (interpret-expr test env file)
         (interpret-expr then env file)
         (interpret-expr else-branch env file))]
    ;; List constructors
    [`(list . ,args)
     (map (lambda (a) (interpret-expr a env file)) args)]
    [`(list* ,args ... ,tail)
     (apply list* (append (map (lambda (a) (interpret-expr a env file)) args)
                          (list (interpret-expr tail env file))))]
    [`(cons ,a ,b)
     (cons (interpret-expr a env file)
           (interpret-expr b env file))]
    [`(car ,a)
     (car (interpret-expr a env file))]
    [`(cdr ,a)
     (cdr (interpret-expr a env file))]
    [`(append . ,args)
     (apply append (map (lambda (a) (interpret-expr a env file)) args))]
    [`(reverse ,a)
     (reverse (interpret-expr a env file))]
    ;; Equality
    [`(equal? ,a ,b)
     (equal? (interpret-expr a env file)
             (interpret-expr b env file))]
    ;; Hash operations
    [`(make-immutable-hash ,a)
     (make-immutable-hash (interpret-expr a env file))]
    [`(make-immutable-hash)
     (make-immutable-hash)]
    [`(hash . ,args)
     (apply hash (map (lambda (a) (interpret-expr a env file)) args))]
    [`(hash-set ,h ,k ,v)
     (hash-set (interpret-expr h env file)
               (interpret-expr k env file)
               (interpret-expr v env file))]
    [`(hash-set* ,h . ,kvs)
     (apply hash-set* (interpret-expr h env file)
            (map (lambda (a) (interpret-expr a env file)) kvs))]
    [`(hash-remove ,h ,k)
     (hash-remove (interpret-expr h env file)
                  (interpret-expr k env file))]
    [`(hash-clear ,h)
     (hash-clear (interpret-expr h env file))]
    [`(hash-update ,h ,k ,updater)
     (hash-update (interpret-expr h env file)
                  (interpret-expr k env file)
                  (interpret-expr updater env file))]
    [`(hash-update ,h ,k ,updater ,default)
     (hash-update (interpret-expr h env file)
                  (interpret-expr k env file)
                  (interpret-expr updater env file)
                  (interpret-expr default env file))]
    ;; String operations
    [`(string-append . ,args)
     (apply string-append (map (lambda (a) (interpret-expr a env file)) args))]
    ;; Path operations
    [`(path->string ,a)
     (path->string (interpret-expr a env file))]
    [`(build-path . ,args)
     (apply build-path (map (lambda (a) (interpret-expr a env file)) args))]
    [`(collection-path . ,args)
     (apply collection-path (map (lambda (a) (interpret-expr a env file)) args))]
    [`(system-library-subpath)
     (system-library-subpath)]
    [`(system-library-subpath ,a)
     (system-library-subpath (interpret-expr a env file))]
    ;; Environment
    [`(getenv ,a)
     (getenv (interpret-expr a env file))]
    ;; Variable reference
    [(? symbol? id)
     (hash-ref env id (lambda () (error 'info.rkt "undefined identifier: ~a in ~a" id file)))]
    [_ (error 'get-info/read "unsupported expression form: ~e in ~a" expr file)]))

;; interpret-quasiquote : sexp hash path -> value
;; Interprets a quasiquote template.
;; Note: after `read`, quasiquote/unquote are already expanded to
;; list forms like (quasiquote ...), (unquote ...), (unquote-splicing ...).
;; We use `list` patterns instead of backtick patterns since `match`
;; treats unquote specially inside quasiquote patterns.
(define (interpret-quasiquote tmpl env file)
  (cond
    [(and (pair? tmpl) (eq? (car tmpl) 'unquote)
          (pair? (cdr tmpl)) (null? (cddr tmpl)))
     (interpret-expr (cadr tmpl) env file)]
    [(and (pair? tmpl) (eq? (car tmpl) 'unquote-splicing))
     (error 'get-info/read "unquote-splicing not allowed outside list context in ~a" file)]
    [(pair? tmpl)
     (define va (interpret-qq-element (car tmpl) env file))
     (define vb (interpret-quasiquote (cdr tmpl) env file))
     (if (and (pair? va) (eq? (car va) 'spliced))
         (append (cdr va) vb)
         (cons va vb))]
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
     (define va (interpret-qq-element (car tmpl) env file))
     (define vb (interpret-quasiquote (cdr tmpl) env file))
     (if (and (pair? va) (eq? (car va) 'spliced))
         (append (cdr va) vb)
         (cons va vb))]
    [else tmpl]))
