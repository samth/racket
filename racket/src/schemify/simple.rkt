#lang racket/base
(require racket/fixnum
         "wrap.rkt"
         "match.rkt"
         "known.rkt"
         "lambda.rkt"
         "import.rkt"
         "mutated-state.rkt")

(provide simple?
         simple/can-copy?)

;; Check whether an expression is simple in the sense that its order
;; of evaluation isn't detectable (`pure?` = #t) or at least it won't
;; try to capture a continuation (`pure?` = #f). In `pure?` mode, if
;; `no-alloc?` is true, then allocation counts as detectable (for
;; ordering with respect to functions that might capture a continuation).
;; If `ordered?` is true with `pure?` as true, then things that always
;; succeed with the same value are allowed, even if they may depend
;; on an earlier action not raising an exception.
;; If `succeeds?` is true with `pure?` and `ordered?` as true, then
;; things that always succeed are allowed, even if they aren't pure
;; (i.e., a later call might produce a different result).
;; This function receives both schemified and non-schemified expressions.
(define (simple? e prim-knowns knowns imports mutated simples unsafe-mode?
                 #:pure? [pure? #t]
                 #:no-alloc? [no-alloc? #f]
                 #:ordered? [ordered? #f] ; weakens `pure?` to allow some reordering
                 #:succeeds? [succeeds? #f] ; weakens `ordered?` to allow more reordering
                 #:result-arity [result-arity 1])
  ;; A nested binding can hide a procedure in the candidate group.
  (define (shadows-local? e local-ids)
    (define (formals-shadow? formals)
      (let loop ([v (unwrap formals)])
        (cond
          [(symbol? v) (memq v local-ids)]
          [(pair? v) (or (loop (unwrap (car v)))
                         (loop (unwrap (cdr v))))]
          [else #f])))
    (let loop ([e e])
      (match e
        [`(quote . ,_) #f]
        [`(quote-syntax . ,_) #f]
        [`(lambda ,formals ,body ...)
         (or (formals-shadow? formals)
             (for/or ([e (in-list body)]) (loop e)))]
        [`(case-lambda [,formals ,body ...] ...)
         (for/or ([formals (in-list formals)] [body (in-list body)])
           (or (formals-shadow? formals)
               (for/or ([e (in-list body)]) (loop e))))]
        [`(let-values ([,idss ,rhss] ...) ,body ...)
         (or (for/or ([ids (in-list idss)]) (formals-shadow? ids))
             (for/or ([e (in-list rhss)]) (loop e))
             (for/or ([e (in-list body)]) (loop e)))]
        [`(letrec-values ([,idss ,rhss] ...) ,body ...)
         (or (for/or ([ids (in-list idss)]) (formals-shadow? ids))
             (for/or ([e (in-list rhss)]) (loop e))
             (for/or ([e (in-list body)]) (loop e)))]
        [`(let ([,ids ,rhss] ...) ,body ...)
         (or (for/or ([id (in-list ids)]) (formals-shadow? id))
             (for/or ([e (in-list rhss)]) (loop e))
             (for/or ([e (in-list body)]) (loop e)))]
        [`(letrec* ([,ids ,rhss] ...) ,body ...)
         (or (for/or ([id (in-list ids)]) (formals-shadow? id))
             (for/or ([e (in-list rhss)]) (loop e))
             (for/or ([e (in-list body)]) (loop e)))]
        [`(set! ,id ,rhs)
         (or (memq (unwrap id) local-ids)
             (loop rhs))]
        [`(,a . ,d) (or (loop a) (loop d))]
        [`,_ #f])))
  ;; Infer a no-prompt summary for a group of local procedures. Assuming
  ;; every member is no-prompt while checking all bodies handles recursion;
  ;; an unknown call in any member rejects the whole group.
  (define (local-no-prompt-knowns idss rhss recursive? body)
    (and (not pure?)
         (for/and ([ids (in-list idss)] [rhs (in-list rhss)])
           (and (= (length ids) 1)
                (simple-mutated-state? (hash-ref mutated (unwrap (car ids)) #f))
                (match rhs
                  [`(lambda . ,_) #t]
                  [`(case-lambda . ,_) #t]
                  [`,_ #f])))
         (let* ([local-ids (for/list ([ids (in-list idss)]) (unwrap (car ids)))]
                [candidate-knowns
                 (for/fold ([new-knowns knowns]) ([ids (in-list idss)]
                                                 [rhs (in-list rhss)])
                   (hash-set new-knowns (unwrap (car ids))
                             (known-procedure/no-prompt (lambda-arity-mask rhs))))])
           (and (not (shadows-local? body local-ids))
                (for/and ([rhs (in-list rhss)])
                  (define body-knowns (if recursive? candidate-knowns knowns))
                  (and (not (shadows-local? rhs local-ids))
                       (match rhs
                         [`(lambda ,_ ,body ...)
                          (simple? `(begin ,@body) prim-knowns body-knowns imports mutated
                                   (make-hasheq) unsafe-mode? #:pure? #f)]
                         [`(case-lambda [,_ ,body ...] ...)
                          (for/and ([clause-body (in-list body)])
                            (simple? `(begin ,@clause-body) prim-knowns body-knowns imports mutated
                                     (make-hasheq) unsafe-mode? #:pure? #f))]
                         [`,_ #f])))
                candidate-knowns))))
  (define (simple/local-body body local-knowns result-arity)
    (simple? body prim-knowns local-knowns imports mutated (make-hasheq) unsafe-mode?
             #:pure? #f #:result-arity result-arity))
  (let simple? ([e e] [result-arity result-arity])
    (define-syntax-rule (cached expr)
      (let* ([c (hash-ref simples e #(0 0 1))]
             [bit (let ([AT (lambda (x) (fxlshift 1 x))])
                    (if pure?
                        (if no-alloc?
                            (if ordered? (if succeeds? (AT 0) (AT 1)) (AT 2))
                            (if ordered? (if succeeds? (AT 3) (AT 4)) (AT 5)))
                        (AT 6)))]
             [r (cond
                  [(fx= bit (fxand (vector-ref c 0) bit)) #t]
                  [(fx= bit (fxand (vector-ref c 1) bit)) #f]
                  [else 'unknown])]
             [arity-match? (eqv? result-arity (vector-ref c 2))])
        (if (or (eq? 'unknown r)
                (not arity-match?))
            (let ([r expr])
              (hash-set! simples e (vector (if r
                                               (fxior (vector-ref c 0) bit)
                                               (vector-ref c 0))
                                           (if r
                                               (vector-ref c 1)
                                               (fxior (vector-ref c 1) bit))
                                           (vector-ref c 2)))
              r)
            r)))
    (define (returns n)
      (or (not result-arity)
          (eqv? n result-arity)))
    (define (simple-begin? es)
      (cached
       (let loop ([es es])
         (cond
           [(null? (cdr es))
            (simple? (car es) result-arity)]
           [else
            (and (simple? (car es) #f)
                 (loop (cdr es)))]))))
    (define (ok-to-call? proc-name v n-args)
      (if pure?
          (and (or (if no-alloc?
                       (known-procedure/pure? v)
                       (or (known-procedure/allocates? v)
                           (and n-args
                                (or (eq? proc-name 'hasheq)
                                    (eq? proc-name 'hasheqv))
                                (even? n-args))))
                   (and ordered?
                        (or (known-procedure/then-pure? v)
                            (and succeeds?
                                 (eqv? n-args 0)
                                 (known-procedure/parameter? v))
                            ;; in unsafe mode, we can assume no contract error:
                            (and unsafe-mode?
                                 (known-field-accessor? v)
                                 (known-field-accessor-authentic? v)
                                 (known-field-accessor-known-immutable? v)))))
               (returns 1))
          (or (and (known-procedure/no-prompt? v)
                   (returns 1))
              (and (eqv? n-args 0)
                   (known-procedure/parameter? v)
                   (returns 1))
              (and (known-procedure/no-prompt/multi? v)
                   (eqv? result-arity #f))
              (and (known-field-accessor? v)
                   (known-field-accessor-authentic? v)
                   (returns 1))
              (and (known-field-mutator? v)
                   (known-field-mutator-authentic? v)
                   (returns 1))
              (and (known-procedure/no-prompt-up-to? v)
                   n-args
                   (<= n-args (known-procedure/no-prompt-up-to-n v))
                   (returns 1)))))
    (match e
      [`(lambda . ,_) (returns 1)]
      [`(case-lambda . ,_) (returns 1)]
      [`(quote . ,_) (returns 1)]
      [`(#%variable-reference . ,_) (returns 1)]
      [`(#%foreign-inline ,_ ,mode) (and (case mode
                                           [(copy copy*) #t]
                                           [(pure pure*) (not no-alloc?)]
                                           [else (not (or pure? no-alloc?))])
                                         (returns 1))]
      [`(let-values ([,idss ,rhss] ...) ,body)
       (cached
        (or (let ([local-knowns (local-no-prompt-knowns idss rhss #f body)])
              (and local-knowns
                   (simple/local-body body local-knowns result-arity)))
            (and (for/and ([ids (in-list idss)]
                           [rhs (in-list rhss)])
                   (simple? rhs (length ids)))
                 (simple? body result-arity))))]
      [`(let ([,ids ,rhss] ...) ,body)
       (cached
        (or (let ([local-knowns
                   (local-no-prompt-knowns (for/list ([id (in-list ids)]) (list id))
                                           rhss #f body)])
              (and local-knowns
                   (simple/local-body body local-knowns result-arity)))
            (and (for/and ([rhs (in-list rhss)])
                   (simple? rhs 1))
                 (simple? body result-arity))))]
      [`(letrec-values ([(,idss ...) ,rhss] ...) ,body)
       (cached
        (or (let ([local-knowns (local-no-prompt-knowns idss rhss #t body)])
              (and local-knowns
                   (simple/local-body body local-knowns result-arity)))
            (and (for/and ([ids (in-list idss)]
                           [rhs (in-list rhss)])
                   (simple? rhs (length ids)))
                 (simple? body result-arity))))]
      [`(letrec* ([,ids ,rhss] ...) ,body)
       (cached
        (or (let ([local-knowns
                   (local-no-prompt-knowns (for/list ([id (in-list ids)]) (list id))
                                           rhss #t body)])
              (and local-knowns
                   (simple/local-body body local-knowns result-arity)))
            (and (for/and ([rhs (in-list rhss)])
                   (simple? rhs 1))
                 (simple? body result-arity))))]
      [`(begin ,es ...)
       #:guard (not pure?)
       (simple-begin? es)]
      [`(begin-unsafe ,es ...)
       (simple-begin? es)]
      [`(begin0 ,e0 ,es ...)
       (cached
        (and (simple? e0 result-arity)
             (for/and ([e (in-list es)])
               (simple? e #f))))]
      [`(set! ,_ ,e)
       #:guard (not pure?)
       (and (simple? e 1)
            (returns 1))]
      [`(if ,tst ,thn ,els)
       (and (simple? tst 1)
            (simple? thn result-arity)
            (simple? els result-arity))]
      [`(values ,es ...)
       (cached
        (and (returns (length es))
             (for/and ([e (in-list es)])
               (simple? e 1))))]
      [`(apply ,proc ,es ...)
       (cached
        (and (not result-arity)
             (not pure?) ; because we can't statically check arity
             (let ([proc (unwrap proc)])
               (and (symbol? proc)
                    (let ([v (or (hash-ref-either knowns imports proc)
                                 (hash-ref prim-knowns proc #f))])
                      (ok-to-call? proc v #f))))
             (for/and ([e (in-list es)])
               (simple? e 1))))]
      [`((letrec-values ([(,idss ...) ,rhss] ...) ,rator) ,args ...)
       #:guard (not pure?)
       (cached
        (let ([local-knowns (local-no-prompt-knowns idss rhss #t rator)]
              [rator (unwrap rator)])
          (and local-knowns
               (symbol? rator)
               (for/or ([ids (in-list idss)])
                 (eq? rator (unwrap (car ids))))
               (let ([v (hash-ref local-knowns rator #f)])
                 (and (known-procedure/no-prompt? v)
                      (bitwise-bit-set? (known-procedure-arity-mask v) (length args))))
               (returns 1)
               (for/and ([arg (in-list args)])
                 (simple? arg 1)))))]
      [`(,proc . ,args)
       (cached
        (let ([proc (unwrap proc)])
          (and
           (or (and (symbol? proc)
                    (let ([v (or (hash-ref-either knowns imports proc)
                                 (hash-ref prim-knowns proc #f))])
                      (and (ok-to-call? proc v (length args))
                           (bitwise-bit-set? (known-procedure-arity-mask v) (length args))))
                    (simple-mutated-state? (hash-ref mutated proc #f)))
               (match proc
                 [`(#%foreign-inline ,_ ,mode)
                  (and (case mode
                         [(copy*) #t]
                         [(pure*) (not no-alloc?)]
                         [else #f])
                       (returns 1))]
                 [`,_ #f]))
           (for/and ([arg (in-list args)])
             (simple? arg 1)))))]
      [`,_
       (let ([e (unwrap e)])
         (and (returns 1)
              (or (and (symbol? e)
                       (simple-mutated-state? (hash-ref mutated e #f)))
                  (integer? e)
                  (boolean? e)
                  (string? e)
                  (bytes? e)
                  (regexp? e))))])))

(define (simple/can-copy? e prim-knowns knowns imports mutated)
  (match e
    [`(quote ,v) (can-copy-literal? v)]
    [`(,_ . ,_) #f]
    [`,_
     (let ([e (unwrap e)])
       (or (and (symbol? e)
                (simple-mutated-state? (hash-ref mutated e #f)))
           (can-copy-literal? e)))]))

(define (can-copy-literal? e)
  (or (integer? e)
      (boolean? e)
      (symbol? e)))
