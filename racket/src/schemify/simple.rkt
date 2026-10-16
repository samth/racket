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

;; Check whether an expression is simple:
;;
;;   `pure?` = #t: its order of evaluation isn't detectable
;;
;;   `pure?` = #f: it won't try to capture a continuation or
;;                 reference a variable too early
;;
;; In `pure?` mode:
;;
;;   `no-alloc?` = #t: allocation counts as detectable (for
;;                     ordering with respect to functions that
;;                     might capture a continuation)
;;
;;   `ordered?` = #t: things that always succeed with the same
;;                    value are allowed, even if they may depend
;;                    on an earlier action not raising an exception
;;
;;   `succeeds?` = #t and `ordered?` = #t: things that always
;;                    succeed are allowed, even if they aren't pure
;;                    (i.e., a later call might produce a different
;;                    result)
;;
;;   `parameter-ok?` = #t: ok to look up a parameter value
;;
;; This function receives both schemified and non-schemified expressions.
;;
;; The `mutated` table is generally expected to be in its stable state, but
;; we want to use `simple?` also in the pass that computes `mutated`. We rely
;; on all `set!`s having been found, at least.
;;
(define (simple? e prim-knowns knowns imports mutated simples unsafe-mode?
                 #:pure? [pure? #t]
                 #:no-alloc? [no-alloc? #f]
                 #:ordered? [ordered? #f] ; weakens `pure?` to allow some reordering
                 #:succeeds? [succeeds? #f] ; weakens `ordered?` to allow more reordering
                 #:parameter-ok? [parameter-ok? #f] ; weakens `pure?` to allow parameter lookup
                 #:result-arity [result-arity 1])
  ;; Bindings have unique names, but mutation discovery may still be in
  ;; progress when this analysis runs. Reject assignments to candidate
  ;; procedures even when they have not reached the mutated table yet.
  (define (assigns-local? e local-ids)
    (match e
      [`(quote . ,_) #f]
      [`(quote-syntax . ,_) #f]
      [`(set! ,id ,rhs)
       (or (memq (unwrap id) local-ids) (assigns-local? rhs local-ids))]
      [`(,a . ,d)
       (or (assigns-local? a local-ids) (assigns-local? d local-ids))]
      [`,_ #f]))
  ;; Infer a no-prompt summary for a group of local procedures. Assuming
  ;; every member is no-prompt while checking all bodies handles recursion;
  ;; an unknown call in any member rejects the whole group.
  (define (local-no-prompt-knowns idss rhss body)
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
           (define (simple-body? body)
             (simple/local-body `(begin ,@body) candidate-knowns 1))
           (and (not (assigns-local? body local-ids))
                (for/and ([rhs (in-list rhss)])
                  (and (not (assigns-local? rhs local-ids))
                       (match rhs
                         [`(lambda ,_ ,body ...) (simple-body? body)]
                         [`(case-lambda [,_ ,body ...] ...)
                          (for/and ([clause-body (in-list body)])
                            (simple-body? clause-body))]
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
                                 (known-field-accessor-known-immutable? v))))
                   (and parameter-ok?
                        (eqv? n-args 0)
                        (known-procedure/parameter? v)
                        (returns 1)))
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
    (define (simple-letrec? idss rhss body)
      (or (let ([local-knowns (local-no-prompt-knowns idss rhss body)])
            (and local-knowns
                 (simple/local-body body local-knowns result-arity)))
          (and (for/and ([ids (in-list idss)] [rhs (in-list rhss)])
                 (simple? rhs (length ids)))
               (simple? body result-arity))))
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
        (and (for/and ([ids (in-list idss)]
                       [rhs (in-list rhss)])
               (simple? rhs (length ids)))
             (simple? body result-arity)))]
      [`(let ([,_ ,rhss] ...) ,body)
       (cached
        (and (for/and ([rhs (in-list rhss)])
               (simple? rhs 1))
             (simple? body result-arity)))]
      [`(letrec-values ([(,idss ...) ,rhss] ...) ,body)
       (cached (simple-letrec? idss rhss body))]
      [`(letrec* ([,ids ,rhss] ...) ,body)
       (cached (simple-letrec? (for/list ([id (in-list ids)]) (list id))
                               rhss body))]
      [`(begin ,es ...)
       (simple-begin? es)]
      [`(begin-unsafe ,es ...)
       (simple-begin? es)]
      [`(begin0 ,e0 ,es ...)
       (cached
        (and (simple? e0 result-arity)
             (for/and ([e (in-list es)])
               (simple? e #f))))]
      [`(set! ,x ,e)
       (and (not pure?)
            (simple? x 1)
            (simple? e 1)
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
        (let ([local-knowns (local-no-prompt-knowns idss rhss rator)]
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
                       (if pure?
                           (simple-mutated-state? (hash-ref mutated e #f))
                           (not (too-early-mutated-state? (hash-ref mutated e #f)))))
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
