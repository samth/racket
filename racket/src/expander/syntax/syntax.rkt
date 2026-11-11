#lang racket/base
(require racket/private/place-local
         racket/fixnum
         (only-in racket/unsafe/ops unsafe-struct*-cas!)
         "../compile/serialize-property.rkt"
         "../compile/serialize-state.rkt"
         "../common/set.rkt"
         "../common/scope-set.rkt"
         "../common/inline.rkt"
         "../namespace/inspector.rkt"
         "preserved.rkt"
         "taint-object.rkt"
         "datum-map.rkt"
         "weaker-inspector.rkt")

(provide
 (struct-out syntax) ; includes `syntax?`
 syntax-shifted-multi-scopes
 syntax-mpi-shifts
 syntax-inspector
 syntax-content
 syntax-taintness
 empty-syntax
 identifier?
 syntax-identifier?

 (struct-out modified-content)
 re-modify-content
 syntax-content*-cas!
 
 syntax->datum
 datum->syntax

 immediate-datum->syntax
 empty-props

 syntax-map
 non-syntax-map
 
 prop:propagation
 prop:propagation-taint
 prop:propagation-set-taint
 propagation-set-taint?
 propagation-set-taint-ref
 
 deserialize-syntax
 deserialize-datum->syntax

 syntax-place-init!)

;; Used for content wrapped with scope propagations and/or a taint,
;; so a `content*` is either a `modified-content` or plain content
(struct modified-content (content scope-propagations+taint)
  #:authentic)

(provide (struct-out stx-context)
         syntax-ctx
         empty-context
         ctx-with-shifted-multi-scopes
         ctx-with-mpi-shifts+inspector)

;; The parts of a syntax object other than its scopes that change
;; rarely and that sibling syntax objects almost always share.
;; Immutable, so it is shared freely among syntax objects.
(struct stx-context (shifted-multi-scopes ; scopes with a distinct identity at each phase; maybe a fallback search
                     mpi-shifts ; chain of module-path-index substitutions
                     inspector) ; inspector for access to protected bindings
  #:authentic
  #:sealed)

(define (ctx-with-shifted-multi-scopes ctx smss)
  (if (eq? smss (stx-context-shifted-multi-scopes ctx))
      ctx
      (stx-context smss
                   (stx-context-mpi-shifts ctx)
                   (stx-context-inspector ctx))))

(define (ctx-with-mpi-shifts+inspector ctx mss insp)
  (if (and (eq? mss (stx-context-mpi-shifts ctx))
           (eq? insp (stx-context-inspector ctx)))
      ctx
      (stx-context (stx-context-shifted-multi-scopes ctx)
                   mss
                   insp)))

(struct syntax ([content* #:mutable] ; datum and nested syntax objects; mutated for lazy propagation
                scopes  ; scopes that apply at all phases
                ctx     ; `stx-context`: shifted multi-scopes, mpi shifts, inspector
                srcloc  ; source location
                props)  ; properties
  #:authentic
  #:sealed
  ;; Custom printer:
  #:property prop:custom-write
  (lambda (s port mode)
    (write-string "#<syntax" port)
    (define srcloc (syntax-srcloc s))
    (when srcloc
      (define srcloc-str (srcloc->string srcloc))
      (when srcloc-str
        (fprintf port ":~a" srcloc-str)))
    (define width (print-syntax-width))
    (cond
      [(eqv? width 0) (void)]
      [(eqv? width +inf.0)
       (fprintf port " ~s" (syntax->datum s))]
      [else
       (parameterize ([error-print-width width])
         (fprintf port " ~.s" (syntax->datum s)))])
    (write-string ">" port))
  #:property prop:serialize
  (lambda (s ser-push! state)
    (define content* (syntax-content* s))
    (define content
      (if (modified-content? content*)
          (let ([prop (modified-content-scope-propagations+taint content*)])
            (if (propagation? prop)
                ((propagation-ref prop) s)
                (modified-content-content content*)))
          content*))
    (define properties
      (intern-properties
       (syntax-props s)
       (lambda ()
         (define preserve-keys (serialize-state-preserve-prop-keys state))
         (for/hasheq ([(k v) (in-hash (syntax-props s))]
                      #:when (or (preserved-property-value? v)
                                 (hash-ref preserve-keys k #f)))
           (values k (check-value-to-preserve (plain-property-value v) syntax?))))
       state))
    (define taint (syntax-taintness s))
    (define context-triple
      (intern-context-triple (intern-scopes (syntax-scopes s) state)
                             (intern-shifted-multi-scopes (syntax-shifted-multi-scopes s) state)
                             (if (serialize-state-drop-shifts? state)
                                 null
                                 (intern-mpi-shifts (syntax-mpi-shifts s) state))
                             state))
    (define stx-state (get-syntax-context state))
    (cond
      [(or properties taint)
       (ser-push! 'tag '#:syntax+props)
       (push-syntax-context! state #f)
       (ser-push! content)
       (pop-syntax-context! state)
       (ser-push! 'reference context-triple)
       (ser-push! 'reference (syntax-srcloc s))
       (ser-push! properties)
       (ser-push! taint)
       (when stx-state (set-syntax-state-all-sharing?! stx-state #f))]
      [else
       ;; We rely on two passes to reach a fixpoint on sharing:
       (define sharing-mode (hash-ref (serialize-state-sharing-syntaxes state) s 'unknown))
       (cond
         [(eq? sharing-mode 'share)
          (ser-push! 'tag '#:datum->syntax)
          (ser-push! (syntax->datum s))]
         [(eq? sharing-mode 'unknown)
          (ser-push! 'tag '#:syntax)
          ;; Communicate to nested syntax objects the info that they might share
          (define this-state (and (no-pair-syntax-in-cdr? content)
                                  (syntax-state #t context-triple (syntax-srcloc s))))
          (push-syntax-context! state this-state)
          ;; Serialize content
          (ser-push! content)
          ;; Check whether we're sharing for all nested syntax objects
          (pop-syntax-context! state)
          (define new-sharing-mode
            (if (and this-state
                     (syntax-state-all-sharing? this-state))
                'share
                'none))
          (hash-set! (serialize-state-sharing-syntaxes state)
                     s
                     ;; If the syntax object has only simple content,
                     ;; it doesn't need any sharing support by itself
                     (if (datum-has-elements? content)
                         new-sharing-mode
                         'none))
          (when (and stx-state (eq? new-sharing-mode 'none))
            (set-syntax-state-all-sharing?! stx-state #f))]
         [else
          (ser-push! 'tag '#:syntax)
          (push-syntax-context! state #f)
          (ser-push! content)
          (pop-syntax-context! state)])
       ;; Finish up
       (ser-push! 'reference context-triple)
       (ser-push! 'reference (syntax-srcloc s))
       (when stx-state
         (unless (and (eq? context-triple (syntax-state-context-triple stx-state))
                      (equal? (syntax-srcloc s) (syntax-state-srcloc stx-state)))
           (set-syntax-state-all-sharing?! stx-state #f)))]))
  #:property prop:reach-scopes
  (lambda (s bulk-shifts reach)
    (define content* (syntax-content* s))
    (reach
     (if (modified-content? content*)
         (let ([prop (modified-content-scope-propagations+taint content*)])
           (if (propagation? prop)
               ((propagation-ref prop) s)
               (modified-content-content content*)))
         content*)
     bulk-shifts)
    (define shifts (and bulk-shifts
                        (append bulk-shifts (syntax-mpi-shifts s))))
    (reach (syntax-scopes s) shifts)
    (reach (syntax-shifted-multi-scopes s) shifts)
    (for ([(k v) (in-immutable-hash (syntax-props s))]
          #:when (preserved-property-value? v))
      (reach (plain-property-value v) bulk-shifts))
    (reach (syntax-srcloc s) bulk-shifts)))

;; Property to abstract over handling of propagation for
;; serialization; property value takes a syntax object and
;; returns its content
(define-values (prop:propagation propagation? propagation-ref)
  (make-struct-type-property 'propagation))

;; Property to abstract over extraction of taint from propagation
(define-values (prop:propagation-taint propagation-taint? propagation-taint-ref)
  (make-struct-type-property 'propagation-taint))
(define-values (prop:propagation-set-taint propagation-set-taint? propagation-set-taint-ref)
  (make-struct-type-property 'propagation-set-taint))

(define (syntax-content s)
  (define content* (syntax-content* s))
  (if (modified-content? content*)
      (modified-content-content content*)
      content*))
  
(define (syntax-taintness s)
  (define content* (syntax-content* s))
  (cond
    [(modified-content? content*)
     (define v (modified-content-scope-propagations+taint content*))
     (if (taint? v)
         v
         ((propagation-taint-ref v) v))]
    [else #f]))

(define (syntax-content*-cas! stx old new)
  (unsafe-struct*-cas! stx 0 old new))

(define (re-modify-content s d)
  (define content* (syntax-content* s))
  (if (modified-content? content*)
      (modified-content d (modified-content-scope-propagations+taint content*))
      d))

;; ----------------------------------------

(define empty-scopes empty-scope-set)
(define empty-shifted-multi-scopes (seteq))
(define empty-mpi-shifts null)
(define empty-props #hasheq())

(define empty-context
  (stx-context empty-shifted-multi-scopes empty-mpi-shifts #f))

(define (syntax-shifted-multi-scopes s) (stx-context-shifted-multi-scopes (syntax-ctx s)))
(define (syntax-mpi-shifts s) (stx-context-mpi-shifts (syntax-ctx s)))
(define (syntax-inspector s) (stx-context-inspector (syntax-ctx s)))

(define empty-syntax
  (syntax #f
          empty-scopes
          empty-context
          #f   ; srcloc
          empty-props))

(define (identifier? s)
  (and (syntax? s) (symbol? (syntax-content s))))

(define (syntax-identifier? s) ; assumes that `s` is syntax
  (symbol? (syntax-content s)))

(define (syntax->datum s)
  (syntax-map s (lambda (tail? x) x) (lambda (s d) d) syntax-content))

(define-place-local known-syntax-pairs (make-weak-hasheq))

;; The context for a new syntax object in the context of `stx-c`
;; and with code inspector `insp`
(define (context-for stx-c insp)
  (cond
    [(not stx-c) empty-context]
    [else
     (define ctx (syntax-ctx stx-c))
     (define new-insp (and insp (weaker-inspector insp (stx-context-inspector ctx))))
     (if (eq? new-insp (stx-context-inspector ctx))
         ctx
         (stx-context (stx-context-shifted-multi-scopes ctx)
                      (stx-context-mpi-shifts ctx)
                      new-insp))]))

(define (immediate-datum->syntax stx-c content stx-l props insp)
  (immediate-datum->syntax/ctx stx-c content stx-l props (context-for stx-c insp)))

(define (immediate-datum->syntax/ctx stx-c content stx-l props ctx)
  (syntax (if (and stx-c
                   (syntax-taintness stx-c))
              (modified-content content
                                (tainted-for-content content))
              content)
          (if stx-c (syntax-scopes stx-c) empty-scopes)
          ctx
          (and stx-l (syntax-srcloc stx-l))
          props))

(define (datum->syntax stx-c s [stx-l #f] [stx-p #f])
  (cond
   [(syntax? s) s]
   [else
    (define insp (current-module-code-inspector))
    (define ctx (context-for stx-c insp))
    (define (wrap content)
      (let ([content (datum-intern-literal content)])
        (immediate-datum->syntax/ctx stx-c content stx-l empty-props ctx)))
    (define result-s
      (non-syntax-map s
                      (lambda (tail? x) (cond
                                          [tail?
                                           (when (and (fx> tail? 32)
                                                      (fx= 0 (fxand tail? (fx- tail? 1))))
                                             (hash-set! known-syntax-pairs x #t))
                                           x]
                                          [else (wrap x)]))
                      (lambda (s) s)
                      disallow-cycles
                      known-syntax-pairs))
    (if (and stx-p (not (eq? (syntax-props stx-p) empty-props)))
        (struct-copy syntax result-s
                     [props (syntax-props stx-p)])
        result-s)]))

;; `(syntax-map s f d->s)` walks over `s`:
;; 
;;  * `(f tail? d)` is called to each datum `d`, where `tail?`
;;    indicates that the value is a pair/null in a `cdr` --- so that it
;;    doesn't need to be wrapped for `datum->syntax`, for example
;;
;;  * `(d->s orig-s d)` is called for each syntax object,
;;    and the second argument is result of traversing its datum
;; 
;;  * the `s-e` function extracts content of a syntax object
;;
;; The optional `seen` argument is an `eq?`-based immutable hash table
;; to detect and reject cycles. See `datum-map`.

(define-inline (syntax-map s f d->s s-e [seen #f])
  (let loop ([s s])
    (datum-map s
               f
               (lambda (tail? v)
                 (cond
                  [(syntax? v) (d->s v (loop (s-e v)))]
                  [else (f tail? v)]))
               seen)))

;; `(non-syntax-map s f s->)` is like `(syntax-map s f d->s)`, except that
;;  when a syntax object is found, it is just passed to `s->` --- so there's
;;  no `d->s` or `s-e`, since they would not be called

(define-inline (non-syntax-map s f [s-> (lambda (x) x)] [seen #f] [known-pairs #f])
  (datum-map s
             f
             (lambda (tail? v)
               (cond
                [(syntax? v) (s-> v)]
                [else (f tail? v)]))
             seen
             known-pairs))

(define disallow-cycles
  (hasheq 'cycle-fail
          (lambda (s)
            (raise-arguments-error 'datum->syntax
                                   "cannot create syntax from cyclic datum"
                                   "datum" s))))

(define (syntax-place-init!)
  (set! known-syntax-pairs (make-weak-hasheq)))

;; ----------------------------------------

;; When serializing syntax objects, let nested objects know the
;; content of an enclosing syntax object, so sharing is enabled if the
;; nested syntax objects have the same context and source location.
(struct syntax-state ([all-sharing? #:mutable] context-triple srcloc)
  #:authentic)

;; When sharing syntax information in serialization, we have to be
;; careful not to lose syntax objects that wrap a pair in a `cdr` (and
;; therefore would not be restored by `datum->syntax`).
(define (no-pair-syntax-in-cdr? content)
  (cond
   [(pair? content) (let loop ([content (cdr content)])
                      (cond
                       [(and (syntax? content)
                             (pair? (syntax-content content)))
                        #f]
                       [(pair? content) (loop (cdr content))]
                       [else #t]))]
   [else #t]))

;; ----------------------------------------

;; Called by the deserializer

;; Consecutive deserialized syntax objects usually share a context
;; triple, so remember the most recent conversion. Threads in a place
;; share the memo, so it is a single immutable value that is read
;; once; a lost update only costs a conversion.
(define-place-local last-context-conversion #f) ; #f or (vector context-triple inspector stx-context)

(define (context-triple->context context-triple inspector)
  (define last last-context-conversion)
  (cond
    [(and last
          (eq? context-triple (vector*-ref last 0))
          (eq? inspector (vector*-ref last 1)))
     (vector*-ref last 2)]
    [else
     (define ctx (stx-context (vector*-ref context-triple 1)
                              (vector*-ref context-triple 2)
                              inspector))
     (set! last-context-conversion (vector-immutable context-triple inspector ctx))
     ctx]))

(define (deserialize-syntax content context-triple srcloc props taint inspector)
  (syntax (if taint
              (modified-content content taint)
              content)
          ;; The deserializer cannot tell an empty set of scopes from other
          ;; empty `seteq`s, so it produces an empty hash table in that case:
          (let ([scs (vector*-ref context-triple 0)])
            (if (scope-set? scs) scs empty-scope-set))
          (context-triple->context context-triple inspector)
          srcloc
          (if props
              (for/hasheq ([(k v) (in-immutable-hash props)])
                (values k (preserved-property-value v)))
              empty-props)))

(define (deserialize-datum->syntax content context-triple srcloc inspector)
  (define s (deserialize-syntax #f context-triple srcloc #f #f inspector))
  (datum->syntax s content s s))
