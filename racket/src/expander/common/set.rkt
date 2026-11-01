#lang racket/base
(require (for-syntax racket/base)
         racket/unsafe/ops
         "scope-set.rkt")

;; Lightweight variant of sets

(provide set seteq seteqv
         set?
         set-empty?
         set-member?
         set-count
         set-add
         set-remove
         set-first
         subset?
         set=?
         set-subtract
         set-union
         set-intersect
         set-partition
         set->list
         list->set
         list->seteq
         for/set
         for/seteq
         for/seteqv
         for*/set
         for*/seteq
         in-set

         scope-set?
         empty-scope-set
         ->scope-set)

(define the-empty-hash #hash())
(define the-empty-hasheq #hasheq())
(define the-empty-hasheqv #hasheqv())

(define set
  (case-lambda
    [() the-empty-hash]
    [l (for/fold ([s the-empty-hash]) ([e (in-list l)])
         (hash-set s e #t))]))
(define seteq
  (case-lambda
    [() the-empty-hasheq]
    [l (for/fold ([s the-empty-hasheq]) ([e (in-list l)])
         (hash-set s e #t))]))
(define (seteqv) the-empty-hasheqv)

;; Most sets are immutable hash tables, but scope sets have their own
;; representation; see "scope-set.rkt". Operations on two sets accept
;; a mixture, and an operation that involves a scope set produces a
;; scope set.

(define (set? s) (or (hash? s) (scope-set? s)))

(define (set-empty? s) (if (scope-set? s)
                           (eqv? 0 (scope-set-count s))
                           (zero? (hash-count s))))
(define (set-member? s e) (if (scope-set? s)
                              (scope-set-member? s e)
                              (hash-ref s e #f)))
(define (set-count s) (if (scope-set? s)
                          (scope-set-count s)
                          (hash-count s)))

(define (set-add s e) (if (scope-set? s)
                          (scope-set-add s e)
                          (hash-set s e #t)))
(define (set-remove s e) (if (scope-set? s)
                             (scope-set-remove s e)
                             (hash-remove s e)))
(define (set-first s) (if (scope-set? s)
                          (scope-set-max s)
                          (hash-iterate-key s (hash-iterate-first s))))

(define (in-set/proc s)
  (if (scope-set? s)
      (in-list (scope-set->list s))
      (in-immutable-hash-keys s)))

(define-sequence-syntax in-set
  (lambda () #'in-set/proc)
  (lambda (stx)
    (syntax-case stx ()
      [[(id) (_ s-expr)]
       #'[(id)
          (:do-in
           ([(s) s-expr])
           (unless (or (scope-set? s) (and (hash? s) (immutable? s)))
             (raise-argument-error 'in-set "set?" s))
           ([i (if (scope-set? s)
                   (scope-set-elems s)
                   (unsafe-immutable-hash-iterate-first s))])
           (if (scope-set? s)
               (pair? i)
               i)
           ([(id) (if (scope-set? s)
                      (unsafe-car i)
                      (unsafe-immutable-hash-iterate-key s i))])
           #t
           #t
           [(if (scope-set? s)
                (unsafe-cdr i)
                (unsafe-immutable-hash-iterate-next s i))])]]
      [_ #f])))

(define (->scope-set s)
  (if (scope-set? s) s (hash->scope-set s)))

(define (subset? s1 s2)
  (cond
    [(scope-set? s1)
     (if (scope-set? s2)
         (scope-set-subset? s1 s2)
         (and ((scope-set-count s1) . <= . (hash-count s2))
              (for/and ([k (in-set s1)]) (hash-ref s2 k #f))))]
    [(scope-set? s2)
     (and ((hash-count s1) . <= . (scope-set-count s2))
          (for/and ([k (in-set s1)]) (scope-set-member? s2 k)))]
    [else
     (hash-keys-subset? s1 s2)]))

(define (set=? s1 s2)
  (cond
    [(eq? s1 s2) #t]
    [(and (scope-set? s1) (scope-set? s2))
     (scope-set=? s1 s2)]
    [else
     (and (= (set-count s1) (set-count s2))
          (subset? s1 s2))]))

(define (set-subtract s1 s2)
  (cond
    [(or (scope-set? s1) (scope-set? s2))
     (scope-set-subtract (->scope-set s1) (->scope-set s2))]
    [else
     (for/fold ([s1 s1]) ([k (in-set s2)])
       (hash-remove s1 k))]))

(define (set-union s1 s2)
  (cond
    [(or (scope-set? s1) (scope-set? s2))
     (scope-set-union (->scope-set s1) (->scope-set s2))]
    [((set-count s1) . < . (set-count s2))
     (set-union s2 s1)]
    [else
     (for/fold ([s1 s1]) ([k (in-set s2)])
       (hash-set s1 k #t))]))

(define (set-intersect s1 s2)
  (cond
    [(or (scope-set? s1) (scope-set? s2))
     (scope-set-intersect (->scope-set s1) (->scope-set s2))]
    [((set-count s1) . < . (set-count s2))
     (set-intersect s2 s1)]
    [else
     (for/fold ([s s2]) ([k (in-set s2)])
       (if (hash-ref s1 k #f)
           s
           (hash-remove s k)))]))

(define (set-partition s pred empty-y-set empty-n-set)
  (for/fold ([y empty-y-set] [n empty-n-set]) ([v (in-set s)])
    (if (pred v)
        (values (set-add y v) n)
        (values y (set-add n v)))))

(define (set->list s)
  (if (scope-set? s)
      (scope-set->list s)
      (for/list ([k (in-set s)])
        k)))

(define (list->set l)
  (for/set ([k (in-list l)])
    k))

(define (list->seteq l)
  (for/seteq ([k (in-list l)])
    k))

(define-syntax-rule (for/set bindings body ...)
  (for/hash bindings (values
                      (let ()
                        body ...)
                      #t)))

(define-syntax-rule (for/seteq bindings body ...)
  (for/hasheq bindings (values
                        (let ()
                          body ...)
                        #t)))

(define-syntax-rule (for/seteqv bindings body ...)
  (for/hasheqv bindings (values
                         (let ()
                           body ...)
                         #t)))

(define-syntax-rule (for*/set bindings body ...)
  (for*/hash bindings (values
                       (let ()
                         body ...)
                       #t)))

(define-syntax-rule (for*/seteq bindings body ...)
  (for*/hasheq bindings (values
                         (let ()
                           body ...)
                         #t)))
