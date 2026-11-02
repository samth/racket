#lang racket/base
(require (for-syntax racket/base)
         racket/fixnum
         racket/unsafe/ops
         "../compile/serialize-property.rkt")

;; A scope set is represented as a list of scopes sorted by decreasing
;; scope id, plus a count and an order-independent hash code that is
;; maintained incrementally: each scope has a pseudo-random key, and
;; the set's code is the `fxxor` of its scopes' keys (as in Zobrist
;; hashing).
;;
;; New scopes have larger ids, and most operations add, remove, or
;; flip a scope that is among the most recent few in a set. With
;; the most recent scope first, those operations allocate only a few
;; pairs, and the result shares its tail with the original set.
;; Shared tails also let equality and subset tests stop early. The
;; hash code makes `equal?`-based hashing on scope sets constant time,
;; and it lets `scope-set=?` reject almost every pair of distinct sets
;; without looking at elements. The most recent scope, which is where
;; bindings are attached, is the first element.
;;
;; Scope sets are immutable, so they are safe to share among threads.

(provide (struct-out scope-base)
         make-scope-key

         scope-set?
         empty-scope-set
         scope-set-empty?
         scope-set-count
         scope-set-code
         scope-set-max
         scope-set-member?
         scope-set-add
         scope-set-remove
         scope-set-flip
         scope-set=?
         scope-set-subset?
         scope-set-subset-of-union?
         scope-set-union
         scope-set-subtract
         scope-set->list
         list->scope-set
         in-scope-set
         scope-set-apply-ops
         scope-ops-ref
         scope-ops-update
         scope-ops-compose)

;; Every scope has an `id` (exact rational) and a fixnum `key`
(struct scope-base (id key)
  #:authentic)

(define (mix n)
  (let* ([h (fx*/wraparound n #x5bd1e995)]
         [h (fxxor h (fxrshift h 17))]
         [h (fx*/wraparound h #x2c1b3c6d)]
         [h (fxxor h (fxrshift h 13))]
         [h (fx*/wraparound h #x297a2d39)])
    (fxxor h (fxrshift h 16))))

(define (make-scope-key id)
  (if (fixnum? id)
      (mix id)
      (mix (fx+/wraparound (fx*/wraparound (mix (numerator id)) 31)
                           (denominator id)))))

(define-syntax-rule (id> a b)
  (let ([x a] [y b])
    (if (and (fixnum? x) (fixnum? y))
        (unsafe-fx> x y)
        (> x y))))

(define-syntax-rule (sc> a b)
  (id> (scope-base-id a) (scope-base-id b)))

(struct scope-set (elems  ; list of scopes, decreasing by id
                   count
                   code)  ; fxxor of scope keys
  #:authentic
  #:sealed
  #:property prop:equal+hash
  (list (lambda (a b eql?) (scope-set=? a b))
        (lambda (a hc) (scope-set-code a))
        (lambda (a hc) (scope-set-code a)))
  #:property prop:serialize
  (lambda (s ser-push! state)
    ;; Same encoding as a `seteq` of scopes, with scopes in
    ;; increasing order
    (ser-push! 'tag '#:seteq)
    (ser-push! 'exact (scope-set-count s))
    (for ([sc (in-list (reverse (scope-set-elems s)))])
      (ser-push! sc)))
  #:property prop:reach-scopes
  (lambda (s extra-shifts reach)
    (for ([sc (in-list (scope-set-elems s))])
      (reach sc extra-shifts))))

(define empty-scope-set (scope-set null 0 0))

(define (scope-set-empty? s) (null? (scope-set-elems s)))

;; The most recent scope in a non-empty set
(define (scope-set-max s) (car (scope-set-elems s)))

(define (scope-set-member? s sc)
  (let loop ([l (scope-set-elems s)])
    (cond
      [(null? l) #f]
      [(eq? (car l) sc) #t]
      [(sc> sc (car l)) #f]
      [else (loop (cdr l))])))

(define (make-set l count code)
  (if (null? l)
      empty-scope-set
      (scope-set l count code)))

(define (scope-set-add s sc)
  (define l (scope-set-elems s))
  (let loop ([p l])
    (cond
      [(or (null? p) (sc> sc (car p)))
       (scope-set (prefix-onto l p (cons sc p))
                  (fx+ (scope-set-count s) 1)
                  (fxxor (scope-set-code s) (scope-base-key sc)))]
      [(eq? (car p) sc) s]
      [else (loop (cdr p))])))

(define (scope-set-remove s sc)
  (define l (scope-set-elems s))
  (let loop ([p l])
    (cond
      [(or (null? p) (sc> sc (car p))) s]
      [(eq? (car p) sc)
       (make-set (prefix-onto l p (cdr p))
                 (fx- (scope-set-count s) 1)
                 (fxxor (scope-set-code s) (scope-base-key sc)))]
      [else (loop (cdr p))])))

(define (scope-set-flip s sc)
  (define l (scope-set-elems s))
  (let loop ([p l])
    (cond
      [(or (null? p) (sc> sc (car p)))
       (scope-set (prefix-onto l p (cons sc p))
                  (fx+ (scope-set-count s) 1)
                  (fxxor (scope-set-code s) (scope-base-key sc)))]
      [(eq? (car p) sc)
       (make-set (prefix-onto l p (cdr p))
                 (fx- (scope-set-count s) 1)
                 (fxxor (scope-set-code s) (scope-base-key sc)))]
      [else (loop (cdr p))])))

;; Copy the elements of `l` up to the tail `p` onto `tail`
(define (prefix-onto l p tail)
  (let loop ([l l])
    (if (eq? l p)
        tail
        (cons (car l) (loop (cdr l))))))

(define (scope-set=? a b)
  (or (eq? a b)
      (and (fx= (scope-set-code a) (scope-set-code b))
           (fx= (scope-set-count a) (scope-set-count b))
           (let loop ([la (scope-set-elems a)] [lb (scope-set-elems b)])
             (or (eq? la lb)
                 (and (eq? (car la) (car lb))
                      (loop (cdr la) (cdr lb))))))))

(define (scope-set-subset? a b)
  (or (eq? a b)
      (and (fx<= (scope-set-count a) (scope-set-count b))
           (let loop ([la (scope-set-elems a)] [na (scope-set-count a)]
                      [lb (scope-set-elems b)] [nb (scope-set-count b)])
             (cond
               [(eq? la lb) #t]
               [(null? la) #t]
               [(fx< nb na) #f]
               [else
                (define x (car la))
                (define y (car lb))
                (cond
                  [(eq? x y) (loop (cdr la) (fx- na 1) (cdr lb) (fx- nb 1))]
                  [(sc> x y) #f]
                  [else (loop la na (cdr lb) (fx- nb 1))])])))))

;; Is `a` a subset of the union of `b` and the scopes in the list
;; `extras`? The `extras` list is expected to be short.
(define (scope-set-subset-of-union? a b extras)
  (cond
    [(null? extras) (scope-set-subset? a b)]
    [else
     (let loop ([la (scope-set-elems a)] [lb (scope-set-elems b)])
       (cond
         [(eq? la lb) #t]
         [(null? la) #t]
         [else
          (define x (car la))
          (cond
            [(memq x extras) (loop (cdr la) lb)]
            [else
             (let b-loop ([lb lb])
               (cond
                 [(null? lb) #f]
                 [(eq? x (car lb)) (loop (cdr la) (cdr lb))]
                 [(sc> x (car lb)) #f]
                 [else (b-loop (cdr lb))]))])]))]))

(define (scope-set-union a b)
  (cond
    [(eq? a b) a]
    [(eqv? 0 (scope-set-count b)) a]
    [(eqv? 0 (scope-set-count a)) b]
    [else
     (for/fold ([a a]) ([sc (in-list (scope-set-elems b))])
       (scope-set-add a sc))]))

(define (scope-set-subtract a b)
  (if (eqv? 0 (scope-set-count b))
      a
      (for/fold ([a a]) ([sc (in-list (scope-set-elems b))])
        (scope-set-remove a sc))))

;; Most recent first
(define (scope-set->list s)
  (scope-set-elems s))

;; Elements of `l` can be in any order
(define (list->scope-set l)
  (for/fold ([s empty-scope-set]) ([sc (in-list l)])
    (scope-set-add s sc)))

;; Iterates most recent first
(define-sequence-syntax in-scope-set
  (lambda () #'scope-set->list)
  (lambda (stx)
    (syntax-case stx ()
      [[(id) (_ s-expr)]
       #'[(id) (in-list (scope-set-elems s-expr))]]
      [_ #f])))

;; ----------------------------------------
;; Scope operations: a list of `(cons scope op)` with `op` as 'add,
;; 'remove, or 'flip, where each scope appears at most once, sorted by
;; decreasing scope id. Lazy propagation records pending operations
;; this way, so that composing two sets of pending operations is a
;; merge and applying them to a scope set is a merge.

;; Result of performing `new` after `cur` (which can be #f), where
;; #f means no operation
(define (compose-op cur new)
  (case new
    [(add) 'add]
    [(remove) 'remove]
    [else (case cur
            [(add) 'remove]
            [(remove) 'add]
            [(flip) #f]
            [else 'flip])]))

(define (scope-ops-ref ops sc)
  (let loop ([ops ops])
    (cond
      [(null? ops) #f]
      [(eq? (caar ops) sc) (cdar ops)]
      [(sc> sc (caar ops)) #f]
      [else (loop (cdr ops))])))

;; Add `op` for `sc` after the operations in `ops`
(define (scope-ops-update ops sc op)
  (let loop ([l ops])
    (cond
      [(or (null? l) (sc> sc (caar l)))
       (prefix-onto ops l (cons (cons sc op) l))]
      [(eq? (caar l) sc)
       (define new-op (compose-op (cdar l) op))
       (cond
         [(eq? new-op (cdar l)) ops]
         [new-op (prefix-onto ops l (cons (cons sc new-op) (cdr l)))]
         [else (prefix-onto ops l (cdr l))])]
      [else (loop (cdr l))])))

;; Operations equivalent to `base` followed by `new`
(define (scope-ops-compose base new)
  (cond
    [(null? new) base]
    [(null? base) new]
    [else
     (let loop ([b base] [n new])
       (cond
         [(null? n) b]
         [(null? b) n]
         [else
          (define bsc (caar b))
          (define nsc (caar n))
          (cond
            [(eq? bsc nsc)
             (define op (compose-op (cdar b) (cdar n)))
             (if op
                 (cons (if (eq? op (cdar b)) (car b) (cons bsc op))
                       (loop (cdr b) (cdr n)))
                 (loop (cdr b) (cdr n)))]
            [(sc> bsc nsc) (cons (car b) (loop (cdr b) n))]
            [else (cons (car n) (loop b (cdr n)))])]))]))

;; Apply scope operations (see above) to a scope set. The result shares the tail of `s`
;; after the oldest scope mentioned by `ops`.
(define (scope-set-apply-ops s ops)
  (cond
    [(null? ops) s]
    [(null? (cdr ops))
     (define sc (caar ops))
     (case (cdar ops)
       [(add) (scope-set-add s sc)]
       [(remove) (scope-set-remove s sc)]
       [else (scope-set-flip s sc)])]
    [else
     (define count (scope-set-count s))
     (define code (scope-set-code s))
     (define new-l
       (let loop ([l (scope-set-elems s)] [ops ops])
         (cond
           [(null? ops) l]
           [else
            (define op (car ops))
            (define sc (car op))
            (cond
              [(and (pair? l) (sc> (car l) sc))
               (define rest (loop (cdr l) ops))
               (if (eq? rest (cdr l))
                   l
                   (cons (car l) rest))]
              [(and (pair? l) (eq? (car l) sc))
               ;; op for a scope in the set
               (cond
                 [(eq? (cdr op) 'add) (loop l (cdr ops))]
                 [else
                  (set! count (fx- count 1))
                  (set! code (fxxor code (scope-base-key sc)))
                  (loop (cdr l) (cdr ops))])]
              [else
               ;; op for a scope not in the set
               (cond
                 [(eq? (cdr op) 'remove) (loop l (cdr ops))]
                 [else
                  (set! count (fx+ count 1))
                  (set! code (fxxor code (scope-base-key sc)))
                  (cons sc (loop l (cdr ops)))])])])))
     (if (eq? new-l (scope-set-elems s))
         s
         (make-set new-l count code))]))
