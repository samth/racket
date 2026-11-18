#!/usr/bin/env python3
"""Instrument common/scope-set.rkt of the interned-first expander (cd126ece77, the same
expander code as afcaf895da) to record, for scope-set operations, how many interned scopes
(id >= 2^59) the set holds and how many of them an operation walks past.

Recorded kinds:
  update    scope-set-add/remove/flip (also single-op apply-ops)
  member    scope-set-member?
  apply     multi-op scope-set-apply-ops (walk starts at the newest scope in the ops)
  subset    scope-set-subset? when it actually walks (not eq?)
  equal     scope-set=? when codes and counts match (so the elements are walked)
For each: `held` = interned scopes in the set (for subset/equal, in both sets together),
and `walked` = interned scopes passed before reaching the target position. Histograms are
printed to stderr at exit as one `PREFIX-STATS` line of Racket data."""
import sys

p = sys.argv[1]
s = open(p).read()

def rep(old, new):
    global s
    assert s.count(old) == 1, old[:80]
    s = s.replace(old, new)

STATS = r'''
;; ---- instrumentation for the interned-prefix measurement ----
(define stats-table (make-hasheq))
(define (stat! kind metric n)
  (define h (hash-ref stats-table kind #f))
  (define h2 (or h (let ([h (make-hasheq)]) (hash-set! stats-table kind h) h)))
  (define m (or (hash-ref h2 metric #f) (let ([m (make-hasheqv)]) (hash-set! h2 metric m) m)))
  (hash-set! m n (add1 (hash-ref m n 0))))
(define (interned-prefix l)
  (let loop ([l l] [n 0])
    (if (and (pair? l) (>= (scope-base-id (car l)) interned-scope-id-base))
        (loop (cdr l) (add1 n))
        n)))
;; interned scopes in `l` that come before where `sc` is or would be
(define (interned-walked l sc)
  (let loop ([l l] [n 0])
    (if (and (pair? l)
             (>= (scope-base-id (car l)) interned-scope-id-base)
             (> (scope-base-id (car l)) (scope-base-id sc)))
        (loop (cdr l) (add1 n))
        n)))
(define (record! kind l sc)
  (define held (interned-prefix l))
  (stat! kind 'held held)
  (define target-interned? (>= (scope-base-id sc) interned-scope-id-base))
  (stat! kind (if target-interned? 'walked-to-interned 'walked-to-normal) (interned-walked l sc)))
(void (plumber-add-flush! (current-plumber)
                          (lambda (h)
                            (eprintf "PREFIX-STATS ~s\n"
                                     (for/list ([(k h) (in-hash stats-table)])
                                       (cons k (for/list ([(m hist) (in-hash h)])
                                                 (cons m (for/list ([(n c) (in-hash hist)])
                                                           (cons n c))))))))))
'''

rep('''(define-syntax-rule (define-scope-set-update (name s sc) l p
                      #:present present-expr
                      #:absent absent-expr)
  (define (name s sc)
    (define l (scope-set-elems s))
''', '''(define-syntax-rule (define-scope-set-update (name s sc) l p
                      #:present present-expr
                      #:absent absent-expr)
  (define (name s sc)
    (define l (scope-set-elems s))
    (record! 'update l sc)
''')
rep('''(define (scope-set-member? s sc)
  (let loop ([l (scope-set-elems s)])''', '''(define (scope-set-member? s sc)
  (record! 'member (scope-set-elems s) sc)
  (let loop ([l (scope-set-elems s)])''')
rep('''(define (scope-set=? a b)
  (or (eq? a b)
      (and (fx= (scope-set-code a) (scope-set-code b))
           (fx= (scope-set-count a) (scope-set-count b))
''', '''(define (scope-set=? a b)
  (or (eq? a b)
      (and (fx= (scope-set-code a) (scope-set-code b))
           (fx= (scope-set-count a) (scope-set-count b))
           (begin (stat! 'equal 'held (+ (interned-prefix (scope-set-elems a))
                                         (interned-prefix (scope-set-elems b))))
                  #t)
''')
rep('''(define (scope-set-subset? a b)
  (or (eq? a b)
      (let loop''', '''(define (scope-set-subset? a b)
  (or (eq? a b)
      (begin (stat! 'subset 'held (+ (interned-prefix (scope-set-elems a))
                                     (interned-prefix (scope-set-elems b))))
             #f)
      (let loop''')
rep('''    [else
     (define count (scope-set-count s))
     (define code (scope-set-code s))''', '''    [else
     (record! 'apply (scope-set-elems s) (caar ops))
     (define count (scope-set-count s))
     (define code (scope-set-code s))''')
# definitions go after interned-scope-id-base
rep('(define interned-scope-id-base (expt 2 59))\n', '(define interned-scope-id-base (expt 2 59))\n' + STATS)
open(p, 'w').write(s)
print('instrumented', p)
