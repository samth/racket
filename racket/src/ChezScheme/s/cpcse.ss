;;; cpcse.ss
;;; Copyright 1984-2017 Cisco Systems, Inc.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;; http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

#|
cpcse: common-subexpression elimination plus loop-invariant code motion
on Lsrc, run after cpletrec.

Candidates
 - A candidate is a call to a primitive that has the `pure` flag and an
   immutable result type (numbers, booleans, characters, lengths), or an
   `if` built from candidates, where every leaf is a quoted constant or a
   reference to an unassigned variable.  Since the leaves are immutable
   and the primitives have no effects and read no mutable state, two
   candidates with the same structure always produce the same value.
 - A candidate may still raise an exception (e.g., `real->flonum` on a
   non-real) unless every primitive in it is `unrestricted`.

CSE
 - When `(let ([x e]) body)` binds an unassigned `x` to a candidate `e`,
   occurrences of `e` within `body` are replaced by `x`.  The let
   dominates its body, so `e` has already been computed.

LICM
 - Loops are recognized as in np-recognize-loops:
     (letrec ([f (lambda (x ...) body)]) (f e ...))
   where `f` is unassigned and is referenced only in operator position.
 - Inner loops are processed first, so code hoisted out of an inner loop
   lands in the outer loop's body, where it can be hoisted again.
 - A candidate in `body` is invariant when none of its variables are
   bound inside the loop.  An invariant candidate is hoisted to a
   binding just before the `letrec` if the first activation of `f`
   evaluates it before doing anything that has an effect or can raise.
   This preserves both side effects and the order of errors, since the
   value is computed at the same point the first iteration would have
   computed it, and later iterations compute the same value.
 - Loop inversion: candidates under an `if` can still be hoisted when
   the test is invariant, or when the test is evaluated at the start of
   the first iteration and its other branch leaves the loop (does not
   call `f`).  The latter is the usual shape of a `for` loop:
     (letrec ([f (lambda (i) (if (< i n) (begin body (f (+ i 1))) (void)))])
       (f 0))
   Instead of copying the loop's exit code to build an inverted loop,
   the copy of the test (with the loop's initial arguments substituted
   for its parameters) is used as a guard on the hoisted binding:
     (let ([h (if (< 0 n) e #f)]) (letrec ...))
   If the guard is false, the loop exits on its first iteration and `h`
   is never used.  If it is true, the first iteration evaluates `e`.
 - When `enable-cse-licm` is 'reorder-errors, primitive calls that can
   raise are treated like those that cannot when deciding whether later
   code may be hoisted past them.  This can change which of two errors
   is reported, but never whether an error is reported.
|#

(begin
(define enable-cse-licm
  ($make-thread-parameter
    ; for experiments: CHEZ_CSE_LICM=0 disables the pass, =reorder allows error reordering
    (let ([e (getenv "CHEZ_CSE_LICM")])
      (cond
        [(not e) #t]
        [(string=? e "0") #f]
        [(string=? e "reorder") 'reorder-errors]
        [else #t]))
    (lambda (x) (if (eq? x 'reorder-errors) x (and x #t)))))

(define $cpcse
  (let ()
    (import (nanopass))
    (include "base-lang.ss")

    (define cse-result-types
      '(boolean char fixnum ufixnum pfixnum nfixnum maybe-fixnum sint uint integer
        exact-real exact-number real number flonum cflonum inexact-number $inexactnum
        length))

    ; pure, but the result can be changed by a mutation (string-truncate!)
    (define cse-excluded-prims '(string-length))

    (define cse-prim?
      (lambda (pr)
        (let ([flags (primref-flags pr)] [name (primref-name pr)])
          (and (all-set? (prim-mask pure) flags)
               (not (all-set? (prim-mask abort-op) flags))
               (memq ($sgetprop name '*result-type* #f) cse-result-types)
               (not (memq name cse-excluded-prims))))))

    ;; immutable-car and immutable-cdr promise that the pair is not mutated
    ;; except where it could be replaced by a fresh pair with the new
    ;; contents, which a mutation visible in the same unit stands for. When
    ;; the unit has no set-car! or set-cdr!, their reads can be kept across
    ;; calls and hoisted like pure primitives (they can still raise when
    ;; not unchecked).  They are not speculated (see below): the guarded
    ;; copy keeps an extra variable live across the calls in the loop,
    ;; which costs more than the reads it saves.
    (define-threaded immutable-ok?)

    (define immutable-read?
      (lambda (pr)
        (and immutable-ok?
             (memq (primref-name pr) '(immutable-car immutable-cdr))
             #t)))

    (define mutates-pairs?
      (lambda (e)
        (call/cc
          (lambda (k)
            (define-pass find-mutation : Lsrc (ir) -> Lsrc ()
              (Expr : Expr (ir) -> Expr ()
                [,pr (when (memq (primref-name pr) '(set-car! set-cdr! car-cas! cdr-cas!)) (k #t)) ir]))
            (find-mutation e)
            #f))))

    ; a call to pr has no side effects and, unless `reorder?`, cannot raise
    (define transparent-prim?
      (lambda (pr reorder?)
        (let ([flags (primref-flags pr)])
          (and (all-set? (prim-mask discard) flags)
               (or reorder?
                   (all-set? (prim-mask unsafe) flags)
                   (all-set? (prim-mask unrestricted) flags))))))

    (define flonum-result?
      (lambda (pr)
        (eq? ($sgetprop (primref-name pr) '*result-type* #f) 'flonum)))

    (define flonum-cand?
      (lambda (e)
        (nanopass-case (Lsrc Expr) e
          [(quote ,d) (flonum? d)]
          [(call ,preinfo ,pr ,e* ...) (flonum-result? pr)]
          [(if ,e0 ,e1 ,e2) (and (flonum-cand? e1) (flonum-cand? e2))]
          [else #f])))

    (define simple-datum?
      (lambda (d)
        (or (fixnum? d) (flonum? d) (char? d) (boolean? d) (null? d) (symbol? d))))

    (define add-ref!
      (lambda (x)
        (when (prelex-referenced x) (set-prelex-multiply-referenced! x #t))
        (set-prelex-referenced! x #t)))

    (define build-ref
      (lambda (x)
        (add-ref! x)
        (with-output-language (Lsrc Expr) `(ref #f ,x))))

    (define build-let
      (lambda (x e body)
        (with-output-language (Lsrc Expr)
          `(call ,(make-preinfo-call)
             (case-lambda ,(make-preinfo-lambda)
               (clause (,x) 1 ,body))
             ,e))))

    ;; Structural key for a candidate, or #f.  Leaves are prelexes and
    ;; (quote . datum); keys are compared with equal?.  The key omits the
    ;; primitive's level, since cptypes can make a later copy of a
    ;; checked operation unchecked, and either computes the same value.
    (define cand-key
      (lambda (e)
        (nanopass-case (Lsrc Expr) e
          [(quote ,d) (and (simple-datum? d) (cons 'quote d))]
          [(ref ,maybe-src ,x) (and (not (prelex-assigned x)) x)]
          [(call ,preinfo ,pr ,e* ...)
           (and (or (cse-prim? pr) (immutable-read? pr))
                (let loop ([e* e*] [rk* '()])
                  (if (null? e*)
                      (cons (primref-name pr) (reverse rk*))
                      (let ([k (cand-key (car e*))])
                        (and k (loop (cdr e*) (cons k rk*)))))))]
          [(if ,e0 ,e1 ,e2)
           (let ([k0 (cand-key e0)])
             (and k0
                  (let ([k1 (cand-key e1)])
                    (and k1
                         (let ([k2 (cand-key e2)])
                           (and k2 (list 'if k0 k1 k2)))))))]
          [else #f])))

    ; a non-trivial candidate: something worth naming
    (define compound?
      (lambda (e)
        (nanopass-case (Lsrc Expr) e
          [(call ,preinfo ,pr ,e* ...) #t]
          [(if ,e0 ,e1 ,e2) #t]
          [else #f])))

    (define cand-for-all
      ; applies `ok?` to each variable and each primref in candidate `e`
      (lambda (e var-ok? pr-ok?)
        (let f ([e e])
          (nanopass-case (Lsrc Expr) e
            [(quote ,d) #t]
            [(ref ,maybe-src ,x) (var-ok? x)]
            [(call ,preinfo ,pr ,e* ...) (and (pr-ok? pr) (andmap f e*))]
            [(if ,e0 ,e1 ,e2) (and (f e0) (f e1) (f e2))]
            [else #f]))))

    (define avail-lookup
      (lambda (e avail)
        (and (compound? e)
             (let ([k (cand-key e)])
               (and k
                    (let ([a (assoc k avail)])
                      (and a (build-ref (cdr a)))))))))

    ;; Each binder is mapped to the list of loops (innermost first) that
    ;; enclose it.  A variable is invariant in loop L when L is not in
    ;; that list.
    (define-threaded binder-loops)

    (define record-binder!
      (lambda (x loops)
        (eq-hashtable-set! binder-loops x loops)))

    ;; A let-bound variable whose right-hand side is a variable or a
    ;; candidate is an alias for that right-hand side (e.g., the temporary
    ;; that cp0 introduces when inlining `real->flonum`).  It is invariant
    ;; in a loop when its right-hand side is, even if bound in the loop.
    (define-threaded aliases)

    (define invariant-in?
      (lambda (x loop)
        (and (not (prelex-assigned x))
             (or (not (memq loop (eq-hashtable-ref binder-loops x '())))
                 (let ([a (eq-hashtable-ref aliases x #f)])
                   (and a (cand-for-all a (lambda (y) (invariant-in? y loop)) (lambda (pr) #t))))))))

    (define bound-in-loop-alias
      ; the alias right-hand side to use for x outside `loop`, or #f
      (lambda (x loop)
        (and (memq loop (eq-hashtable-ref binder-loops x '()))
             (eq-hashtable-ref aliases x #f))))

    (define references?
      ; does e reference x?
      (lambda (e x)
        (call/cc
          (lambda (k)
            (define-pass find-ref : Lsrc (ir) -> Lsrc ()
              (Expr : Expr (ir) -> Expr ()
                [(ref ,maybe-src ,x2) (when (eq? x2 x) (k #t)) ir]))
            (find-ref e)
            #f))))

    (define only-called?
      ; is f referenced only in operator position within e?
      (lambda (e f)
        (call/cc
          (lambda (k)
            (define-pass check : Lsrc (ir) -> Lsrc ()
              (Expr : Expr (ir) -> Expr ()
                [(ref ,maybe-src ,x) (when (eq? x f) (k #f)) ir]
                [(call ,preinfo (ref ,maybe-src ,x) ,[e*] ...)
                 (guard (eq? x f))
                 ir]))
            (check e)
            #t))))

    (define-record-type hguard
      (nongenerative)
      (sealed #t)
      ; test: Lsrc candidate; then?: candidate is in the then branch;
      ; first?: test refers to loop parameters, which must be replaced
      ; by the loop's initial arguments
      (fields test then? first?))

    (define-record-type hoist
      (nongenerative)
      (sealed #t)
      (fields node guards key (mutable var)))

    (define guards-key
      (lambda (guards)
        (map (lambda (g) (cons (hguard-then? g) (cand-key (hguard-test g)))) guards)))

    ;; Walk the part of a loop body that the first activation evaluates
    ;; before any effect, collecting invariant candidates in evaluation
    ;; order.  Returns #t if evaluating `e` (minus the hoisted parts) has
    ;; no effect and cannot raise, so that the walk may continue past it.
    (define collect-hoists
      (lambda (body loop params reorder?)
        (define rhoist* '())
        (define invariant?
          (lambda (e)
            (and (compound? e)
                 (cand-key e)
                 (cand-for-all e (lambda (x) (invariant-in? x loop)) (lambda (pr) #t)))))
        (define param-or-invariant?
          (lambda (x) (or (memq x params) (invariant-in? x loop))))
        (define transparent?
          (lambda (e)
            (nanopass-case (Lsrc Expr) e
              [(quote ,d) #t]
              [(ref ,maybe-src ,x) #t]
              [,pr #t]
              [(case-lambda ,preinfo ,cl* ...) #t]
              [(profile ,src) #t]
              [(pariah) #t]
              [(seq ,e0 ,e1) (and (transparent? e0) (transparent? e1))]
              [(if ,e0 ,e1 ,e2) (and (transparent? e0) (transparent? e1) (transparent? e2))]
              [(call ,preinfo ,pr ,e* ...)
               (and (transparent-prim? pr reorder?) (andmap transparent? e*))]
              [(call ,preinfo0 (case-lambda ,preinfo1 (clause (,x* ...) ,interface ,body)) ,e* ...)
               (guard (fx= interface (length e*)))
               (and (andmap transparent? e*) (transparent? body))]
              [else #f])))
        (define walk*
          ; e* are evaluated in an unspecified order, as for call arguments.
          ; Choose an order that puts arguments that are transparent after
          ; hoisting first, then one other argument whose hoists are kept,
          ; then the rest (whose hoists are dropped).
          (lambda (e* guards)
            (let ([saved rhoist*])
              (let loop ([e* e*] [rtrans '()] [ropaque '()])
                (if (null? e*)
                    (let* ([trans (reverse rtrans)]
                           [opaque (reverse ropaque)]
                           [chosen (let ([with (filter pair? opaque)])
                                     (if (null? with) '() (car with)))])
                      ; rhoist* is kept latest-first
                      (set! rhoist* (append chosen (apply append (reverse trans)) saved))
                      (null? opaque))
                    (begin
                      (set! rhoist* '())
                      (let ([t (walk (car e*) guards)])
                        (if t
                            (loop (cdr e*) (cons rhoist* rtrans) ropaque)
                            (loop (cdr e*) rtrans (cons rhoist* ropaque))))))))))
        (define walk-if
          (lambda (e0 e1 e2 guards)
            ; a guard's test is copied to the preheader and evaluated there,
            ; at the point where the first iteration evaluates it, so it may
            ; raise; it only has to be free of other effects
            (let ([t0 (walk e0 guards)])
              (cond
                [(and (cand-key e0)
                      (cand-for-all e0 (lambda (x) (invariant-in? x loop)) (lambda (pr) #t)))
                 ; invariant test: the same branch is taken on every iteration
                 (let* ([t1 (walk e1 (cons (make-hguard e0 #t #f) guards))]
                        [t2 (walk e2 (cons (make-hguard e0 #f #f) guards))])
                   (and t0 t1 t2))]
                [(and (cand-key e0)
                      (cand-for-all e0 param-or-invariant? (lambda (pr) #t))
                      (let ([r1 (references? e1 loop)] [r2 (references? e2 loop)])
                        (and (not (eq? r1 r2)) (if r1 'then 'else))))
                 =>
                 (lambda (which)
                   ; first-iteration test: the other branch leaves the loop
                   (if (eq? which 'then)
                       (walk e1 (cons (make-hguard e0 #t #t) guards))
                       (walk e2 (cons (make-hguard e0 #f #t) guards)))
                   #f)]
                [else (and t0 (transparent? e1) (transparent? e2))]))))
        (define walk
          (lambda (e guards)
            (if (invariant? e)
                (begin
                  (set! rhoist* (cons (make-hoist e guards (cons (guards-key guards) (cand-key e)) #f) rhoist*))
                  #t)
                (nanopass-case (Lsrc Expr) e
                  [(quote ,d) #t]
                  [(ref ,maybe-src ,x) #t]
                  [,pr #t]
                  [(case-lambda ,preinfo ,cl* ...) #t]
                  [(profile ,src) #t]
                  [(pariah) #t]
                  [(seq ,e0 ,e1)
                   ; a candidate whose value is unused is kept only for its
                   ; possible error; leave it in place, as a benchmark may
                   ; rely on it (see `black-box`)
                   (and (if (invariant? e0) (transparent? e0) (walk e0 guards))
                        (walk e1 guards))]
                  [(if ,e0 ,e1 ,e2) (walk-if e0 e1 e2 guards)]
                  [(call ,preinfo ,pr ,e* ...)
                   (guard (eq? (primref-name pr) 'black-box))
                   ; black-box asks for its argument to be computed where it is
                   #f]
                  [(call ,preinfo ,pr ,e* ...)
                   (and (walk* e* guards) (transparent-prim? pr reorder?))]
                  [(call ,preinfo0 (case-lambda ,preinfo1 (clause (,x* ...) ,interface ,body)) ,e* ...)
                   (guard (fx= interface (length e*)))
                   (and (walk* e* guards) (walk body guards))]
                  [(call ,preinfo ,e0 ,e* ...)
                   (walk* (cons e0 e*) guards)
                   #f]
                  [(letrec ([,x* ,e*] ...) ,body)
                   (and (andmap transparent? e*) (walk body guards))]
                  [(letrec* ([,x* ,e*] ...) ,body)
                   (and (andmap transparent? e*) (walk body guards))]
                  [else #f]))))
        (walk body '())
        (reverse rhoist*)))

    ;; Speculation: the walk above stops at the first operation that may
    ;; have an effect, such as a call to an inner loop, so invariant code
    ;; after that point stays in the loop.  Generic arithmetic there can
    ;; still be computed before the loop under a guard that checks its
    ;; operands, so that the computation cannot raise, keeping the
    ;; original as a fallback:
    ;;   (let ([s (if (number? y) (#3%* x y) #f)])
    ;;     (letrec ([f (lambda (i) ... (if s s (* x y)) ...)]) ...))
    ;; If the guard fails, `s` is #f and the loop computes the original
    ;; expression at its original point, so errors are unchanged.
    ;; Generic `+`, `-`, and `*` need numbers; `$fxx+` and `$fxx*` need
    ;; fixnums, so their operands must be variables or constants, since
    ;; the result of a nested call may be a bignum.
    (define spec-number-prims '(+ - *))
    (define spec-fixnum-prims '($fxx+ $fxx*))

    (define spec-leaves
      ; an alist mapping each variable that the guard for `e` must check
      ; to 'fixnum or 'number, or #f if `e` cannot be speculated
      (lambda (e)
        (let f ([e e] [need 'number] [vars '()])
          (and vars
               (nanopass-case (Lsrc Expr) e
                 [(quote ,d) (and (if (eq? need 'fixnum) (fixnum? d) (number? d)) vars)]
                 [(ref ,maybe-src ,x)
                  (let ([a (assq x vars)])
                    (cond
                      [(not a) (cons (cons x need) vars)]
                      [(and (eq? need 'fixnum) (eq? (cdr a) 'number))
                       (cons (cons x 'fixnum) (remq a vars))]
                      [else vars]))]
                 [(call ,preinfo ,pr ,e* ...)
                  (let ([name (primref-name pr)])
                    (and (eq? need 'number)
                         (pair? e*)
                         (cond
                           [(memq name spec-number-prims)
                            (fold-left (lambda (vars e) (f e 'number vars)) vars e*)]
                           [(memq name spec-fixnum-prims)
                            (fold-left (lambda (vars e) (f e 'fixnum vars)) vars e*)]
                           [else #f])))]
                 [else #f])))))

    (define collect-speculations
      ; returns a list of (node . vars) for invariant generic arithmetic
      ; in `body`, not looking inside closures or inner loops
      (lambda (body loop)
        (define found '())
        (define outside?
          (lambda (x)
            (and (not (prelex-assigned x))
                 (not (memq loop (eq-hashtable-ref binder-loops x '()))))))
        (define spec-vars
          (lambda (e)
            (nanopass-case (Lsrc Expr) e
              [(call ,preinfo ,pr ,e* ...)
               (let ([vars (spec-leaves e)])
                 (and vars (andmap (lambda (a) (outside? (car a))) vars) vars))]
              [else #f])))
        (let walk ([e body])
          (let ([vars (spec-vars e)])
            (if vars
                (set! found (cons (cons e vars) found))
                (nanopass-case (Lsrc Expr) e
                  [(call ,preinfo ,pr ,e* ...)
                   (guard (eq? (primref-name pr) 'black-box))
                   (void)]
                  [(call ,preinfo0 (case-lambda ,preinfo1 (clause (,x* ...) ,interface ,body)) ,e* ...)
                   (guard (fx= interface (length e*)))
                   (for-each walk e*)
                   (walk body)]
                  [(call ,preinfo ,e0 ,e* ...) (walk e0) (for-each walk e*)]
                  [(if ,e0 ,e1 ,e2) (walk e0) (walk e1) (walk e2)]
                  [(seq ,e0 ,e1) (walk e0) (walk e1)]
                  [(set! ,maybe-src ,x ,e) (walk e)]
                  [(letrec ([,x* ,e*] ...) ,body) (for-each walk e*) (walk body)]
                  [(letrec* ([,x* ,e*] ...) ,body) (for-each walk e*) (walk body)]
                  [else (void)]))))
        (reverse found)))

    (define build-speculation
      ; the guarded, unchecked copy of `node` to compute before the loop
      (lambda (node vars)
        (with-output-language (Lsrc Expr)
          (let ([rhs (let f ([e node])
                       (nanopass-case (Lsrc Expr) e
                         [(quote ,d) `(quote ,d)]
                         [(ref ,maybe-src ,x) (build-ref x)]
                         [(call ,preinfo ,pr ,e* ...)
                          `(call ,(make-preinfo-call) ,($sgetprop (primref-name pr) '*prim3* #f)
                             ,(map f e*) ...)]
                         [else (sorry! 'cpcse "unexpected speculation ~s" e)]))])
            (fold-left
              (lambda (rhs a)
                (let ([pred (if (eq? (cdr a) 'fixnum) 'fixnum? 'number?)])
                  `(if (call ,(make-preinfo-call) ,($sgetprop pred '*prim3* #f) ,(build-ref (car a)))
                       ,rhs
                       (quote #f))))
              rhs vars)))))

    (define-pass replace-speculated : Lsrc (ir node-ht) -> Lsrc ()
      (Expr : Expr (ir) -> Expr ()
        [(call ,preinfo ,e0 ,e1 ...)
         (let ([s (eq-hashtable-ref node-ht ir #f)])
           (if s
               `(if ,(build-ref s) ,(build-ref s) ,ir)
               `(call ,preinfo ,(Expr e0) ,(map Expr e1) ...)))])
      (Expr ir))

    (define speculate
      ; returns the speculation bindings for the loop `f`, as a list of
      ; (var . rhs), and the body with speculated nodes replaced
      (lambda (body f loops)
        (let ([found (collect-speculations body f)])
          (if (null? found)
              (values '() body)
              (let ([node-ht (make-eq-hashtable)])
                (let loop ([found found] [seen '()] [rbind* '()])
                  (if (null? found)
                      (values (reverse rbind*) (replace-speculated body node-ht))
                      (let* ([node (caar found)] [k (cand-key node)] [a (and k (assoc k seen))])
                        (if a
                            (begin
                              (eq-hashtable-set! node-ht node (cdr a))
                              (loop (cdr found) seen rbind*))
                            (let ([s (make-prelex* 'spec)])
                              (record-binder! s loops)
                              (eq-hashtable-set! node-ht node s)
                              (loop (cdr found)
                                    (if k (cons (cons k s) seen) seen)
                                    (cons (cons s (build-speculation node (cdar found))) rbind*))))))))))))

    ;; Replace hoisted nodes (by identity) and, for hoists with no guards,
    ;; any candidate with the same key.
    (define-pass replace-hoisted : Lsrc (ir node-ht key-alist) -> Lsrc ()
      (definitions
        (define lookup
          (lambda (ir)
            (let ([x (eq-hashtable-ref node-ht ir #f)])
              (if x
                  (build-ref x)
                  (and (not (null? key-alist))
                       (avail-lookup ir key-alist)))))))
      (Expr : Expr (ir) -> Expr ()
        [(call ,preinfo ,pr ,e1 ...)
         (guard (eq? (primref-name pr) 'black-box))
         ir]
        [(call ,preinfo ,e0 ,e1 ...)
         (or (lookup ir)
             `(call ,preinfo ,(Expr e0) ,(map Expr e1) ...))]
        [(if ,e0 ,e1 ,e2)
         (or (lookup ir)
             `(if ,(Expr e0) ,(Expr e1) ,(Expr e2)))])
      (Expr ir))

    (define copy-guard-test
      ; copy a guard's test, replacing hoisted nodes and, for a
      ; first-iteration guard, loop parameters by initial arguments
      (lambda (e node-ht subst loop)
        (with-output-language (Lsrc Expr)
          (let f ([e e])
            (let ([x (eq-hashtable-ref node-ht e #f)])
              (if x
                  (build-ref x)
                  (nanopass-case (Lsrc Expr) e
                    [(quote ,d) `(quote ,d)]
                    [(ref ,maybe-src ,x)
                     (guard (bound-in-loop-alias x loop))
                     (f (bound-in-loop-alias x loop))]
                    [(ref ,maybe-src ,x)
                     (let ([a (assq x subst)])
                       (if a
                           (let ([arg (cdr a)])
                             (nanopass-case (Lsrc Expr) arg
                               [(ref ,maybe-src ,x1) (build-ref x1)]
                               [(quote ,d) `(quote ,d)]
                               [else (sorry! 'cpcse "unexpected initial argument ~s" arg)]))
                           (build-ref x)))]
                    [(call ,preinfo ,pr ,e* ...)
                     `(call ,(make-preinfo-call) ,pr ,(map f e*) ...)]
                    [(if ,e0 ,e1 ,e2) `(if ,(f e0) ,(f e1) ,(f e2))]
                    [else (sorry! 'cpcse "unexpected guard test ~s" e)])))))))

    (define trivial-arg?
      (lambda (e)
        (nanopass-case (Lsrc Expr) e
          [(quote ,d) #t]
          [(ref ,maybe-src ,x) (not (prelex-assigned x))]
          [else #f])))

    (define licm
      ; f, x*, body, e*: the loop, already processed; loops: the loops
      ; enclosing this one; returns the rebuilt expression
      (lambda (f preinfo1 x* interface body preinfo2 maybe-src e* loops reorder?)
        (define rebuild
          (lambda (body e*)
            (let-values ([(bind* body) (speculate body f loops)])
              (fold-right
                (lambda (b e) (build-let (car b) (cdr b) e))
                (with-output-language (Lsrc Expr)
                  `(letrec ([,f (case-lambda ,preinfo1 (clause (,x* ...) ,interface ,body))])
                     (call ,preinfo2 (ref ,maybe-src ,f) ,e* ...)))
                bind*))))
        (let ([hoist* (collect-hoists body f x* reorder?)])
          (if (null? hoist*)
              (rebuild body e*)
              (let ([node-ht (make-eq-hashtable)])
                ; one variable per distinct (guards, candidate) pair
                (let loop ([hoist* hoist*] [seen '()] [rvar* '()])
                  (if (null? hoist*)
                      (void)
                      (let* ([h (car hoist*)] [a (assoc (hoist-key h) seen)])
                        (if a
                            (begin
                              (eq-hashtable-set! node-ht (hoist-node h) (cdr a))
                              (loop (cdr hoist*) seen rvar*))
                            (let ([t (make-prelex* 'licm)])
                              (record-binder! t loops)
                              (hoist-var-set! h t)
                              (eq-hashtable-set! node-ht (hoist-node h) t)
                              (loop (cdr hoist*) (cons (cons (hoist-key h) t) seen) (cons t rvar*)))))))
                (let* ([first-hoist* (filter hoist-var hoist*)]
                       ; bind non-trivial initial arguments first, to keep their effects first
                       [tmp* (map (lambda (e) (and (not (trivial-arg? e)) (make-prelex* 'tloop))) e*)]
                       [arg* (map (lambda (e t) (if t (with-output-language (Lsrc Expr) `(ref #f ,t)) e)) e* tmp*)]
                       [subst (map cons x* arg*)]
                       [key-alist (fold-left
                                    (lambda (al h)
                                      ; an unguarded hoist is computed whenever the loop is entered
                                      (if (null? (hoist-guards h))
                                          (cons (cons (cand-key (hoist-node h)) (hoist-var h)) al)
                                          al))
                                    '() first-hoist*)]
                       [body (replace-hoisted body node-ht key-alist)]
                       [rhs* (map (lambda (h)
                                    (let ([node (hoist-node h)])
                                      (fold-left
                                        (lambda (rhs g)
                                          (let ([test (copy-guard-test (hguard-test g) node-ht
                                                        (if (hguard-first? g) subst '()) f)]
                                                ; keep a flonum binding known to be a flonum, so it can be unboxed
                                                [dummy (with-output-language (Lsrc Expr)
                                                         (if (flonum-cand? node) `(quote 0.0) `(quote #f)))])
                                            (with-output-language (Lsrc Expr)
                                              (if (hguard-then? g)
                                                  `(if ,test ,rhs ,dummy)
                                                  `(if ,test ,dummy ,rhs)))))
                                        ; the node's own references were already counted
                                        (replace-hoisted-node node node-ht f)
                                        (hoist-guards h))))
                               first-hoist*)])
                  (for-each (lambda (t) (when t (record-binder! t loops) (add-ref! t))) tmp*)
                  (let ([e (fold-right
                             (lambda (h rhs e) (build-let (hoist-var h) rhs e))
                             (rebuild body arg*)
                             first-hoist* rhs*)])
                    (fold-right
                      (lambda (t init e) (if t (build-let t init e) e))
                      e tmp* e*))))))))

    (define replace-hoisted-node
      ; a hoisted node may contain earlier hoisted nodes (e.g., when a
      ; guarded hoist's candidate contains an unguarded one), and aliases
      ; bound inside the loop, which are replaced by their right-hand sides
      (lambda (node node-ht loop)
        (with-output-language (Lsrc Expr)
          (let f ([e node] [top? #t] [copy? #f])
            ; copy?: e is part of an alias's right-hand side, which stays
            ; in the loop too, so its references are new
            (let ([x (and (not top?) (eq-hashtable-ref node-ht e #f))])
              (if x
                  (build-ref x)
                  (nanopass-case (Lsrc Expr) e
                    [(ref ,maybe-src ,x1)
                     (let ([a (bound-in-loop-alias x1 loop)])
                       (cond
                         [a (f a #f #t)]
                         [copy? (build-ref x1)]
                         [else e]))]
                    [(call ,preinfo ,pr ,e* ...)
                     `(call ,preinfo ,pr ,(map (lambda (e) (f e #f copy?)) e*) ...)]
                    [(if ,e0 ,e1 ,e2)
                     `(if ,(f e0 #f copy?) ,(f e1 #f copy?) ,(f e2 #f copy?))]
                    [else e])))))))

    (define-pass cpcse : Lsrc (ir reorder?) -> Lsrc ()
      (definitions
        (define loop?
          (lambda (x1 x2 interface e* body)
            (and (eq? x1 x2)
                 (fx= interface (length e*))
                 (not (prelex-assigned x1))
                 (only-called? body x1))))
        (define do-loop
          (lambda (x1 preinfo1 x* interface body preinfo2 maybe-src e* avail loops)
            (let ([e* (map (lambda (e) (Expr e avail loops)) e*)])
              (let ([inner (cons x1 loops)])
                (record-binder! x1 loops)
                (for-each (lambda (x) (record-binder! x inner)) x*)
                (let ([body (Expr body avail inner)])
                  (licm x1 preinfo1 x* interface body preinfo2 maybe-src e* loops reorder?)))))))
      (Expr : Expr (ir avail loops) -> Expr ()
        [(letrec ([,x1 (case-lambda ,preinfo1 (clause (,x* ...) ,interface ,body))])
           (call ,preinfo2 (ref ,maybe-src ,x2) ,e* ...))
         (guard (loop? x1 x2 interface e* body))
         (do-loop x1 preinfo1 x* interface body preinfo2 maybe-src e* avail loops)]
        ; named-let shape, as produced by cp0
        [(call ,preinfo2 (letrec ([,x1 (case-lambda ,preinfo1 (clause (,x* ...) ,interface ,body))])
                           (ref ,maybe-src ,x2))
           ,e* ...)
         (guard (loop? x1 x2 interface e* body))
         (do-loop x1 preinfo1 x* interface body preinfo2 maybe-src e* avail loops)]
        [(call ,preinfo0 (case-lambda ,preinfo1 (clause (,x* ...) ,interface ,body)) ,e* ...)
         (guard (fx= interface (length e*)))
         (let ([e* (map (lambda (e) (Expr e avail loops)) e*)])
           (for-each (lambda (x) (record-binder! x loops)) x*)
           (for-each (lambda (x e)
                       (when (and (not (prelex-assigned x))
                                  (nanopass-case (Lsrc Expr) e
                                    [(ref ,maybe-src ,x1) (not (prelex-assigned x1))]
                                    [else (and (compound? e) (cand-key e) #t)]))
                         (eq-hashtable-set! aliases x e)))
             x* e*)
           (let ([body-avail (fold-left
                               (lambda (avail x e)
                                 (if (and (not (prelex-assigned x)) (compound? e))
                                     (let ([k (cand-key e)])
                                       (if k (cons (cons k x) avail) avail))
                                     avail))
                               avail x* e*)])
             `(call ,preinfo0 (case-lambda ,preinfo1 (clause (,x* ...) ,interface ,(Expr body body-avail loops)))
                ,e* ...)))]
        [(call ,preinfo ,pr ,e* ...)
         (guard (eq? (primref-name pr) 'black-box))
         ; no CSE for black-box's argument
         `(call ,preinfo ,pr ,(map (lambda (e) (Expr e '() loops)) e*) ...)]
        [(call ,preinfo ,pr ,e* ...)
         (let ([e `(call ,preinfo ,pr ,(map (lambda (e) (Expr e avail loops)) e*) ...)])
           (or (avail-lookup e avail) e))]
        [(call ,preinfo ,e0 ,e* ...)
         `(call ,preinfo ,(Expr e0 avail loops) ,(map (lambda (e) (Expr e avail loops)) e*) ...)]
        [(if ,e0 ,e1 ,e2)
         (let ([e `(if ,(Expr e0 avail loops) ,(Expr e1 avail loops) ,(Expr e2 avail loops))])
           (or (avail-lookup e avail) e))]
        [(case-lambda ,preinfo ,cl* ...)
         `(case-lambda ,preinfo ,(map (lambda (cl) (CaseLambdaClause cl avail loops)) cl*) ...)]
        [(letrec ([,x* ,e*] ...) ,body)
         (for-each (lambda (x) (record-binder! x loops)) x*)
         `(letrec ([,x* ,(map (lambda (e) (Expr e avail loops)) e*)] ...) ,(Expr body avail loops))]
        [(letrec* ([,x* ,e*] ...) ,body)
         (for-each (lambda (x) (record-binder! x loops)) x*)
         `(letrec* ([,x* ,(map (lambda (e) (Expr e avail loops)) e*)] ...) ,(Expr body avail loops))])
      (CaseLambdaClause : CaseLambdaClause (ir avail loops) -> CaseLambdaClause ()
        [(clause (,x* ...) ,interface ,body)
         (for-each (lambda (x) (record-binder! x loops)) x*)
         `(clause (,x* ...) ,interface ,(Expr body avail loops))])
      (Expr ir '() '()))

    (lambda (x)
      (let ([mode (enable-cse-licm)])
        (if mode
            (fluid-let ([binder-loops (make-eq-hashtable)]
                        [immutable-ok? (not (mutates-pairs? x))]
                        [aliases (make-eq-hashtable)])
              (cpcse x (eq? mode 'reorder-errors)))
            x)))))
)
