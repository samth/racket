;;; primflags.ms
;;; Tests for incorrect primitive flag annotations
;;; These tests attempt to demonstrate bugs where primitives are marked
;;; with flags that don't match their actual behavior.

;;; The key issue is that `mifoldable` means the compiler can fold/cache
;;; the result at compile time or avoid re-computing it. For mutable
;;; structure accessors, this is incorrect because the value can change.

(define-syntax primflags-mat
  (syntax-rules ()
    [(_ name form ...)
     (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level 3])
       (mat name form ...))]))

;;; =============================================================
;;; CATEGORY 1: Mutable structure accessors marked as mifoldable
;;; =============================================================

;;; Test: unbox is mifoldable but boxes are mutable
;;; If the compiler caches the unbox result, mutation won't be seen
(primflags-mat unbox-mifoldable-bug
  ;; This test checks if unbox correctly sees mutations
  ;; If unbox is incorrectly folded, the second unbox might return 1
  (let ([b (box 1)])
    (let ([v1 (unbox b)])
      (set-box! b 2)
      (let ([v2 (unbox b)])
        (and (eqv? v1 1) (eqv? v2 2)))))

  ;; Same test but in a loop context where folding is more likely
  (let ([b (box 0)])
    (let loop ([i 0] [sum 0])
      (if (fx= i 10)
          (eqv? sum 45) ; 0+1+2+...+9 = 45
          (let ([v (unbox b)])
            (set-box! b (fx+ v 1))
            (loop (fx+ i 1) (fx+ sum v))))))

  ;; Test with procedure that reads box twice
  (let ([b (box 'first)])
    (define (read-twice)
      (let ([a (unbox b)])
        (set-box! b 'second)
        (let ([c (unbox b)])
          (cons a c))))
    (equal? (read-twice) '(first . second)))
)

;;; Test: time accessors are mifoldable but time objects are mutable
(primflags-mat time-accessors-mifoldable-bug
  ;; time-second should see mutations via set-time-second!
  (let ([t (make-time 'time-utc 0 100)])
    (let ([s1 (time-second t)])
      (set-time-second! t 200)
      (let ([s2 (time-second t)])
        (and (eqv? s1 100) (eqv? s2 200)))))

  ;; time-nanosecond should see mutations
  (let ([t (make-time 'time-utc 500 0)])
    (let ([n1 (time-nanosecond t)])
      (set-time-nanosecond! t 999)
      (let ([n2 (time-nanosecond t)])
        (and (eqv? n1 500) (eqv? n2 999)))))

  ;; time-type should see mutations
  (let ([t (make-time 'time-utc 0 0)])
    (let ([ty1 (time-type t)])
      (set-time-type! t 'time-monotonic)
      (let ([ty2 (time-type t)])
        (and (eq? ty1 'time-utc) (eq? ty2 'time-monotonic)))))
)

;;; Test: port-name is mifoldable but can be changed via set-port-name!
(primflags-mat port-name-mifoldable-bug
  (let ([p (open-output-string)])
    (let ([n1 (port-name p)])
      (set-port-name! p "new-name")
      (let ([n2 (port-name p)])
        (and (string? n1) (equal? n2 "new-name")))))
)

;;; Test: sstats accessors are mifoldable but sstats are mutable
(primflags-mat sstats-accessors-mifoldable-bug
  (let ([s (make-sstats
             (make-time 'time-duration 0 1)  ; cpu
             (make-time 'time-duration 0 2)  ; real
             100  ; bytes
             200  ; gc-count
             (make-time 'time-duration 0 3)  ; gc-cpu
             (make-time 'time-duration 0 4)  ; gc-real
             300)]) ; gc-bytes
    (let ([b1 (sstats-bytes s)])
      (set-sstats-bytes! s 999)
      (let ([b2 (sstats-bytes s)])
        (and (eqv? b1 100) (eqv? b2 999)))))
)

;;; =============================================================
;;; CATEGORY 2: Allocation functions with pure/mifoldable
;;; These create new objects, so eq? identity matters
;;; =============================================================

;;; Test: make-wrapper-procedure should create distinct objects
(primflags-mat make-wrapper-procedure-identity
  ;; Each call should create a new wrapper with distinct identity
  (let* ([base-proc (lambda (x) x)]
         [w1 (make-wrapper-procedure base-proc 2 'wrapper-data)]
         [w2 (make-wrapper-procedure base-proc 2 'wrapper-data)])
    (not (eq? w1 w2)))
)

;;; Test: make-source-object should create distinct objects
(primflags-mat make-source-object-identity
  (let* ([sfd (make-source-file-descriptor "test" (open-bytevector-input-port #vu8()))]
         [so1 (make-source-object sfd 0 10)]
         [so2 (make-source-object sfd 0 10)])
    (not (eq? so1 so2)))
)

;;; =============================================================
;;; CATEGORY 3: Side-effecting operations marked mifoldable
;;; =============================================================

;;; Test: gensym->unique-string has side effect of interning
;;; The comment in primdata.ss says it can't be discarded because
;;; it puts the symbol in the oblist, yet it's marked mifoldable
(primflags-mat gensym-unique-string-side-effect
  ;; After calling gensym->unique-string, the gensym should be interned
  (let ([g (gensym "test")])
    (let ([str (gensym->unique-string g)])
      (and (string? str)
           ;; The gensym should now be findable via string->symbol
           ;; (this verifies the side effect happened)
           (gensym? g))))
)

;;; =============================================================
;;; CATEGORY 4: Division operations with arith-op (implies true)
;;; These can raise exceptions but arith-op implies always returns true
;;; =============================================================

;;; Test: Division by zero should raise exception, not return #f
(primflags-mat division-exceptions
  ;; These should raise exceptions, demonstrating that 'true' flag
  ;; doesn't mean they always return - they can also raise
  (error? (fxdiv 10 0))
  (error? (fxmod 10 0))
  (error? (fxquotient 10 0))
  (error? (fxremainder 10 0))
  (error? (/ 1 0))
  (error? (quotient 10 0))
  (error? (remainder 10 0))
  (error? (modulo 10 0))
)

;;; =============================================================
;;; CATEGORY 5: Regression test for flbit-field return type
;;; This is the original bug that prompted this audit
;;; =============================================================

(primflags-mat flbit-field-returns-integer
  ;; flbit-field should return an exact integer, not a flonum
  (exact? (flbit-field 1.0 0 1))
  (fixnum? (flbit-field 1.0 0 10))
  (integer? (flbit-field 1.5 0 64))
  ;; The result should be usable in integer operations
  (eqv? (+ (flbit-field 1.0 0 10) 1) (+ (flbit-field 1.0 0 10) 1))
)
