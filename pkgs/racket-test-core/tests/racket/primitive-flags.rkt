#lang racket/base

;;; primitive-flags.rkt
;;; Tests for incorrect primitive flag annotations in Racket CS
;;; These tests attempt to demonstrate bugs where primitives are marked
;;; with flags that don't match their actual behavior.

(require racket/flonum
         racket/fixnum
         racket/unsafe/ops)

(define tests-passed 0)
(define tests-failed 0)

(define-syntax-rule (test name expr expected)
  (let ([result expr]
        [exp expected])
    (if (equal? result exp)
        (begin
          (set! tests-passed (add1 tests-passed))
          (printf "  PASS: ~a~n" name))
        (begin
          (set! tests-failed (add1 tests-failed))
          (printf "  FAIL: ~a~n    Expected: ~s~n    Got: ~s~n" name exp result)))))

(define-syntax-rule (test-true name expr)
  (test name expr #t))

(define-syntax-rule (test-false name expr)
  (test name expr #f))

;;; =============================================================
;;; CATEGORY 1: Mutation operations marked as folding
;;; flvector-set! and fxvector-set! are marked as known-procedure/folding
;;; but they are mutation operations with side effects
;;; =============================================================

(printf "~n=== Testing mutation operations incorrectly marked as folding ===~n")

;; Test: flvector-set! has side effects (should not be folded away)
(let ([fv (make-flvector 3 0.0)])
  (flvector-set! fv 0 1.0)
  (flvector-set! fv 1 2.0)
  (flvector-set! fv 2 3.0)
  (test "flvector-set! side effects visible"
        (list (flvector-ref fv 0) (flvector-ref fv 1) (flvector-ref fv 2))
        '(1.0 2.0 3.0)))

;; Test: fxvector-set! has side effects (should not be folded away)
(let ([xv (make-fxvector 3 0)])
  (fxvector-set! xv 0 10)
  (fxvector-set! xv 1 20)
  (fxvector-set! xv 2 30)
  (test "fxvector-set! side effects visible"
        (list (fxvector-ref xv 0) (fxvector-ref xv 1) (fxvector-ref xv 2))
        '(10 20 30)))

;; Test: multiple mutations to same index
(let ([fv (make-flvector 1 0.0)])
  (flvector-set! fv 0 1.0)
  (flvector-set! fv 0 2.0)
  (flvector-set! fv 0 3.0)
  (test "flvector-set! last mutation wins"
        (flvector-ref fv 0)
        3.0))

;;; =============================================================
;;; CATEGORY 2: I/O operations marked as folding
;;; char-ready? is marked as known-procedure/folding but it
;;; checks I/O state which is non-deterministic
;;; =============================================================

(printf "~n=== Testing I/O operations incorrectly marked as folding ===~n")

;; Test: char-ready? depends on I/O state, not constant
;; We can't really test the folding bug directly, but we can verify
;; the function behaves correctly with I/O
(let ([in (open-input-string "hello")])
  (test-true "char-ready? returns true for string port with data"
             (char-ready? in))
  (read-char in) ; consume 'h'
  (test-true "char-ready? still true after reading one char"
             (char-ready? in)))

;; Test: char-ready? on empty port
(let ([in (open-input-string "")])
  ;; For string ports, char-ready? should return #t even if at EOF
  ;; because there's no blocking
  (test-true "char-ready? returns true for empty string port (no blocking)"
             (char-ready? in)))

;;; =============================================================
;;; CATEGORY 3: Mutable refs marked as folding
;;; flvector-ref and fxvector-ref are marked as folding, but
;;; the vectors are mutable so the value can change
;;; =============================================================

(printf "~n=== Testing mutable refs incorrectly marked as folding ===~n")

;; Test: flvector-ref should see mutations
(let ([fv (make-flvector 1 1.0)])
  (let ([v1 (flvector-ref fv 0)])
    (flvector-set! fv 0 2.0)
    (let ([v2 (flvector-ref fv 0)])
      (test "flvector-ref sees mutation (v1=1.0)" v1 1.0)
      (test "flvector-ref sees mutation (v2=2.0)" v2 2.0))))

;; Test: fxvector-ref should see mutations
(let ([xv (make-fxvector 1 100)])
  (let ([v1 (fxvector-ref xv 0)])
    (fxvector-set! xv 0 200)
    (let ([v2 (fxvector-ref xv 0)])
      (test "fxvector-ref sees mutation (v1=100)" v1 100)
      (test "fxvector-ref sees mutation (v2=200)" v2 200))))

;; Test: multiple reads interleaved with writes
(let ([fv (make-flvector 1 0.0)])
  (define (read-modify-read delta)
    (let ([before (flvector-ref fv 0)])
      (flvector-set! fv 0 (+ before delta))
      (flvector-ref fv 0)))
  (test "read-modify-read first call" (read-modify-read 1.0) 1.0)
  (test "read-modify-read second call" (read-modify-read 1.0) 2.0)
  (test "read-modify-read third call" (read-modify-read 1.0) 3.0))

;;; =============================================================
;;; CATEGORY 4: Random operations should not be foldable
;;; unsafe-flrandom is marked with folding-unsafe
;;; =============================================================

(printf "~n=== Testing random operations incorrectly marked as foldable ===~n")

;; Test: unsafe-flrandom should return different values
(let ([prng (make-pseudo-random-generator)])
  (let ([r1 (unsafe-flrandom prng)]
        [r2 (unsafe-flrandom prng)]
        [r3 (unsafe-flrandom prng)])
    ;; These should all be different (with very high probability)
    (test-true "unsafe-flrandom returns flonum" (flonum? r1))
    (test-true "unsafe-flrandom returns flonum" (flonum? r2))
    (test-true "unsafe-flrandom returns flonum" (flonum? r3))
    ;; At least two should differ (with overwhelming probability)
    (test-true "unsafe-flrandom is non-deterministic"
               (or (not (= r1 r2))
                   (not (= r2 r3))
                   (not (= r1 r3))))))

;;; =============================================================
;;; CATEGORY 5: unsafe-place-local-set! marked as then-pure
;;; =============================================================

(printf "~n=== Testing place-local mutation marked as then-pure ===~n")

;; Test: unsafe-place-local-set! has side effects
(let ([pl (unsafe-make-place-local 'initial)])
  (test "place-local initial value"
        (unsafe-place-local-ref pl)
        'initial)
  (unsafe-place-local-set! pl 'modified)
  (test "place-local sees mutation"
        (unsafe-place-local-ref pl)
        'modified))

;;; =============================================================
;;; CATEGORY 6: flbit-field return type (the original bug)
;;; =============================================================

(printf "~n=== Testing flbit-field return type ===~n")

;; flbit-field returns an exact integer, not a flonum
(test-true "flbit-field returns exact integer"
           (exact-integer? (flbit-field 1.0 0 1)))

(test-true "flbit-field result is fixnum for small ranges"
           (fixnum? (flbit-field 1.0 0 10)))

;; The result should work in integer arithmetic
(let ([bits (flbit-field 1.0 0 10)])
  (test-true "flbit-field result usable in fx+"
             (fixnum? (fx+ bits 1))))

;;; =============================================================
;;; Summary
;;; =============================================================

(printf "~n=== Test Summary ===~n")
(printf "Passed: ~a~n" tests-passed)
(printf "Failed: ~a~n" tests-failed)

(unless (zero? tests-failed)
  (error 'primitive-flags "~a tests failed" tests-failed))
