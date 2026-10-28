;;; cpcoverage.ss
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

;;; cpcoverage inserts coverage counters for coverage-guided fuzzers
;;; such as libFuzzer when `generate-coverage-counters` is true. Each
;;; lambda clause body and each arm of an `if` gets a call
;;; `($coverage-hit i)`, which cpprim inlines as an increment of the
;;; 8-bit counter `i` in the bytevector that is the value of
;;; `$coverage-counters`, as with clang's `-fsanitize-coverage=inline-8bit-counters`.
;;; A program that runs such code must first call
;;; `Senable_coverage_counters`; the fuzzer registers the counters with
;;; `Scoverage_counters`.
;;;
;;; Counter indices are a hash of the expression being compiled and the
;;; position of the counter in it, so that separately compiled code
;;; mostly uses different counters, and so that compiling the same code
;;; twice gives the same indices.

(define generate-coverage-counters ($make-thread-parameter #f (lambda (x) (and x #t))))

(define $cpcoverage
(let ()
  (import (nanopass))
  (include "base-lang.ss")

  (define-pass cpcoverage : Lsrc (ir seed) -> Lsrc ()
    (definitions
      (define n 0)
      (define next-index!
        (lambda ()
          (let ([i n])
            (set! n (fx+ n 1))
            (modulo (+ (* seed 2654435761) (* i 2246822519))
                    (constant coverage-counter-count)))))
      (define hit
        (lambda (e)
          (with-output-language (Lsrc Expr)
            `(seq (call ,(make-preinfo-call) ,(lookup-primref 3 '$coverage-hit)
                    (quote ,(next-index!)))
                  ,e)))))
    (Expr : Expr (ir) -> Expr ()
      [(if ,[e0] ,[e1] ,[e2]) `(if ,e0 ,(hit e1) ,(hit e2))])
    (CaseLambdaClause : CaseLambdaClause (ir) -> CaseLambdaClause ()
      [(clause (,x* ...) ,interface ,[body])
       `(clause (,x* ...) ,interface ,(hit body))]))

  ;; A hash of the expression that does not depend on the names that
  ;; the expander generates, and that looks at a bounded part of it
  (define (stable-hash x)
    (let ([fuel 10000])
      (let f ([x x])
        (set! fuel (fx- fuel 1))
        (cond
          [(fx<= fuel 0) 0]
          [(pair? x) (logand (+ (* 31 (f (car x))) (f (cdr x)) 7) #xffffffff)]
          [(gensym? x) (string-hash (symbol->string x))]
          [(symbol? x) (symbol-hash x)]
          [(string? x) (string-hash x)]
          [(fixnum? x) (logand x #xffffffff)]
          [(vector? x) (f (vector->list x))]
          [else 1]))))

  (lambda (x)
    (if (generate-coverage-counters)
        (cpcoverage x (stable-hash ($uncprep x)))
        x))))
