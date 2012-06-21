#lang racket/base

;; Copyright (C) 2012 by Sam Tobin-Hochstadt
;; Modified to be a regular Racket library

;;; Copyright (C) 2002 by Noel Welsh.
;;;
;;; This file is part of SRFI-1.

;;; This SRFI-1 implementation is distributed under the same terms as
;;; Racket.

;;; Author: Noel Welsh <noelwelsh@yahoo.com>

;; Commentary:

;; Based on the reference implementation by Olin Shiver and hence:

;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;; this code as long as you do not remove this copyright notice or
;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.
;;     -Olin

;; Olin Shivers verified that he is fine with redistributing this code
;; under the LGPL.  (Verified personally by Eli Barzilay.)

(require racket/list)
(provide lset<=
         lset=
         lset-adjoin
         lset-union
         lset-intersection
         lset-difference
         lset-xor
         lset-diff+intersection)

;; Lists-as-sets
;;;;;;;;;;;;;;;;;

;; Original comments from Olin (may no longer fully apply):
;; This is carefully tuned code; do not modify casually.
;; - It is careful to share storage when possible;
;; - Side-effecting code tries not to perform redundant writes.
;; - It tries to avoid linear-time scans in special cases where constant-time
;;   computations can be performed.
;; - It relies on similar properties from the other list-lib procs it calls.
;;   For example, it uses the fact that the implementations of MEMBER and
;;   FILTER in this source code share longest common tails between args
;;   and results to get structure sharing in the lset procedures.

;; A modified version of this procedure from `srfi/optional`
;; Checks parameter values.
(define (check-arg pred val caller)
  (cond [(pred val) val]
        [else
         (define expected
           (cond [(eq? pred number?) "number"]
                 [(eq? pred integer?) "integer"]
                 [(eq? pred pair?) "pair"]
                 [(eq? pred procedure?) "procedure"]
                 [(eq? pred string?) "string"]
                 [(eq? pred vector?) "vector"]
                 [else ""]))
         (raise-argument-error caller expected val)]))

(define (%lset2<= = lis1 lis2) (andmap (lambda (x) (member x lis2 =)) lis1))

(define (lset<= = . lists)
  (check-arg procedure? = 'lset<=)
  (or (not (pair? lists)) ; 0-ary case
      (let lp ((s1 (car lists)) (rest (cdr lists)))
        (or (not (pair? rest))
            (let ((s2 (car rest))  (rest (cdr rest)))
              (and (or (eq? s2 s1)         ; Fast path
                       (%lset2<= = s1 s2)) ; Real test
                   (lp s2 rest)))))))

(define (lset= = . lists)
  (check-arg procedure? = 'lset=)
  (or (not (pair? lists)) ; 0-ary case
      (let lp ((s1 (car lists)) (rest (cdr lists)))
        (or (not (pair? rest))
            (let ((s2   (car rest))
                  (rest (cdr rest)))
              (and (or (eq? s1 s2) ; Fast path
                       (and (%lset2<= = s1 s2) (%lset2<= = s2 s1))) ; Real test
                   (lp s2 rest)))))))

(define (lset-adjoin = lis . elts)
  (check-arg procedure? = 'lset-adjoin)
  (foldl (lambda (elt ans) (if (member elt ans =) ans (cons elt ans)))
        lis elts))

(define (lset-union = . lists)
  (check-arg procedure? = 'lset-union)
  (foldl (lambda (lis ans)             ; Compute ANS + LIS.
            (cond ((null? lis) ans)     ; Don't copy any lists
                  ((null? ans) lis)     ; if we don't have to.
                  ((eq? lis ans) ans)
                  (else
                   (foldl (lambda (elt ans)
                           (if (ormap (lambda (x) (= x elt)) ans)
                             ans
                             (cons elt ans)))
                         ans lis))))
          '() lists))

(define (lset-intersection = lis1 . lists)
  (check-arg procedure? = 'lset-intersection)
  (let ((lists (remove* lis1 lists eq?))) ; Throw out any LIS1 vals.
    (cond ((ormap null? lists) '())   ; Short cut
          ((null? lists)       lis1)  ; Short cut
          (else (filter (lambda (x)
                          (andmap (lambda (lis) (member x lis =)) lists))
                        lis1)))))

(define (lset-difference = lis1 . lists)
  (check-arg procedure? = 'lset-difference)
  (let ((lists (filter pair? lists))) ; Throw out empty lists.
    (cond ((null? lists)     lis1)    ; Short cut
          ((memq lis1 lists) '())     ; Short cut
          (else (filter (lambda (x)
                          (andmap (lambda (lis) (not (member x lis =)))
                                 lists))
                        lis1)))))

(define (lset-xor = . lists)
  (check-arg procedure? = 'lset-xor)
  (foldl (lambda (b a) ; Compute A xor B:
            ;; Note that this code relies on the constant-time
            ;; short-cuts provided by LSET-DIFF+INTERSECTION,
            ;; LSET-DIFFERENCE & APPEND to provide constant-time short
            ;; cuts for the cases A = (), B = (), and A eq? B. It takes
            ;; a careful case analysis to see it, but it's carefully
            ;; built in.
            ;; Compute a-b and a^b, then compute b-(a^b) and
            ;; cons it onto the front of a-b.
            (let-values ([(a-b a-int-b) (lset-diff+intersection = a b)])
              (cond ((null? a-b)     (lset-difference = b a))
                    ((null? a-int-b) (append b a))
                    (else (foldl (lambda (xb ans)
                                   (if (member xb a-int-b =) ans (cons xb ans)))
                                a-b
                                b)))))
          '() lists))

(define (lset-diff+intersection = lis1 . lists)
  (check-arg procedure? = 'lset-diff+intersection)
  (cond ((andmap null? lists) (values lis1 '())) ; Short cut
        ((memq lis1 lists)    (values '() lis1)) ; Short cut
        (else (partition (lambda (elt)
                           (not (ormap (lambda (lis) (member elt lis =))
                                       lists)))
                         lis1))))
