#lang racket/base

;; errors.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;; Racket port
;;
;; Condition types and error handling hooks

(provide (struct-out exn:fail:tuition)
         (struct-out exn:fail:tuition:terminal)
         (struct-out exn:fail:tuition:terminal:operation)
         (struct-out exn:fail:tuition:input)
         current-error-handler
         handle-tuition-error)

;; Exception hierarchy
(struct exn:fail:tuition exn:fail () #:transparent)
(struct exn:fail:tuition:terminal exn:fail:tuition (reason) #:transparent)
(struct exn:fail:tuition:terminal:operation exn:fail:tuition:terminal (operation) #:transparent)
(struct exn:fail:tuition:input exn:fail:tuition (reason) #:transparent)

;; Configurable error handler
;; Takes (where condition) where `where` is a symbol like 'event-loop, 'input-loop, etc.
(define current-error-handler
  (make-parameter
   (lambda (where condition)
     (log-warning "tuition ~a: ~a" where (exn-message condition)))))

(define (handle-tuition-error where condition)
  ((current-error-handler) where condition))
