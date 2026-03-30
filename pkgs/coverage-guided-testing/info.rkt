#lang info

(define collection "coverage-guided-testing")
(define deps '("base" "rackcheck-lib" "errortrace-lib" "rackunit-lib"))
(define build-deps '("rackunit-lib"))
(define pkg-desc "Coverage-guided property-based testing for Racket, built on rackcheck and errortrace")
(define version "0.1.0")
(define pkg-authors '(claude))
(define license '(Apache-2.0 OR MIT))
