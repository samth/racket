#lang info

;; Standalone benchmark CLIs require arguments and/or redirected FASTA input.
;; Run the dedicated conformance checker instead of recursively instantiating
;; every performance program as a unit test.
(define test-omit-paths 'all)
