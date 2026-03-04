#lang racket/base
(require compiler/zo-structs
         compiler/zo-parse
         compiler/zo-marshal
         compiler/private/opaque
         ffi/unsafe/vm
         racket/file
         racket/match
         "log.rkt")

(provide convert-zo-fasl-to-vfasl!)

(define (convert-zo-fasl-to-vfasl! zo-path)
  (log-demodularizer-info "Converting to vfasl format")
  (define parsed (call-with-input-file* zo-path zo-parse))
  (define converted (convert-parsed parsed))
  (define bstr (zo-marshal converted))
  (call-with-output-file* zo-path
                          #:exists 'truncate/replace
                          (lambda (out) (write-bytes bstr out)))
  (void))

(define (convert-parsed parsed)
  (match parsed
    [(linkl-bundle table) (linkl-bundle (convert-bundle-table table))]
    [(linkl-directory table)
     (linkl-directory
      (for/hash ([(name bundle) (in-hash table)])
        (values name
                (and bundle (linkl-bundle (convert-bundle-table (linkl-bundle-table bundle)))))))]))

(define (convert-bundle-table table)
  (define opq (hash-ref table 'opaque #f))
  (cond
    [opq (hash-set table 'opaque (convert-opaque-to-vfasl opq))]
    [else table]))

(define (convert-opaque-to-vfasl opq)
  (define fasl-bytes (opaque-bstr opq))
  (define tmp-in (make-temporary-file "vfasl-in-~a.so"))
  (define tmp-out (make-temporary-file "vfasl-out-~a.so"))
  (dynamic-wind void
                (lambda ()
                  (call-with-output-file* tmp-in
                                          #:exists 'truncate/replace
                                          (lambda (out) (write-bytes fasl-bytes out)))
                  (vm-eval `(vfasl-convert-file ,(path->string tmp-in) ,(path->string tmp-out) #f))
                  (opaque (file->bytes tmp-out)))
                (lambda ()
                  (when (file-exists? tmp-in)
                    (delete-file tmp-in))
                  (when (file-exists? tmp-out)
                    (delete-file tmp-out)))))
