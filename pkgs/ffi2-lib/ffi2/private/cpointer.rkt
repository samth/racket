#lang racket/base
(require (rename-in '#%foreign
                    [ffi2-ptr->cpointer ffi2-ptr->cpointer*]
                    [cpointer->ffi2-ptr cpointer->ffi2-ptr*]))

(provide ffi2-ptr->cpointer
         cpointer->ffi2-ptr)

(define (ffi2-ptr->cpointer ptr)
  (unless (ffi2-ptr? ptr)
    (raise-argument-error 'ffi2-ptr->cpointer "ffi2-ptr?" ptr))
  (ffi2-ptr->cpointer* ptr))

(define (cpointer->ffi2-ptr ptr)
  (cpointer->ffi2-ptr* 'cpointer->ffi2-ptr ptr))

