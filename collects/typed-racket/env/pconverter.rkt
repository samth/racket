#lang racket/base


(require "../utils/utils.rkt"
         (rep type-rep object-rep filter-rep rep-utils free-variance)
         (for-template (rep type-rep object-rep filter-rep)
                       (types union abbrev))
         (types abbrev)
         racket/match)

(provide converter)

(define (converter v basic sub)
  (define (gen-constructor sym)
    (string->symbol (string-append "make-" (substring (symbol->string sym) 7))))
  (match v
    [(Union: elems) `(make-Union (sort (list ,@(map sub elems)) < #:key Type-seq))]
    [(Base: n cnt pred marshaled _) marshaled]
    [(Name: stx) `(make-Name (quote-syntax ,stx))]
    [(fld: t acc mut) `(make-fld ,(sub t) (quote-syntax ,acc) ,mut)]
    [(Struct: name parent flds proc poly? pred-id)
     `(make-Struct (quote-syntax ,name) ,(sub parent)
                   ,(sub flds) ,(sub proc) ,(sub poly?)
                   (quote-syntax ,pred-id))]
    [(App: rator rands stx) `(make-App ,(sub rator) ,(sub rands) (quote-syntax ,stx))]
    [(Opaque: pred cert) `(make-Opaque (quote-syntax ,pred) (syntax-local-certifier))]
    [(Refinement: parent pred cert) `(make-Refinement ,(sub parent)
                                                      (quote-syntax ,pred)
                                                      (syntax-local-certifier))]
    [(Mu-name: n b) `(make-Mu ,(sub n) ,(sub b))]
    [(Poly-names: ns b) `(make-Poly (list ,@(map sub ns)) ,(sub b))]
    [(PolyDots-names: ns b) `(make-PolyDots (list ,@(map sub ns)) ,(sub b))]
    [(arr: dom rng rest drest kws)
     `(make-arr ,(sub dom) ,(sub rng) ,(sub rest) ,(sub drest) ,(sub kws))]
    [(TypeFilter: t p i)
     `(make-TypeFilter ,(sub t) ,(sub p) ,(if (identifier? i)
                                              `(quote-syntax ,i)
                                              i))]
    [(NotTypeFilter: t p i)
     `(make-NotTypeFilter ,(sub t) ,(sub p)
                          ,(if (identifier? i)
                               `(quote-syntax ,i)
                               i))]
    [(Path: p i)
     `(make-Path ,(sub p) ,(if (identifier? i)
                               `(quote-syntax ,i)
                               i))]
    [(? Rep?)
     `(,(gen-constructor (car (vector->list (struct->vector v))))
       ,@(map sub (Rep-values v)))]
    [_ (basic v)]))