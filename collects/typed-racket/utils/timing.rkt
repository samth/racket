#lang racket/base
(require (for-syntax racket/base syntax/parse racket/format) racket/block)
(provide log-time (struct-out tr-event) timing-logger %)

(define-logger timing)

(struct tr-event (start? msec name) #:prefab)

(define-syntax (log-time stx)
  (syntax-parse stx
    [(_ (~and name (~or _:str ((~literal format) . _)))
        ;; the above check is to make sure we don't forget
        ;; the name string
        e:expr ... last:expr)
     (quasisyntax/loc stx
       (#,(if (eq? 'expression (syntax-local-context))
              #'block 
              #'begin)
        (define before (current-inexact-milliseconds)) 
        (when (log-level? timing-logger 'debug)
          (log-message timing-logger 'debug 'timing
                       (format "starting tr event ~a" name)
                       (tr-event #t before name)))
        e ...
        (begin0
          last
          (when (log-level? timing-logger 'debug)
            (log-message timing-logger 'debug 'timing
                         (format "finishing tr event ~a" name)
                         (tr-event #f 
                                   (- (current-inexact-milliseconds) before)
                                   name))))))]))

(define-syntax (% stx)
  (syntax-case stx ()
    [(_ e . rest)
     #`(log-time #,(~a (syntax->datum #'e)) (e . rest))]))