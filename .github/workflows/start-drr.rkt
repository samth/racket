#lang racket/base
(require racket/class)

(define fire-up-drracket-and-run-tests
  (dynamic-require 'tests/drracket/private/drracket-test-util
                   'fire-up-drracket-and-run-tests))
(define wait-for-drracket-frame
  (dynamic-require 'tests/drracket/private/drracket-test-util
                   'wait-for-drracket-frame))
(define do-execute
  (dynamic-require 'tests/drracket/private/drracket-test-util
                   'do-execute))
(define set-module-language!
  (dynamic-require 'tests/drracket/private/drracket-test-util
                   'set-module-language!))
(define queue-callback
  (dynamic-require 'racket/gui 'queue-callback))

(define (queue-callback/res thunk)
  (define c (make-channel))
  (queue-callback (λ () (channel-put c (thunk))))
  (channel-get c))

(fire-up-drracket-and-run-tests
 (λ ()
   (define the-string "drracket started up")
   (define drs (wait-for-drracket-frame))
   (set-module-language!)
   (define defs (queue-callback/res (λ ()  (send drs get-definitions-text))))
   (define ints (queue-callback/res (λ () (send drs get-interactions-text))))
   (queue-callback/res (λ ()
                         (send defs erase)
                         (send defs insert (format "#lang racket\n~s\n" `(displayln ,the-string)))))
   (do-execute drs)
   (define result
     (queue-callback/res
      (λ ()
        (send ints
              get-text
              (send ints paragraph-start-position 2)
              (send ints paragraph-end-position 2)))))
   (unless (equal? result the-string)
     (eprintf "test failed, got ~a\n" result)
     (exit -1))

   (printf "test succeeded\n")))
