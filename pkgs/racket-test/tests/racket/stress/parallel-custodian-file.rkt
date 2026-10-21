#lang racket/base
(require racket/file)

;; Shutting down a custodian that manages a parallel thread must not
;; deadlock while the thread opens and closes files, which takes the
;; custodian lock; the terminated thread also must not report seeing
;; its custodian shut down (racket/racket#5627)

(define file (path->string (make-temporary-file)))

(define o (open-output-bytes))

(parameterize ([current-error-port o])
  (for ([i (in-range 1000)])
    (define c (make-custodian))
    (define ready (make-semaphore))
    (parameterize ([current-custodian c])
      (thread #:pool 'own
              (lambda ()
                (semaphore-post ready)
                (let loop ()
                  (call-with-input-file file read-byte)
                  (loop)))))
    (semaphore-wait ready)
    (sleep 0.0001)
    (custodian-shutdown-all c)))

(delete-file file)

(unless (equal? #"" (get-output-bytes o))
  (error "unexpected error output" (get-output-bytes o)))
