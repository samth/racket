#lang racket/base
(require racket/file)

;; Shutting down a custodian that manages a file port must not
;; deadlock with a parallel thread outside that custodian that opens
;; and closes files: opening takes the rktio lock and then the
;; custodian lock, while closing the port for the shutdown takes the
;; port and rktio locks

(define file (path->string (make-temporary-file)))

(define t
  (thread #:pool 'own
          (lambda ()
            (let loop ()
              (call-with-input-file file read-byte)
              (loop)))))

(for ([i (in-range 10000)])
  (define c (make-custodian))
  (parameterize ([current-custodian c])
    (open-input-file file))
  (custodian-shutdown-all c))

(kill-thread t)
(delete-file file)
