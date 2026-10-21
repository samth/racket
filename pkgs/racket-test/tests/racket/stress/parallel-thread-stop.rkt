#lang racket/base
(require racket/file)

;; Stopping a parallel thread, either with `kill-thread` or with a
;; custodian shutdown, should finish promptly even when the thread
;; spends nearly all of its time in uninterruptible mode, as it does
;; when it opens and closes a file named by a path in a loop. (With a
;; string instead of a path, the thread stops quickly.)

(define limit-ms 1000)

(define file (make-temporary-file))

(define (stop-time kill?)
  (define c (make-custodian))
  (define ready (make-semaphore))
  (define t
    (parameterize ([current-custodian c])
      (thread #:pool 'own
              (lambda ()
                (semaphore-post ready)
                (let loop ()
                  (call-with-input-file file read-byte)
                  (loop))))))
  (semaphore-wait ready)
  (sleep 0.0001)
  (define start (current-inexact-milliseconds))
  (if kill?
      (kill-thread t)
      (custodian-shutdown-all c))
  (begin0 (- (current-inexact-milliseconds) start)
    (custodian-shutdown-all c)))

(for* ([kill? (in-list '(#t #f))]
       [i (in-range 10)])
  (define ms (stop-time kill?))
  (unless (ms . < . limit-ms)
    (delete-file file)
    (error 'parallel-thread-stop
           "~a took ~a ms to stop a parallel thread"
           (if kill? "kill-thread" "custodian-shutdown-all")
           (round ms))))

(delete-file file)
