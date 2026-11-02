#lang racket/base
(require racket/path
         setup/collects)

(provide (all-defined-out))

(define (rkt->ss p)
  (if (path-has-extension? p #".rkt")
      (path-replace-extension p #".ss")
      p))

(define (path*->collects-relative p [cache #f])
  (cond
    [cache
     ;; Converting can probe the filesystem several times, and the same
     ;; dependencies show up for many modules, so use the cache that
     ;; also serves `collects-relative*->path`:
     (hash-ref! cache (vector 'collects-relative p)
                (lambda () (path*->collects-relative p #f)))]
    [(bytes? p)
     (let ([q (path->collects-relative (bytes->path p))])
       (if (path? q)
           (path->bytes q)
           q))]
    [else (path->collects-relative p)]))

(define (collects-relative*->path p cache)
  (if (bytes? p)
      (bytes->path p)
      (hash-ref! cache p (lambda () (collects-relative->path p)))))
