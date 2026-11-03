#lang racket/base
(require racket/flonum
         racket/future
         racket/fixnum)
(define C 4)
(define pool (make-parallel-thread-pool (string->number (or (getenv "POOL") "4"))))
(define WORK (string->number (or (getenv "WORK") "3000000")))
(define (work n)
  (let L ([i 0]
          [a 0.0])
    (if (fx= i n)
        a
        (L (fx+ i 1) (fl+ a (fl/ 1.0 (fx->fl (fx+ i 1))))))))
(define starts (make-vector C 0.0))
(define ends (make-vector C 0.0))
(define stagger '())
(define spans '())
(define create '())
(define chunks '())
(define parent-share? (getenv "PARENT"))
(define t-all (current-inexact-monotonic-milliseconds))
(for ([pass 40])
  (define t0 (current-inexact-monotonic-milliseconds))
  (define ths
    (for/list ([k (if parent-share?
                      (sub1 C)
                      C)])
      (thread #:pool pool
              #:keep 'results
              (lambda ()
                (vector-set! starts k (current-inexact-monotonic-milliseconds))
                (work WORK)
                (vector-set! ends k (current-inexact-monotonic-milliseconds))))))
  (define t1 (current-inexact-monotonic-milliseconds))
  (when parent-share?
    (vector-set! starts (sub1 C) (current-inexact-monotonic-milliseconds))
    (work WORK)
    (vector-set! ends (sub1 C) (current-inexact-monotonic-milliseconds)))
  (for-each thread-wait ths)
  (define t2 (current-inexact-monotonic-milliseconds))
  (set! create (cons (- t1 t0) create))
  (set! chunks
        (append (for/list ([s starts]
                           [e ends])
                  (- e s))
                chunks))
  (set! stagger
        (cons (- (for/fold ([m 0.0]) ([s starts])
                   (max m s))
                 (for/fold ([m +inf.0]) ([s starts])
                   (min m s)))
              stagger))
  (set! spans (cons (- t2 t0) spans)))
(define (med l)
  (list-ref (sort l <) (quotient (length l) 2)))
(define (r x)
  (/ (round (* 100 x)) 100.0))
(printf "total ~a ms; per pass median: span ~a, create ~a, start stagger ~a; chunk median ~a max ~a\n"
        (r (- (current-inexact-monotonic-milliseconds) t-all))
        (r (med spans))
        (r (med create))
        (r (med stagger))
        (r (med chunks))
        (r (apply max chunks)))
