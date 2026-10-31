#lang racket/base
;; binary-trees core written directly in Chez Scheme and run through vm-eval,
;; to compare pairs, immutable and mutable records, and vectors at the VM level.
;; usage: racket chez-level.rkt {pair|rec|mrec|vec|ivec} depth
(require ffi/unsafe/vm)
(define which (string->symbol (vector-ref (current-command-line-arguments) 0)))
(define depth (string->number (vector-ref (current-command-line-arguments) 1)))
(define f
  (vm-eval
   '(let ()
      (define-record-type node (fields left right) (sealed #t) (nongenerative node-bt2))
      (define (make-r d) (if (fx= d 0) (make-node #f #f) (let ([n (fx- d 1)]) (make-node (make-r n) (make-r n)))))
      (define (check-r t) (let ([l (node-left t)]) (if l (fx+ 1 (fx+ (check-r l) (check-r (node-right t)))) 1)))
      (define (make-p d) (if (fx= d 0) (cons #f #f) (let ([n (fx- d 1)]) (cons (make-p n) (make-p n)))))
      (define (check-p t) (let ([l (car t)]) (if l (fx+ 1 (fx+ (check-p l) (check-p (cdr t)))) 1)))
      (define (make-v d) (if (fx= d 0) (vector #f #f) (let ([n (fx- d 1)]) (vector (make-v n) (make-v n)))))
      (define (check-v t) (let ([l (vector-ref t 0)]) (if l (fx+ 1 (fx+ (check-v l) (check-v (vector-ref t 1)))) 1)))
      (define-record-type mnode (fields (mutable left) (mutable right)) (sealed #t) (nongenerative node-bt3))
      (define (make-m d) (if (fx= d 0) (make-mnode #f #f) (let ([n (fx- d 1)]) (make-mnode (make-m n) (make-m n)))))
      (define (check-m t) (let ([l (mnode-left t)]) (if l (fx+ 1 (fx+ (check-m l) (check-m (mnode-right t)))) 1)))
      (define (make-iv d) (if (fx= d 0) (vector->immutable-vector (vector #f #f)) (let ([n (fx- d 1)]) (let ([a (make-iv n)]) (vector->immutable-vector (vector a (make-iv n)))))))
      (define (run make check max-depth)
        (let ([long (make max-depth)])
          (do ([d 4 (fx+ d 2)]) ((fx> d max-depth))
            (let ([iters (fxsll 1 (fx+ (fx- max-depth d) 4))])
              (do ([i 0 (fx+ i 1)] [c 0 (fx+ c (check (make d)))]) ((fx= i iters) c))))
          (check long)))
      (lambda (which depth) (case which [(pair) (run make-p check-p depth)] [(vec) (run make-v check-v depth)] [(mrec) (run make-m check-m depth)] [(ivec) (run make-iv check-v depth)] [else (run make-r check-r depth)])))))
(define gc0 (current-gc-milliseconds))
(define t0 (current-process-milliseconds))
(define r (f which depth))
(printf "in-racket ~a ~a: cpu ~a ms gc ~a ms\n" which r (- (current-process-milliseconds) t0) (- (current-gc-milliseconds) gc0))
