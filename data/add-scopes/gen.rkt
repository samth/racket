#lang racket/base
;; Generates test programs for Ryan's review suggestions on racket/racket#5669
(require racket/format racket/list racket/file)
(define dir (vector-ref (current-command-line-arguments) 0))
(make-directory* dir)
(define (out name lang body)
  (with-output-to-file (build-path dir name) #:exists 'truncate
    (lambda () (printf "#lang ~a\n" lang) (for ([f body]) (write f) (newline)))))

;; --- teaching languages ---
;; deeply nested arithmetic
(define (nest-plus n) (for/fold ([e 'x]) ([i n]) `(+ 1 ,e)))
(for ([n '(500 2000)])
  (out (~a "bsl-plus-" n ".rkt") "htdp/bsl"
       `((define (f x) ,(nest-plus n)) (check-expect (f 0) ,n))))
;; deeply nested cond
(define (nest-cond n) (for/fold ([e 0]) ([i n]) `(cond [(= x ,i) ,i] [else ,e])))
(for ([n '(200 800)])
  (out (~a "bsl-cond-" n ".rkt") "htdp/bsl"
       `((define (f x) ,(nest-cond n)) (check-expect (f 3) 3))))
;; deeply nested local (local-expands its definitions)
(define (nest-local n)
  (for/fold ([e (string->symbol (~a "a" n))]) ([i (in-range n 0 -1)])
    `(local [(define ,(string->symbol (~a "a" i)) ,(if (= i 1) 'x (string->symbol (~a "a" (sub1 i)))))] ,e)))
(for ([n '(50 150 300)])
  (out (~a "isl-local-" n ".rkt") "htdp/isl"
       `((define (f x) ,(nest-local n)) (check-expect (f 7) 7))))
;; nested lambda in ISL+
(define (nest-lambda n)
  (for/fold ([e 'x]) ([i n]) `((lambda (y) ,e) ,i)))
(for ([n '(200)])
  (out (~a "islplus-lambda-" n ".rkt") "htdp/isl+"
       `((define (f x) ,(nest-lambda n)) (check-expect (f 1) 1))))
;; a large flat program: many structs, functions with conds, check-expects
(define (big-program n)
  (append*
   (for/list ([i n])
     (define s (string->symbol (~a "pt" i)))
     (define f (string->symbol (~a "f" i)))
     (define mk (string->symbol (~a "make-pt" i)))
     (define x (string->symbol (~a "pt" i "-x")))
     (define y (string->symbol (~a "pt" i "-y")))
     `((define-struct ,s (x y))
       (define (,f p n)
         (cond [(zero? n) (+ (,x p) (,y p))]
               [(even? n) (,f (,mk (,y p) (,x p)) (sub1 n))]
               [(> n 100) (* 2 (,x p))]
               [else (local [(define q (,mk (+ 1 (,x p)) (,y p)))] (,f q (sub1 n)))]))
       (check-expect (,f (,mk 1 2) 3) ,(+ 1 2 2))))))
(for ([n '(300 1000)])
  (out (~a "isl-big-" n ".rkt") "htdp/isl" (big-program n)))
(out "asl-big-300.rkt" "htdp/asl" (big-program 300))

;; --- interned introducer ---
;; `sp-ref` looks up an identifier in a space, as Rhombus does: it adds
;; the space's interned scope (created early, so older than the scopes
;; around it) to the identifier and checks its binding. Nested `n`
;; levels deep inside `let`s, the identifier has a big scope set in
;; which the interned scope is near the end.
(define interned-prelude
  '((require (for-syntax racket/base))
    (define-syntax (sp-ref stx)
      (syntax-case stx ()
        [(_ id)
         (let ([in-space ((make-interned-syntax-introducer 'my-space) #'id 'add)])
           (identifier-binding in-space)
           (if (free-identifier=? in-space #'id) #'id #'id))]))))
(define (nest-space n k)
  (for/fold ([e `(list ,@(for/list ([j k]) `(sp-ref ,(string->symbol (~a "x" n)))))])
            ([i (in-range n 0 -1)])
    `(let ([,(string->symbol (~a "x" i))
            ,(if (= i 1) 0 `(+ ,@(for/list ([j k]) `(sp-ref ,(string->symbol (~a "x" (sub1 i)))))))])
       ,e)))
(for ([n '(100 300 600 1000)])
  (out (~a "interned-" n ".rkt") "racket/base"
       (append interned-prelude `((define (f) ,(nest-space n 50)) (f)))))
;; Many module-level definitions, each referring to the previous one
;; through `sp-ref`
(for ([m (quote (3000 10000))])
(out (~a "interned-flat-" m ".rkt") "racket/base"
     (append interned-prelude
             (for/list ([i m])
               `(define ,(string->symbol (~a "g" i))
                  ,(if (zero? i) 0 `(+ 1 (sp-ref ,(string->symbol (~a "g" (sub1 i)))))))))))
