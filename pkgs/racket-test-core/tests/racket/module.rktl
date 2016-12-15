
(load-relative "loadtest.rktl")

(Section 'module)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module n racket/base
  (define n 'n) 
  (define-struct s (field1 field2) #:mutable)
  (provide n
	   (struct-out s)
	   (rename-out [n m])))

(require 'n)
(test 'n 'required-n n)
(test 'n 'required-n m)

(test 's-field1 object-name s-field1)
(test 's-field2 object-name s-field2)
(test 'set-s-field1! object-name set-s-field1!)
(test 'set-s-field2! object-name set-s-field2!)
(test 's? object-name s?)
(test 7 s-field1 (make-s 7 8))
(test 8 s-field2 (make-s 7 8))
(define an-s (make-s 7 8))
(test (void) set-s-field1! an-s 12)
(test (void) set-s-field2! an-s 13)
(test 12 s-field1 an-s)
(test 13 s-field2 an-s)

(syntax-test #'(set! n 10))
(syntax-test #'(set! m 10))
(syntax-test #'(set! make-s 10))

(syntax-test #'(module))
(syntax-test #'(module m))
(syntax-test #'(module 5 racket/base))

(syntax-test #'(module m 5))

(syntax-test #'(module m racket/base . 1))

(syntax-test #'(#%module-begin))
(syntax-test #'(+ (#%module-begin) 2))

(syntax-test #'(module n+ racket/base (#%module-begin (#%module-begin (define n+ 'n+) (provide n+)))))
(syntax-test #'(module n+ racket/base (define n+ 'n+) (#%module-begin (provide n+))))
(syntax-test #'(module n+ racket/base (define n+ 'n+) (#%module-begin) (provide n+)))
(syntax-test #'(module n+ racket/base (#%module-begin) (define n+ 'n+) (provide n+)))
(module n+ racket/base (#%module-begin (define n+ 'n+) (provide n+)))

(syntax-test #'(#%declare))
(syntax-test #'(module m racket/base (#%declare something)))
(syntax-test #'(module m racket/base (#%declare "something")))
(syntax-test #'(module m racket/base (#%declare #:something)))

(syntax-test #'(#%provide))
(syntax-test #'(#%provide . x))
(syntax-test #'(#%provide y . x))
(syntax-test #'(module m racket/base (define x 10) (#%provide . x)))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide y . x)))
(syntax-test #'(module m racket/base (define x 10) (#%provide 1)))
(syntax-test #'(module m racket/base (define x 10) (#%provide "bad")))
(syntax-test #'(module m racket/base (define x 10) (#%provide not-here)))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide x (rename y x))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide x z)))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide x y (rename x y))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (rename))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (rename x))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (rename x y z))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (rename not-here x))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (rename x 1))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (rename 1 x))))
(syntax-test #'(module m racket/base (define-struct x (y)) (#%provide (struct))))
(syntax-test #'(module m racket/base (define-struct x (y)) (#%provide (struct . x))))
(syntax-test #'(module m racket/base (define-struct x (y)) (#%provide (struct x))))
(syntax-test #'(module m racket/base (define-struct x (y)) (#%provide (struct x (y) z))))
(syntax-test #'(module m racket/base (define-struct x (y)) (#%provide (struct x (y) . z))))
(syntax-test #'(module m racket/base (define-struct x (y)) (#%provide (struct 1 ()))))
(syntax-test #'(module m racket/base (define-struct x (y)) (#%provide (struct x (1)))))
(syntax-test #'(module m racket/base (define-struct x (y)) (#%provide (struct x (y . 1)))))
;; (syntax-test #'(module m racket/base (define-struct x (y)) (#%provide (struct x (y y)))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-from))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-from . racket/base))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-from 1))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-from xxxx))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-from racket/base x))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-from-except))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-from-except . racket/base))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-from-except 1))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-from-except racket/base + . -))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-from-except racket/base 1))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-from-except xxxx +))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-from-except racket/base no))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-from-except racket/base + no))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-defined x))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-defined . x))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-defined 1))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-defined-except . x))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-defined-except 1))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-defined-except x 1))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-defined-except no))))

(syntax-test #'(#%require . x))
(syntax-test #'(#%require m . x))
(syntax-test #'(module m racket/base (#%require n . x)))
(syntax-test #'(module m racket/base (#%require (prefix))))
(syntax-test #'(module m racket/base (#%require (prefix n))))
(syntax-test #'(module m racket/base (#%require (prefix . pre:))))
(syntax-test #'(module m racket/base (#%require (prefix pre: . n))))
(syntax-test #'(module m racket/base (#%require (prefix 1 n))))
(syntax-test #'(module m racket/base (#%require (prefix pre: n more))))
(syntax-test #'(module m racket/base (#%require (prefix pre: n . more))))
(syntax-test #'(module m racket/base (#%require (all-except))))
(syntax-test #'(module m racket/base (#%require (all-except . n))))
(syntax-test #'(module m racket/base (#%require (all-except n 1))))
(syntax-test #'(module m racket/base (#%require (all-except n . n))))
(syntax-test #'(module m racket/base (#%require (rename))))
(syntax-test #'(module m racket/base (#%require (rename . n))))
(syntax-test #'(module m racket/base (#%require (rename 'n))))
(syntax-test #'(module m racket/base (#%require (rename 'n . n))))
(syntax-test #'(module m racket/base (#%require (rename 'n n))))
(syntax-test #'(module m racket/base (#%require (rename 'n n . m))))
(syntax-test #'(module m racket/base (#%require (rename 'n 1 m))))
(syntax-test #'(module m racket/base (#%require (rename 'n n 1))))
(syntax-test #'(module m racket/base (#%require (rename 'n n not-there))))
(syntax-test #'(module m racket/base (#%require (rename 'n n m extra))))

(syntax-test #'(module m racket/base (define x 6) (define x 5)))
(syntax-test #'(module m racket/base (define x 10) (define-syntax x 10)))
(syntax-test #'(module m racket/base (define-syntax x 10) (define x 10)))

;; Cyclic re-def of n:
(syntax-test #'(module n 'n 10))

;; It's now ok to shadow the initial import:
(module _shadow_ racket/base
  (define car 5)
  (provide car))

(test 5 dynamic-require ''_shadow_ 'car)

;; Ok to redefine imported:
(module defines-car-that-overrides-import racket/base (#%require racket/base) (define car 5) (provide car))
(module defines-car-that-overrides-import/stx racket/base (#%require racket/base (for-syntax racket/base)) (define-syntax (car stx) #'6) (provide car))
(test 5 dynamic-require ''defines-car-that-overrides-import 'car)
(test 6 dynamic-require ''defines-car-that-overrides-import/stx 'car)
;; Can't redefine multiple times or import after definition:
(syntax-test #'(module m racket/base (#%require racket/base) (define car 5) (define car 5)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ()
  (define (do-try export import ref val)
    (parameterize ([current-namespace (make-base-namespace)])
      (eval `(module a racket/base
              (define x 'x)
              (define y 'y)
              (provide ,export)))
      (eval `(module b racket/base
              (require ,import)
              (define result ,ref)
              (provide result)))
      (test val dynamic-require ''b 'result)))
  (define-syntax try
    (syntax-rules (=>)
      [(_ export import ref => val)
       (do-try 'export 'import 'ref val)]))

  (try x 'a x => 'x)
  (try y 'a y => 'y)
  (try (combine-out x y) 'a x => 'x)
  (try (combine-out x y) 'a y => 'y)

  (try (combine-out x y) (only-in 'a x) x => 'x)
  (try (combine-out x y) (only-in 'a [x y]) y => 'x)

  (try (rename-out [x y]) 'a y => 'x)

  (try x (prefix-in a: 'a) a:x => 'x)
  (try x (prefix-in |a :| 'a) |a :x| => 'x)
  (try x (prefix-in z. (prefix-in |a :| 'a)) |z.a :x| => 'x)
  (try (prefix-out o: x) 'a o:x => 'x)
  (try (prefix-out |o :| x) 'a |o :x| => 'x)

  (try (prefix-out o: x) (prefix-in i. 'a) i.o:x => 'x)
  (try (prefix-out |o :| x) (rename-in 'a [|o :x| ex]) ex => 'x))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check namespace-attach-module:

(let* ([n (make-empty-namespace)]
       [l null]
       [here (lambda (v)
	       (set! l (cons v l)))])
  (namespace-attach-module (current-namespace) 'racket/base n)
  (parameterize ([current-namespace n])
    (namespace-require 'racket/base)
    (eval `(module a racket/base
	     (define a 1)
	     (,here 'a)
	     (provide a)))
    (test null values l)
    (eval `(module b racket/base
	     (require (for-template 'a))
	     (define b 1)
	     (,here 'b)
	     (provide b)))
    (test null values l)
    (eval `(module c racket/base
	     (require (for-template 'b))
	     (define c 1)
	     (,here 'c)
	     (provide c)))
    (test null values l)
    (eval `(module d racket/base
	     (require (for-syntax 'c))
	     (define d 1)
	     (,here 'd)
	     (provide d)))
    (test '(c) values l)
    (eval `(module e racket/base
	     (require (for-syntax 'd))
	     (define e 1)
	     (,here 'e)
	     (provide e)))
    (test '(d b c) values l)
    (eval `(module f racket/base
	     (,here 'f)
	     (require 'e 'b)))
    (test '(d b d b c) values l)
    (eval `(require 'f))
    (let ([finished '(f b e  a  d b d b c)])
      (test finished values l)
      (eval '10) ; triggers `d` and `b`
      (let ([finished (append '(d b) finished)])
        (test finished values l)
        (namespace-attach-module n ''f)
        (test finished values l)
        (parameterize ([current-namespace (make-empty-namespace)])
          (namespace-attach-module n ''f)
          (test finished values l)
          (namespace-require 'racket/base)
          (eval `(require 'a))
          (eval `(require 'f))
          (test finished values l)
          (eval '10)
          (test (list* 'd 'b finished) values l))))))

(let* ([n (make-base-namespace)]
       [l null]
       [here (lambda (v)
	       (set! l (cons v l)))])
  (parameterize ([current-namespace n])
    (eval `(module a racket/base
             (require (for-syntax racket/base)
                      (for-meta 2 racket/base))
	     (define a 1)
             (define-syntax (a-macro stx) #'-1)
             (begin-for-syntax
              (,here 'pma))
             (begin-for-syntax
              (,here 'ma)
              (define a-meta 10)
              (define-syntax (a-meta-macro stx) #'-1)
              (begin-for-syntax
               (define a-meta-meta 100)
               (,here 'mma)))
	     (,here 'a)
	     (provide a a-macro (for-syntax a-meta-macro))))
    (test '(ma mma pma) values l)
    (set! l null)
    (dynamic-require ''a #f)
    (test '(a) values l)
    (eval `10)
    (test '(a) values l)
    (dynamic-require ''a 0) ; => 'a is available...
    (eval `10)
    (test '(ma pma a) values l)
    (eval '(begin-for-syntax)) ; triggers phase-1 visit => phase-2 instantiate
    (test '(mma ma pma a) values l)
    (void)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check redundant import and re-provide

(module m_cr racket/base
  (provide x_cr y_cr z_cr w_cr)
  (define x_cr 12)
  (define y_cr 14)
  (define z_cr 16)
  (define w_cr 18))

(syntax-test #'(module n_cr racket/base
		 (require 'm_cr)
		 (#%provide (all-from-except 'm_cr no-such-var))))
(syntax-test #'(module n_cr racket/base
		 (require 'm_cr)
		 (#%provide (all-from-except 'm_cr cons))))

(module n_cr racket/base
  (require 'm_cr)
  (#%provide (all-from-except 'm_cr x_cr)))

(module p_cr racket/base
  (require 'n_cr 'm_cr)
  (#%provide (all-from 'm_cr)))

(require 'p_cr)
(test 14 values y_cr)

(module p2_cr racket/base
  (require 'm_cr 'n_cr)
  (#%provide (all-from 'm_cr)))

(require 'p2_cr)
(test 16 values z_cr)

(module p3_cr racket/base
  (require 'm_cr 'n_cr)
  (#%provide (all-from 'n_cr)))

(require 'p3_cr)
(test 18 values w_cr)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test `require' scoping

(module fake-prefix-in racket/base
  (require (for-syntax racket/base)
           racket/require-syntax)
  (define-require-syntax (pseudo-+ stx)
    (syntax-case stx ()
      [(_ id)
       #'(only-in racket/base [+ id])]))
  (provide pseudo-+))

(require 'fake-prefix-in
         (pseudo-+ ++))
(test 12 values (++ 7 5))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test proper bindings for `#%module-begin'

(define expand-test-use-toplevel? #t)

(test (void) eval
      '(begin
	 (module mod_beg2 racket/base
           (require (for-syntax racket/base))
           (#%provide (all-from-except racket/base #%module-begin))
           (#%provide (rename mb #%module-begin))
           (define-syntax (mb stx)
             (syntax-case stx ()
               [(_ . forms)
                #`(#%plain-module-begin 
                   #,(datum->syntax stx '(require (for-syntax racket/base)))
                   . forms)])))
	 (module m 'mod_beg2
           3)))

(test (void) eval
      '(begin
	 (module mod_beg2 racket/base
           (require (for-syntax racket/base))
           (#%provide (all-from-except racket/base #%module-begin))
           (#%provide (rename mb #%module-begin))
           (define-syntax (mb stx)
             (syntax-case stx ()
               [(_ . forms)
                #`(#%plain-module-begin 
                   #,(datum->syntax stx '(require (for-syntax racket/base)))
                   . forms)])))
	 (module m 'mod_beg2
           3 4)))

(test (void) eval
      '(begin
	 (module mod_beg2 racket/base
           (require (for-syntax racket/base))
           (#%provide (all-from-except racket/base #%module-begin))
           (#%provide (rename mb #%module-begin))
           (define-syntax (mb stx)
             (syntax-case stx ()
               [(mb . forms)
                #`(#%plain-module-begin 
                   #,(datum->syntax #'mb '(require (for-syntax racket/base)))
                   . forms)])))
	 (module m 'mod_beg2
           3)))

(define expand-test-use-toplevel? #f)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check line between macro definition and use:

(module local-binding-produces-identity racket/base
  (provide proc)
  
  (define proc
    (let ()
      (define-syntax identity
        (syntax-rules ()
          [(_ misc-id)
           (lambda (x)
             (let ([misc-id 'other])
               x))]))
      
      (identity x))))

(test 77 (dynamic-require ''local-binding-produces-identity 'proc) 77)

(module module-binding-produces-identity racket/base
  (define-syntax identity
    (syntax-rules ()
      [(_ misc-id)
       (lambda (x)
         (let ([misc-id 'other])
           x))]))
  (identity x))

(test 79
      (let ([proc #f])
        (parameterize ([current-print (lambda (v) (set! proc v))])
          (dynamic-require ''module-binding-produces-identity #f))
        proc)
      79)

(module macro-introduced-binding-produces-identity racket/base
  (define-syntax-rule (gen)
    (begin
      (define-syntax identity
        (syntax-rules ()
          [(_ misc-id)
           (lambda (x)
             (let ([misc-id 'other])
               x))]))
      (identity x)))
  (gen))

(test 78
      (let ([proc #f])
        (parameterize ([current-print (lambda (v) (set! proc v))])
          (dynamic-require ''macro-introduced-binding-produces-identity #f))
        proc)
      78)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ([f1 (make-temporary-file)]
      [f2 (make-temporary-file)]
      [exn:fail-cycle? (lambda (exn)
                         (and (exn:fail? exn)
                              (regexp-match? #rx"cycle" (exn-message exn))))])
  (let-values ([(b1 tmp1 mbd1?) (split-path f1)]
               [(b2 tmp2 mbd2?) (split-path f2)])
              
  (with-output-to-file f1
    #:exists 'truncate/replace
    (lambda ()
      (write `(module ,(string->symbol (path->string tmp1)) racket/base (require (file ,(path->string f2)))))))
  (with-output-to-file f2
    #:exists 'truncate/replace
    (lambda ()
      (write `(module ,(string->symbol (path->string tmp2)) racket/base (require (file ,(path->string f1)))))))
  (err/rt-test (dynamic-require f1 #f) exn:fail-cycle?)
  (err/rt-test (dynamic-require f2 #f) exn:fail-cycle?)
  (delete-file f1)
  (delete-file f2)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test #t module-path? "hello")
(test #t module-path? "hello.rkt")
(test #f module-path? "hello*ss")
(test #t module-path? "hello%2ess")
(test #t module-path? "hello%00ss")
(test #f module-path? "hello%2Ess")
(test #f module-path? "hello%41ss")
(test #f module-path? "hello%4")
(test #f module-path? "hello%")
(test #f module-path? "hello%q0")
(test #f module-path? "hello%0q")
(test #f module-path? "foo.rkt/hello")
(test #f module-path? "foo/")
(test #f module-path? "a/foo/")
(test #f module-path? "/foo.rkt")
(test #f module-path? "/a/foo.rkt")
(test #f module-path? "a/foo.rkt/b")
(test #t module-path? "a/foo%2ess/b")
(test #t module-path? "a/_/b")
(test #t module-path? "a/0123456789+-_/b.---")
(test #t module-path? "a/0123456789+-_/b.-%2e")
(test #t module-path? "../foo.rkt")
(test #t module-path? "x/../foo.rkt")
(test #t module-path? "x/./foo.rkt")
(test #t module-path? "x/.")
(test #t module-path? "x/..")

(test #t module-path? (collection-file-path "module.rktl" "tests" "racket"))
(test #t module-path? (string->path "x"))

(test #t module-path? 'hello)
(test #f module-path? 'hello/)
(test #f module-path? 'hello.rkt)
(test #t module-path? 'hello%2ess)
(test #f module-path? 'hello%2Ess)
(test #f module-path? 'hello/a.rkt)
(test #f module-path? '/hello/a.rkt)
(test #f module-path? '/hello)
(test #f module-path? '/a/hello)
(test #f module-path? 'a//hello)
(test #f module-path? '../hello)
(test #f module-path? './hello)
(test #f module-path? 'a/../hello)
(test #f module-path? 'b/./hello)
(test #f module-path? 'b/*/hello)

(test #t module-path? '(lib "hello"))
(test #f module-path? '(lib "hello/"))
(test #f module-path? '(lib "hello/../b"))
(test #t module-path? '(lib "hello/a"))
(test #t module-path? '(lib "hello/a.rkt"))
(test #f module-path? '(lib "hello.bb/a.rkt"))
(test #f module-path? '(lib "/hello/a.rkt"))
(test #t module-path? '(lib "hello/a.rkt" "ack"))
(test #t module-path? '(lib "hello/a.rkt" "ack" "bar"))
(test #t module-path? '(lib "hello/a.rkt" "ack/bar"))
(test #f module-path? '(lib "hello/a.rkt" "ack/"))
(test #f module-path? '(lib "hello/a.rkt" "ack" "/bar"))
(test #f module-path? '(lib "hello/a.rkt" "ack" ".."))
(test #f module-path? '(lib "hello/a.rkt" "ack" bar))
(test #f module-path? '(lib "hello/a.rkt"  . bar))
(test #f module-path? '(lib . "hello/a.rkt"))
(test #f module-path? '(lib))

(test #f module-path? '(planet))
(test #f module-path? '(planet robby))
(test #t module-path? '(planet robby/redex))
(test #t module-path? '(planet robby%2e/%2eredex))
(test #f module-path? '(planet robby%2/redex))
(test #f module-path? '(planet robby/redex%2))
(test #f module-path? '(planet robby/redex/))
(test #f module-path? '(planet robby/redex/foo/))
(test #f module-path? '(planet /robby/redex/foo))
(test #f module-path? '(planet robby/redex.plt/foo))
(test #f module-path? '(planet robby/redex/foo.rkt))
(test #f module-path? '(planet robby/redex/foo.rkt/bar))
(test #f module-path? '(planet robby/../foo))
(test #t module-path? '(planet robby/redex/foo))
(test #t module-path? '(planet robby/redex/foo/bar))
(test #t module-path? '(planet robby/redex:7/foo))
(test #t module-path? '(planet robby/redex:7))
(test #t module-path? '(planet robby/redex:7:8/foo))
(test #t module-path? '(planet robby/redex:7:<=8/foo))
(test #t module-path? '(planet robby/redex:7:>=8/foo))
(test #t module-path? '(planet robby/redex:7:8-9/foo))
(test #t module-path? '(planet robby/redex:7:8-9))
(test #t module-path? '(planet robby/redex:700:800-00900/foo))
(test #t module-path? '(planet robby/redex:700:800-00900/foo%2e))
(test #f module-path? '(planet robby/redex:=7/foo))
(test #f module-path? '(planet robby/redex::8/foo))
(test #f module-path? '(planet robby/redex:7:/foo))
(test #f module-path? '(planet robby/redex.plt:7:8/foo))
(test #f module-path? '(planet robby/redex:a/foo))
(test #f module-path? '(planet robby/redex:7:a/foo))
(test #f module-path? '(planet robby/redex:7:a-10/foo))
(test #f module-path? '(planet robby/redex:7:10-a/foo))

(test #f module-path? '(planet "foo.rkt"))
(test #t module-path? '(planet "foo.rkt" ("robby" "redex.plt")))
(test #f module-path? '(planet "../foo.rkt" ("robby" "redex.plt")))
(test #t module-path? '(planet "foo.rkt" ("robby" "redex.plt" 7 (7 8))))
(test #t module-path? '(planet "foo.rkt" ("robby" "redex.plt" 7 8)))
(test #t module-path? '(planet "foo.rkt" ("robby" "redex.plt" 7 (= 8))))
(test #t module-path? '(planet "foo.rkt" ("robby" "redex.plt") "sub" "deeper"))
(test #t module-path? '(planet "foo%2e.rkt" ("robby%2e" "redex%2e.plt") "sub%2e" "%2edeeper"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check `relative-in'

(let ([check
       (lambda (path)
         (parameterize ([current-namespace (make-base-namespace)])
           (eval
            `(module relative-in-test racket/base
               (require ,path)
               (provide x)
               (define x (string-join '("a" "b" "c") "."))))
           (test "a.b.c" dynamic-require ''relative-in-test 'x)))])
  (check 'racket/string)
  (check '(relative-in racket/delay "string.rkt"))
  (check '(relative-in racket "string.rkt"))
  (check '(relative-in (lib "racket/main.rkt") "string.rkt"))
  (check '(relative-in (lib "racket") "string.rkt"))
  (check '(relative-in (lib "main.rkt" "racket") "string.rkt"))
  (check `(relative-in ,(collection-file-path "promise.rkt" "racket") "string.rkt"))
  (check '(relative-in racket (relative-in "private/reqprov.rkt" "../string.rkt"))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check collection-path details

(test-values '(not there) (lambda ()
                            (collection-path "nonesuch" 
                                             #:fail (lambda (s) 
                                                      (test #t string? s)
                                                      (values 'not 'there)))))
(test-values '(1 2) (lambda ()
                      (collection-file-path "none.rkt" "nonesuch" 
                                       #:fail (lambda (s)
                                                (test #t string? s)
                                                (values 1 2)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check 'module-language, `module-compiled-language-info', and `module->language-info'

(let ([mk (lambda (val)
            (compile (syntax-property #'(module m scheme/base)
                                      'module-language
                                      val)))])
  (test #f 'info (module-compiled-language-info (mk 10)))
  (test '#(scheme x "whatever") 'info (module-compiled-language-info (mk '#(scheme x "whatever"))))
  (let ([ns (make-base-namespace)])
    (parameterize ([current-namespace ns])
      (eval mk ns)
      (eval (mk '#(scheme x "whatever")))
      (test '#(scheme x "whatever") module->language-info ''m)
      (let ([path (build-path (collection-path "tests" "racket")
                              "langm.rkt")])
        (parameterize ([read-accept-reader #t]
                       [current-module-declare-name (module-path-index-resolve
                                                     (module-path-index-join path #f))])
          (eval
           (read-syntax path
                        (open-input-string "#lang tests/racket (provide x) (define x 1)"
                                           path)))
          ((current-module-name-resolver) (current-module-declare-name) #f)))
      (test '#(tests/racket/lang/getinfo get-info closure-data)
            module->language-info 'tests/racket/langm))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check shadowing of initial imports:

(let ([m-code '(module m racket/base (define-syntax-rule (lambda . _) 5) (provide lambda))]
      [n-code '(module n racket/base 
                 (require 'm) 
                 (define five (lambda (x) x)) 
                 (define five-stx #'lambda)
                 (provide five five-stx))]
      [p-code '(module p racket/base
                 (require 'n)
                 (define same? (free-identifier=? #'lambda five-stx))
                 (provide same?))])
  (let ([ns (make-base-namespace)])
    (eval m-code ns)
    (eval '(require 'm) ns)
    (test 5 eval '(lambda (x) x) ns)
    (let ([m-ns (eval '(module->namespace ''m) ns)])
      (test 5 eval '(lambda (x) x) m-ns))
    (eval n-code ns)
    (eval '(require 'n) ns)
    (test 5 eval 'five ns)
    (eval p-code ns)
    (eval '(require 'p) ns)
    (test #f eval 'same? ns)
    (let ([n-ns (eval '(module->namespace ''n) ns)])
      (test 5 eval '(lambda (x) x) n-ns)))
  (let ([ns (make-base-namespace)])
    (eval m-code ns)
    (let ([n-zo (let ([s (open-output-bytes)])
                  (parameterize ([current-namespace ns])
                    (write (compile n-code) s))
                  (parameterize ([read-accept-compiled #t])
                    (read (open-input-bytes (get-output-bytes s)))))])
      (eval n-zo ns)
      (eval '(require 'n) ns)
      (test 5 eval 'five ns)
      (eval p-code ns)
      (eval '(require 'p) ns)
      ; (test #f eval 'same? ns)
      (let ([n-ns (eval '(module->namespace ''n) ns)])
        (test 5 eval '(lambda (x) x) n-ns)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check printing of resolved module paths

(let ([s (open-output-string)])
  (print (make-resolved-module-path (build-path (current-directory) "a.rkt")) s)
  (test #t regexp-match? #rx"<resolved-module-path:\"" (get-output-string s)))
(let ([s (open-output-string)])
  (print (make-resolved-module-path 'other) s)
  (test #t regexp-match? #rx"<resolved-module-path:'" (get-output-string s)))

(let ([s (open-output-string)])
  (print (module->namespace 'racket/base) s)
  (test #t regexp-match? #rx"<namespace:\"" (get-output-string s)))
(let ([s (open-output-string)])
  (print (module->namespace ''n) s)
  (test #t regexp-match? #rx"<namespace:'" (get-output-string s)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check "source" name of built-in module:

(parameterize ([current-namespace (module->namespace ''#%network)])
  (test '#%network 
        variable-reference->module-source
        (eval (datum->syntax #'here '(#%variable-reference)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check handling of unbound names in local-expand:

(err/rt-test (expand '(module m racket
                        (require racket/require)
                        (require (filtered-in (lambda (n) foo) racket))))
             exn:fail:contract:variable?)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that `quote' can be renamed for use in
;; require specs

(parameterize ([current-namespace (make-base-namespace)])
  (map 
   eval
   '((module service racket
       (#%module-begin
        (module s racket/base)))
     
     (module good-client racket
       (#%module-begin
        (require (quote service))))
     
     (module another-good-client racket
       (#%module-begin
        (require
         (rename-in racket/base
                    [quote dynamic-in]))
        (require
         (dynamic-in service))))
     
     (module also-good-client racket
       (#%module-begin
        (require
         (rename-in racket/base
                    [quote dynamic-in]))
        (require
         (rename-in (dynamic-in service)))))
     
     (module submodule-good-client racket
       (#%module-begin
        (require
         (rename-in racket/base
                    [quote dynamic-in]))
        (require
         (rename-in (submod (dynamic-in service) s)))))
     
     (module another-submodule-good-client racket
       (#%module-begin
        (require
         (rename-in racket/base
                    [quote dynamic-in]
                    [submod alt:submod]))
        (require
         (rename-in (alt:submod (dynamic-in service) s))))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check phase-1 syntax used via for-template
;; and other indirections

(module there-and-back-x racket/base
  (require (for-syntax racket/base))
  (begin-for-syntax
   (provide s s?)
   (struct s (x y))))

(module there-and-back-y racket/base
  (require (for-template 'there-and-back-x))
  (s 1 2)
  (provide s s?))

(module there-and-back-z racket/base
  (require 'there-and-back-y)
  (provide f)
  (define (f) (s 1 2)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check printing of an error message:

(err/rt-test (eval '(module bad-module '#%kernel
                      (#%require (for-meta -1 (only racket make-base-namespace) (only scheme make-base-namespace)))))
             (lambda (exn) (regexp-match? #rx"phase -1" (exn-message exn))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check renames and lifts:

(module post-ex-rename-example-1 racket/base
  (require (for-syntax racket/base))
  (provide go)
  (define-syntax (go stx)
    (syntax-local-lift-module-end-declaration
     #'(define-stuff))
    #'(define-syntax (define-stuff stx)
        #'(define x #f))))

(module post-ex-rename-example-2 racket/base
  (require 'post-ex-rename-example-1)
  (go))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check interaction of binding-context and mark:

(module binding-context-a racket
  (provide q)

  (define-syntax (q stx)
    (syntax-case stx ()
      [(_ f) (with-syntax ([x (syntax-local-introduce #'x)])
               #'(f x))])))

(module binding-context-b racket
  (require 'binding-context-a)

  (define-syntax-rule (go id)
    (begin
      (define id 5)
      (define-syntax-rule (prov)
        (provide id))
      (prov)))
  
  (q go))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check modidx for 'origin items

(syntax-case (parameterize ([current-namespace (make-base-namespace)])
               (expand
                '(module m racket/base
                   (define-syntax-rule (m x) 1)
                   (m x)))) ()
  [(_ name lang (mb rc ds (app cwv (lam () (qt one)) pnt)))
   (begin
     (test 1 syntax-e #'one)
     (test #t identifier? (car (syntax-property #'one 'origin)))
     (test #t symbol? 
           (resolved-module-path-name
            (module-path-index-resolve
             (car (identifier-binding (car (syntax-property #'one 'origin))))))))])

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that set! of an unbound for-syntax variable is a syntax error

(err/rt-test (expand '(module m racket/base
                        (require (for-syntax racket/base))
                        (begin-for-syntax
                         (lambda () (set! x 6)))))
             exn:fail:syntax?)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that an exception during a `provide' expansion
;; doesn't leave the thread in the during-expansion state:

(with-handlers ([exn? void])
  (eval '(module m racket
           (require (for-syntax racket/provide-transform))
           (define-syntax ex
             (make-provide-transformer
              (lambda args
                (/ 0))))
           (provide (ex)))))

(err/rt-test (eval '(define-syntax m (syntax-local-module-defined-identifiers))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that invocation order matches `require' order:

(module order-check-module-a racket/base 'a)
(module order-check-module-b racket/base 'b)
(module order-check-module racket/base (require 'order-check-module-a
                                                'order-check-module-b))
(let ([o (open-output-string)])
  (parameterize ([current-output-port o])
    (dynamic-require ''order-check-module #f))
  (test "'a\n'b\n" get-output-string o))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check phase-shifted, compile-time use of `variable-reference->namespace'

(module uses-variable-reference->namespace racket/base
  (require (for-syntax racket/base))
  (begin-for-syntax
   (variable-reference->namespace (#%variable-reference))))
(module uses-uses-variable-reference->namespace racket/base
  (require (for-template 'uses-variable-reference->namespace)))

(require 'uses-uses-variable-reference->namespace)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check reference to phase-2 definition:

(let ()
  (define m1-expr
    '(module m1 racket/base
       (require (for-syntax racket/base))
       (begin-for-syntax
        (require (for-syntax racket/base))
        (begin-for-syntax
         (define m1 2)
         (provide m1)))))
  
  (define m2-expr
    '(module m2 racket/base
       (require (for-meta -2 'm1))
       m1))

  (parameterize ([current-namespace (make-base-namespace)])
    (eval m1-expr)
    (eval m2-expr))

  (parameterize ([current-namespace (make-base-namespace)])
    (define (compile-eval e)
      (define-values (i o) (make-pipe))
      (write (compile e) o)
      (parameterize ([read-accept-compiled #t])
        (eval (read i))))
    (compile-eval m1-expr)
    (compile-eval m2-expr)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check JIT treatement of seemingly constant imports

(let ()
  (define (a-expr mut?)
    `(module a racket/base
       (#%printing-module-begin
        ,(if mut?
             `(define a 5)
             `(define (a x)
                ;; long enough to not be inlined:
                (list x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)))
        (provide a))))
  (define b-expr
    `(module b racket/base
       (#%printing-module-begin
        (require 'a)
        (define (b q) (a q))
        (provide b))))

  (define (compile-m e strs)
    (parameterize ([current-namespace (make-base-namespace)])
      (for ([str (in-list strs)])
        (parameterize ([read-accept-compiled #t])
          (eval (read (open-input-bytes str)))))
      (define o (open-output-bytes))
      (write (compile e) o)
      (define s (get-output-bytes o))
      (define vlen (bytes-ref s 2))
      ;; Add a hash, so that loading this module in two contexts tries to
      ;; use the same loaded bytecode and same JIT-generated code:
      (bytes-copy! s (+ 4 vlen)
                   (subbytes
                    (bytes-append (string->bytes/utf-8 (format "~s" (bytes-length s)))
                                  (make-bytes 20 0))
                    0
                    20))
      s))

  (define a-s (compile-m (a-expr #f) '()))
  (define am-s (compile-m (a-expr #t) '()))
  (define b-s (compile-m b-expr (list a-s)))

  (define temp-dir (find-system-path 'temp-dir))
  (define dir (build-path temp-dir "compiled"))
  (define dir-existed? (directory-exists? dir))
  (unless dir-existed? (make-directory dir))

  (define (go a-s)
    (parameterize ([current-namespace (make-base-namespace)]
                   [read-accept-compiled #t])
      (eval (read (open-input-bytes a-s)))
      (with-output-to-file (build-path dir "check-gen_rkt.zo")
        #:exists 'truncate
        (lambda () (write-bytes b-s)))
      ((dynamic-require (build-path temp-dir "check-gen.rkt") 'b) 10)))
  ;; Triger JIT generation with constant function as `a':
  (go a-s)
  ;; Check that we don't crash when trying to use a different `a':
  (err/rt-test (go am-s) exn:fail?)
  ;; Cleanup
  (delete-file (build-path dir "check-gen_rkt.zo"))
  (unless dir-existed? (delete-directory dir)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test 5
      'm->n
      (parameterize ([current-namespace (make-base-namespace)])
        (eval '(module m racket/base (define x 5) (provide (protect-out x))))
        (eval '(module n racket/base (require 'm)))
        (eval '(require 'n))
        (parameterize ([current-namespace (module->namespace ''n)])
          (eval 'x))))

(test #t
      'ffi/unsafe->n
      (parameterize ([current-namespace (make-base-namespace)])
        (eval '(module n racket/base (require ffi/unsafe)))
        (eval '(require 'n))
        (parameterize ([current-namespace (module->namespace ''n)])
          (eval '(procedure? ptr-set!)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check link checking and a constructor with auto fields:

(module a-with-auto-field racket/base
  (provide make-region)
  (define-values (struct:region make-region region? region-get region-set!)
    (make-struct-type 'region #f 6 6 #f)))

(module use-a-with-auto-field racket/base
  (require 'a-with-auto-field)
  (void (make-region 1 2 3 4 5 6)))

(require 'use-a-with-auto-field)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check that `require' inside `beging-for-syntax' sets up the right phase dependency

(let ([o (open-output-bytes)])
  (parameterize ([current-output-port o]
                 [current-namespace (make-base-namespace)])
    (eval
     '(module m racket/base
        (printf "~s\n" (variable-reference->phase (#%variable-reference)))))
    (eval
     '(module n racket/base
        (require (for-syntax racket/base))
        (begin-for-syntax
         (require 'm))))
    (eval '(require 'n))
    (eval '10))
  (test #"1\n1\n" get-output-bytes o))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check re-expansion of a module with a macro-introduced `only-in'
;; and a definition of the name that `only-in' switched away from:

(parameterize ([current-namespace (make-base-namespace)])
  (define src
    '(module m racket
       (define-syntax (req stx)
         (syntax-case stx ()
           [(_ spec)
            (let ()
              (with-syntax {[name (datum->syntax #'spec 'enqueue!)]}
                #'(begin
                    (require (rename-in spec [name temp]))
                    (define-syntax name 10))))]))
       
       (req (only-in data/queue enqueue!))))
  (expand-syntax (expand src)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; catch errors due to a module that is not available

(module avail-z racket/base
  (provide foo)
  (define-syntax-rule (foo x) x))

(module avail-y racket/base
  (require 'avail-z)
  (eval-syntax #'(foo 10)))

(err/rt-test (dynamic-require ''avail-y #f)
             (lambda (exn) (and (exn? exn)
                                (regexp-match? #rx"module that is not available"
                                               (exn-message exn)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that a `syntax-local-ift-require' into a top-level context
;; appropriately forces a visit of compile-time code:

(parameterize ([current-namespace (make-base-namespace)])
  (eval '(module m racket/base
           (provide x)
           (define-syntax-rule (x) 5)))
  (eval '(require (for-syntax racket/base)))
  (eval '(define-syntax (m stx)
           (syntax-local-lift-require ''m (datum->syntax stx '(x)))))
  (eval '(m)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that local-expanding module body
;; doesn't pollute future expansion with
;; bindings in any phase (such as phase 2)

(module check-defn-le-lang racket
   (provide
    (except-out (all-from-out racket) #%module-begin)
    (rename-out [module-begin #%module-begin]))

   (define-syntax (module-begin stx)
     (syntax-case stx ()
       ((_ . bs)
        (local-expand
         #'(#%module-begin . bs)
         'module-begin null)))))

(module check-defn-le-module 'check-defn-le-lang
   (require (for-meta 2 racket/base))
   (define x 0)
   (begin-for-syntax
     (begin-for-syntax
       (define y 2))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check `dynamic-require` re-export fast path

(for ([specs (list (list '(provide)
                         'x)
                   (list '(provide x)
                         'x)
                   (list '(provide (rename-out [x xx]))
                         'x)
                   (list '(provide (rename-out [y x]))
                         'x)
                   (list '(provide)
                         '(rename-out [y x])
                         "y")
                   (list '(provide)
                         '(rename-out [z x])
                         "x"
                         ;; slow:
                         "exp\nexp\nrun\nexp\n"))])
  (define ns (make-base-namespace))
  (define o (open-output-string))
  (parameterize ([current-output-port o])
    (eval `(module m racket/base
             (require (for-syntax racket/base))
             (begin-for-syntax (displayln "exp"))
             (define x "x")
             (define y "y")
             (define-syntax (z stx) #'x)
             ,(car specs)
             (module* sub #f
               (displayln "run")
               (provide ,(cadr specs))))
          ns))
  (define expected (if (null? (cddr specs)) "x" (caddr specs)))
  (define expected-out (if ((length specs) . < . 4)
                           "exp\nexp\nrun\n"
                           (list-ref specs 3)))
  (define (dynamic-require/o m x)
    (parameterize ([current-output-port o])
      (dynamic-require m x)))
  (parameterize ([current-namespace ns])
    (test expected dynamic-require/o '(submod 'm sub) 'x)
    (test expected dynamic-require/o '(submod 'm sub) 'x))
  (test expected-out get-output-string o))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check handling of module contexts that are kept
;; only for the context's identity (where sealing
;; could be mishandled)

(let ()
  (define m '(module m racket
               (provide (all-defined-out) def)
               (define-syntax def (make-rename-transformer #'define))))
  (define c #f)
  (sync (thread ; thread isolates `errortrace` parameter side effects
         (lambda ()
           (parameterize ([current-namespace (make-base-namespace)])
             (namespace-require 'errortrace)
             (set! c (compile m))))))
  (write c (open-output-bytes)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that skipping definitions (but continuing
;; with the rest of a module body) is disallowed.

(module disallowed-definition-avoider racket/base

  (define fail
    ((call-with-continuation-prompt
      (lambda ()
        (call/cc values)))))
  
  (error "no"))

(err/rt-test (dynamic-require ''disallowed-definition-avoider #f)
             exn:fail:contract:variable?)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that `syntax-local-lift-require` works interactively
;; with a namespace from `module->namespace`:
(let ()
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns])
    (eval '(module m racket/base
             (require (for-syntax racket/base))
             (define-syntax (m stx)
               (syntax-case stx ()
                 [(_)
                  (syntax-local-introduce
                   (syntax-local-lift-require
                    'racket/list
                    (datum->syntax stx 'empty)))]))))
    (eval '(require 'm))
    (parameterize ([current-namespace (module->namespace ''m)])
      (eval '(m)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that a submodule can be armed:

(test #t
      syntax?
      (expand
       (expand
        #'(module m racket/base
            (define-syntax-rule (s) (module x racket/base 10))
            (s)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check interaction of marks and a syntax-object side channel

;; Tests a special case that makes a reference to an identifier in
;; the enclosing module work, even though the identifier is missing
;; a module context.

#|

I think this was a bad idea. It's trying to make generated identifiers
"just work", but the hack to provide this behavior only covered the
case of module-leve bindings; it doesn't cover local bindings.

(let ()
  (define (mk mode wrap?)
    `(module m racket
       (require (for-syntax syntax/parse racket/syntax))
       (define-for-syntax funny #f)
       (define-syntax (make-funny-set! stx)
         (syntax-parse stx
           [(_ v)
            (define unmarked (generate-temporary))
            (set! funny (syntax-local-introduce unmarked))
            #`(define #,unmarked v)]))
       (define-syntax (funny-ref stx)
         (syntax-parse stx
           [(_)
            funny]))
       (define-syntax (funny-set! stx)
         (syntax-parse stx
           [(_ v)
            #`(set! #,funny v)]))
       (define-syntax (funny-varref stx)
         (syntax-parse stx
           [(_)
            #`(#%variable-reference #,funny)]))
       (make-funny-set! 2)
       ,((if wrap? (lambda (v) `(let () ,v)) values)
         (case mode
           [(ref) '(funny-ref)]
           [(set) '(funny-set! 3)]
           [(var) '(funny-varref)]))))
  (for* ([m '(ref set var)]
         [wrap? '(#t #f)])
    (parameterize ([current-namespace (make-base-namespace)])
      (eval (mk m wrap?)))))

|#

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that module caching doesn't cause submodules
;; to be loaded/declared too early

(define (install-module-hashes! s start len c)
  (define vlen (bytes-ref s (+ start 2)))
  (define mode (integer->char (bytes-ref s (+ start 3 vlen))))
  (case mode
    [(#\B)
     (define h (make-bytes 20 (+ 42 c)))
     (bytes-copy! s (+ start 4 vlen) h)]
    [(#\D)
     (define (read-num rel-pos)
       (define pos (+ start rel-pos))
       (integer-bytes->integer s #t #f pos (+ pos 4)))
     (define count (read-num (+ 4 vlen)))
     (for/fold ([pos (+ 8 vlen)]) ([i (in-range count)])
       (define pos-pos (+ pos 4 (read-num pos)))
       (define mod-start (read-num pos-pos))
       (define mod-len (read-num (+ pos-pos 4)))
       (install-module-hashes! s (+ start mod-start) mod-len i)
       (+ pos-pos 16))
     (void)]
    [else (error "unknown")]))

(let ()
  (define dir (find-system-path 'temp-dir))
  (define tmx (build-path dir "tmx.rkt"))
  (define e (compile '(module tmx racket/base
                        (module s racket/base
                          (provide x)
                          (define x 1)))))
  (make-directory* (build-path dir "compiled"))
  (define zo-path (build-path dir "compiled" "tmx_rkt.zo"))

  (define bstr
    (let ([b (open-output-bytes)])
      (write e b)
      (let* ([s (get-output-bytes b)])
        (install-module-hashes! s 0 (bytes-length s) 0)
        s)))

  (call-with-output-file zo-path
    #:exists 'truncate
    (lambda (o)
      (write-bytes bstr o)))
  (parameterize ([current-namespace (make-base-namespace)]
                 [current-module-declare-name (make-resolved-module-path tmx)]
                 [current-load-relative-directory dir])
    (eval (parameterize ([read-accept-compiled #t])
            (read (open-input-bytes bstr)))))
  (parameterize ([current-namespace (make-base-namespace)])
    (dynamic-require tmx #f)
    (test #f module-declared? `(submod ,tmx s) #f)
    (test 1 dynamic-require `(submod ,tmx s) 'x))
  (delete-file zo-path))

;; Check that module-code caching works
(let ([saved-namespace #f])
  (define dir (find-system-path 'temp-dir))
  (define tmx (build-path dir "tmx2.rkt"))
  (define e (compile '(module tmx2 racket/kernel
                        (#%provide x)
                        (define-values (x) 1))))
  (make-directory* (build-path dir "compiled"))
  (define zo-path (build-path dir "compiled" "tmx2_rkt.zo"))

  (define bstr
    (let ([b (open-output-bytes)])
      (write e b)
      (let* ([s (get-output-bytes b)])
        (install-module-hashes! s 0 (bytes-length s) 100)
        s)))

  (call-with-output-file zo-path
    #:exists 'truncate
    (lambda (o)
      (write-bytes bstr o)))
  (define first-namespace (make-base-namespace))
  (parameterize ([current-namespace first-namespace]
                 [current-module-declare-name (make-resolved-module-path tmx)]
                 [current-load-relative-directory dir])
    (eval (parameterize ([read-accept-compiled #t])
            (read (open-input-bytes bstr)))))

  ;; Mangle the bytecode file; cached variant should be used:
  (call-with-output-file zo-path
    #:exists 'update
    (lambda (o)
      (file-position o (- (file-size zo-path) 100))
      (write-bytes (make-bytes 100 (char->integer #\!)) o)))

  (test 2 add1
        (parameterize ([current-namespace (make-base-namespace)])
          (dynamic-require tmx 'x)))
  (delete-file zo-path)

  ;; Need to retain the namespace until here
  (set! saved-namespace first-namespace))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that `provide` doesn't run enclosed expanders until within a
;; module (as opposed to a `#%module-begin` expansion):

(module check-contract-out-by-itself racket (provide (contract-out)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check `local-require` in a compile-time position:

(module provides-a-for-local-require racket/base
  (define a 1)
  (provide a))

(module uses-a-in-macro-rhs racket/base
  (require (for-syntax racket/base))
  (provide one)
  
  (define-syntax (m stx)
    (local-require 'provides-a-for-local-require)
    #`#,a)
  
  (define one (m)))

(test 1 dynamic-require ''uses-a-in-macro-rhs 'one)

(module uses-a-in-begin-for-syntax racket/base
  (require (for-syntax racket/base))
  (provide one)
  
  (begin-for-syntax
    (define one-ct
      (let ()
        (local-require 'provides-a-for-local-require)
        a)))
  
  (define-syntax (m stx)
    #`#,one-ct)
  
  (define one (m)))

(test 1 dynamic-require ''uses-a-in-begin-for-syntax 'one)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check lifted requires, submodules, and re-expansion:

(define lifted-require-of-submodule
  `(,#'module m racket/base
    (require (for-syntax racket/base))
    (module a racket/base
      (provide a)
      (define a 'a))

    (define-syntax (m stx)
      (syntax-local-lift-require '(submod "." a) (syntax-local-introduce #'a)))

    (m)))

(test #t syntax? (expand-syntax (expand lifted-require-of-submodule)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check module lifting

(module module-lift-example-1 racket/base
  (require (for-syntax racket/base))
  (define-syntax (m stx)
    (syntax-local-lift-module
     #'(module m racket/base
         (provide x)
         (define x 10)))
    #'(begin
        (require 'm)
        (define out x)
        (provide out)))
  (m))

(test 10 dynamic-require ''module-lift-example-1 'out)

(module module-lift-example-2 racket/base
  (require (for-syntax racket/base))
  (define-syntax (m stx)
    (syntax-local-lift-module #'(module* sub #f
                                  (provide s)
                                  (define s (add1 a))))
    #'(void))
  (m)
  (define a 1))

(test 2 dynamic-require '(submod 'module-lift-example-2 sub) 's)


(module module-lift-example-3 racket/base
  (require (for-syntax racket/base))
  (define-syntax (m stx)
    (syntax-local-lift-module #'(module m racket/base
                                  (provide x)
                                  (define x 11)))
    (syntax-local-lift-module-end-declaration
     #'(let ()
         (local-require (submod "." m))
         (set! out x)))
    #'(void))
  (define out -10)
  (m)
  (provide out))

(test 11 dynamic-require ''module-lift-example-3 'out)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check addition of 'disappeared-use by `provide`

(require (rename-in racket/base [lib racket-base:lib]))

(let ()
  (define (find-disappeared stx id)
    (let loop ([s stx])
      (cond
       [(syntax? s)
        (define p (cons (syntax-property s 'disappeared-use)
                        (syntax-property s 'origin)))
        (or (let loop ([p p])
              (cond
               [(identifier? p) (and (free-identifier=? p id)
                                     (eq? (syntax-e p) (syntax-e id)))]
               [(pair? p) (or (loop (car p))
                              (loop (cdr p)))]
               [else #f]))
            (loop (syntax-e s)))]
       [(pair? s)
        (or (loop (car s))
            (loop (cdr s)))]
       [else #f])))
  (let ([form (expand `(module m racket/base
                        (provide (struct-out s))
                        (struct s ())))])
    (test #t find-disappeared form #'struct-out))
  (let ([form (expand `(module m racket/base
                        (require (only-in racket/base car))))])
    (test #t find-disappeared form #'only-in))
  (let ([form (expand `(module m racket/base
                        (require (rename-in racket/base [lib racket-base:lib])
                                 (racket-base:lib "racket/base"))))])
    (test #t find-disappeared form #'racket-base:lib))
  ;; Check case where the provide transformer also sets disappeared-use
  (let ([form (expand `(module m racket/base
                         (require (for-syntax racket/base racket/provide-transform))
                           (define-syntax my-out
                             (make-provide-transformer
                               (lambda (stx phases) null)
                               (lambda (stx phases)
                                 (syntax-case stx ()
                                   [(head id)
                                    (syntax-property #'(rename-out)
                                                     'disappeared-use
                                                     (syntax-local-introduce #'id))]))))
                           (provide (my-out map))))])
    (test #t find-disappeared form #'map)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module force-local-expand-of-body racket/base
  (require (for-syntax racket/base))
  (provide (rename-out [mb #%module-begin])
           (except-out (all-from-out racket/base) #%module-begin))
  
  (define-syntax (mb stx)
    (syntax-case stx ()
      [(_ . b)
       (local-expand #`(#%module-begin . b) (syntax-local-context) null)])))

(module use-local-require-at-phase-1 'force-local-expand-of-body
  (require (for-syntax racket/base))
  
  (begin-for-syntax
    (local-require (only-in racket [+ ++]))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Try trivial nested `begin-for-syntax`,
;; avoiding anything else that might prepare phase 2 in advance

(module starts-phase-2-without-any-content racket
  (begin-for-syntax
    (begin-for-syntax)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure `eval-syntax` doesn't create a fallback context

(module exports-cons-with-context racket/base
  (provide cons-id)
  (define cons-id #'cons))
(require 'exports-cons-with-context racket/base)

(let ([mod (datum->syntax #f `(,#'module m racket/base
                               ;; If a fallback is installed, then
                               ;; the module context of `cons` applies:
                               ,cons-id))])
  (err/rt-test (eval-syntax mod)
               (lambda (exn) (regexp-match #rx"ambiguous" (exn-message exn)))))

;; `eval` should install a fallback for a non`-module` form:
(test (void) eval (datum->syntax #f `(begin (,#'module m racket/base
                                              ,cons-id))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module shadows-a-racket-base-binding-and-exports racket/base
  (provide (all-defined-out)) ; exports `path?`
  (struct path ()))

(module import-shadows-a-racket-base-binding racket/base
  (require 'shadows-a-racket-base-binding-and-exports)
  (provide (all-from-out racket/base)))

;; Fails because imported module doesn't provide `path?`:
(syntax-test #'(module m racket/base
                 (require (rename-in 'import-shadows-a-racket-base-binding
                                     [path? other-path?]))))

(module import-shadows-a-racket-base-binding-and-doesnt-confuse-struct-out racket/base
  (require 'shadows-a-racket-base-binding-and-exports)
  (provide (struct-out path)))

(module shadows-a-racket-base-binding-and-exports-all racket/base
  (provide (all-from-out racket/base)) ; does not export `path?`
  (struct path ()))

(syntax-test #'(module m racket/base
                 (require (rename-in 'shadows-a-racket-base-binding-and-exports-all
                                     [path? other-path?]))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check `syntax-local-lift-require` on an
;; spec that doesn't have the target environment's
;; context:

(module has-a-submodule-that-exports-x racket
  (module b racket/base
    (define x 1)
    (provide x))

  (define-syntax (lifted-require-of-x stx)
    (syntax-case stx ()
      [(_ mod)
       (let ([x (car (generate-temporaries '(x)))])
         (syntax-local-lift-require
          #`(rename mod #,x x)
          x))]))

  (provide lifted-require-of-x))

(require 'has-a-submodule-that-exports-x)

(test 1 values (lifted-require-of-x (submod 'has-a-submodule-that-exports-x b)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This test happens to trigger a combination
;; of lazy adds and reoves that exposed a bug
;; in caching lazy scope propagations

(eval
 (expand
  #'(module x racket/kernel
      (module ma racket/base
        (#%module-begin
         (#%require (for-syntax racket/kernel))
         (define-values (x) 1)
         (define-syntaxes (foo) (lambda (stx) (quote-syntax x)))
         (#%provide foo)))
      (module mb racket/kernel
        (#%require (submod ".." ma))
        (foo)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure that shutting down a custodian
;; releases a lock as it should

(parameterize ([current-custodian (make-custodian)])
  (thread-wait
   (thread
    (lambda ()
      (parameterize ([current-namespace (make-base-namespace)])
        (eval '(module m racket/base
                (require (for-syntax racket/base))
                (begin-for-syntax
                  #;(log-error "nested")
                  ;; Using an environment variable to communicate across phases:
                  (when (getenv "PLT_ready_to_end")
                    #;(log-error "adios")
                    (custodian-shutdown-all (current-custodian))))))
        (eval '(module n racket/base
                (require (for-syntax racket/base))
                (begin-for-syntax
                  #;(log-error "outer")
                  (dynamic-require ''m 0)
                  (eval #f))))
        (putenv "PLT_ready_to_end" "yes")
        (dynamic-require ''n 0)
        #;(log-error "go")
        (eval #f))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check `namespace-mapped-symbols` and modidx shifting

(let ()
  (define tmp (make-temporary-file "~a-module-test" 'directory))
  (parameterize ([current-directory tmp]
                 [current-load-relative-directory tmp])
    (make-directory "compiled")
    (call-with-output-file*
     "compiled/a_rkt.zo"
     (lambda (o) (write (compile '(module a racket/base
                              (provide (all-defined-out))
                              (define a 1)
                              (define b 2)
                              (define c 3)))
                   o)))
    (call-with-output-file*
     "compiled/b_rkt.zo"
     (lambda (o) (write (compile '(module b racket/base
                              (require "a.rkt"
                                       ;; Force saving of context, instead of
                                       ;; reconstruction:
                                       (only-in racket/base [car extra-car]))))
                   o))))
  (dynamic-require (build-path tmp "b.rkt") #f)
  (define ns (module->namespace (build-path tmp "b.rkt")))
  (test #t
        'mapped-symbols
        (and (for/and ([name '(a b c)])
               (member name (namespace-mapped-symbols ns)))
             #t))
  (delete-directory/files tmp))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module exports-x*-as-x racket/base
  (define x* 5)
  (provide (rename-out [x* x])))

(module exports-x**-as-x racket/base
  (require 'exports-x*-as-x)
  (define x* 5)
  (define-syntax-rule (x**) x*)
  (provide (rename-out [x x***])
           (rename-out [x** x])))

(require 'exports-x**-as-x)
(test 5 'five (x))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check 'module-body-context-simple? and 'module-body-...context properties

(define (check-module-body-context-properties with-kar?)
  (define m (expand `(module m racket/base
                      ,@(if with-kar?
                            `((require (rename-in racket/base [car kar])))
                            null)
                      (define inside 7))))
  
  (test (not with-kar?) syntax-property m 'module-body-context-simple?)

  (define i (syntax-property m 'module-body-context))
  (define o (syntax-property m 'module-body-inside-context))
  
  (test #t syntax? i)
  (test #t syntax? o)
  
  (test car eval-syntax (datum->syntax i 'car))
  (test 'inside cadr (identifier-binding (datum->syntax i 'inside)))
  (test #f identifier-binding (datum->syntax o 'inside))
  (test (if with-kar? 'car #f)
        'kar-binding
        (let ([v (identifier-binding (datum->syntax i 'kar))])
          (and v (cadr v)))))

(check-module-body-context-properties #f)
(check-module-body-context-properties #t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that nesting `module+` under multiple `begin-for-syntax`
;; layers works

(module module-with-nested-module+s racket/base
  (require (for-syntax racket/base))
  (begin-for-syntax
    (require (for-syntax racket/base))
    (module+ test 1)
    (begin-for-syntax
      (require (for-syntax racket/base))
      (module+ test1 1)
      (begin-for-syntax
        (module+ test2 1)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check phase shifting in `dynamic-require`

(module module-with-phase-2-definition-of-x racket/base
  (require (for-syntax racket/base))
  (begin-for-syntax
    (require (for-syntax racket/base))
    (begin-for-syntax
      (provide x)
      (define x 5))))

(module module-that-exports-phase-2-x-at-phase-0 racket/base
  (require (for-meta -2 'module-with-phase-2-definition-of-x))
  (provide (for-meta -2 (all-from-out 'module-with-phase-2-definition-of-x))))

(test 5 dynamic-require ''module-that-exports-phase-2-x-at-phase-0 'x)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Check that
;; `namespace-anchor->namespace` internally enables top-level mode for
;; binding handling:

;; Example from Alex Knauth:
(module module-that-uses-eval-to-define-a-macro-in-its-own-namespace racket/base
  (require (for-syntax racket/base))
  (define-namespace-anchor a)
  (define ns (namespace-anchor->namespace a))
  (eval '(define-syntax x (λ (stx) #'333)) ns)
  (define result (eval 'x ns))
  (provide result))

(test 333 dynamic-require ''module-that-uses-eval-to-define-a-macro-in-its-own-namespace 'result)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that multiple imports of a name are disallowed
;; when they're from the from the same module but different
;; phases of that module

(module defines-a-at-two-phase-levels racket/base
  (require (for-syntax racket/base))
  
  (provide a (for-syntax a))
  
  (define a 0)
  (begin-for-syntax
    (define a 1)))

(err/rt-test (eval #'(module b racket/base
                       (require 'defines-a-at-two-phase-levels
                                (for-syntax racket/base
                                            'defines-a-at-two-phase-levels))))
             (lambda (exn)
               (regexp-match? #rx" already" (exn-message exn))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check re-export of an identifier from `#%kernel`
;; through a rename transformer:

(module rexports-values-from-kernel racket/base
  (require (for-syntax racket/base))
  (provide f)
  (define-syntax f (make-rename-transformer #'values)))

(dynamic-require ''rexports-values-from-kernel 'f)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that shifts associated with re-expansion are
;; properly tracked for `module->namespace`

(parameterize ([current-load-relative-directory
                (let-values ([(b n dir?)
                              (split-path
                               (collection-file-path "promise.rkt" "racket"))])
                  b)])
  (let* ([e (expand (namespace-syntax-introduce
                     (datum->syntax #f '(module m racket/base
                                         ;; A prefixed import forces saving of the
                                         ;; context as a syntax object in marshaled
                                         ;; form:
                                         (require (prefix-in p: "promise.rkt"))))))]
         [c (compile e)]
         [o (open-output-bytes)]
         [_ (write c o)]
         [r (parameterize ([read-accept-compiled #t])
              (read (open-input-bytes (get-output-bytes o))))])
    (parameterize ([current-namespace (make-base-namespace)])
      (eval r)
      (dynamic-require ''m #f)
      (parameterize ([current-namespace (module->namespace ''m)])
        (and (memq 'p:force (namespace-mapped-symbols))
             #t)))))

;; ----------------------------------------
;; Check that `syntax-source-module` is #f for a top-level evaluation
;; that starts outside of a module:

(define my-very-own-x 'x)

(define (make-module-that-has-a-complex-renaming v)
  `(module module-that-has-a-complex-renaming racket
    ;; this line is necessary, but you can require anything
    (require (rename-in racket/base [car prefix:car]))
    (module+ sub)
  
    (define my-very-own-x ,v)))

(eval (make-module-that-has-a-complex-renaming 10))
(parameterize ([current-module-declare-name
                (make-resolved-module-path 'module-that-has-a-complex-renaming2)])
  (eval (make-module-that-has-a-complex-renaming 11)))

(require 'module-that-has-a-complex-renaming)
(require 'module-that-has-a-complex-renaming2)
(require (submod 'module-that-has-a-complex-renaming sub))
(require (submod 'module-that-has-a-complex-renaming2 sub))

(parameterize ([current-namespace (module->namespace ''module-that-has-a-complex-renaming)])
  (test #f syntax-source-module (namespace-syntax-introduce #'my-very-own-x))
  (test 10 eval #'my-very-own-x))

(parameterize ([current-namespace (module->namespace ''module-that-has-a-complex-renaming2)])
  (test #f syntax-source-module (namespace-syntax-introduce #'my-very-own-x))
  (test 11 eval #'my-very-own-x))

(parameterize ([current-namespace (module->namespace '(submod 'module-that-has-a-complex-renaming sub))])
  (test #f syntax-source-module (namespace-syntax-introduce #'my-very-own-x))
  (test 10 eval 'my-very-own-x))

(parameterize ([current-namespace (module->namespace '(submod 'module-that-has-a-complex-renaming2 sub))])
  (test #f syntax-source-module (namespace-syntax-introduce #'my-very-own-x))
  (test 11 eval 'my-very-own-x))

(module provide-the-x-identifier racket/base
  (define x-id #'my-very-own-x)
  (provide x-id))

(parameterize ([current-namespace (module->namespace ''module-that-has-a-complex-renaming)])
  (test 'provide-the-x-identifier
        resolved-module-path-name
        (module-path-index-resolve (syntax-source-module
                                    (namespace-syntax-introduce 
                                     (dynamic-require ''provide-the-x-identifier 'x-id))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that `all-defined` exports at only the right phase

(module module-that-exports-at-phase-0-only racket/kernel
  (#%require (for-syntax racket/kernel))
  (#%provide (all-defined))
  (define-values (x) 1)
  (begin-for-syntax
    (define-values (x) 2)))

(module module-that-imports-at-multiple-phases racket/kernel
  (#%require 'module-that-exports-at-phase-0-only
             ;; Causes a collsion if the module exports too much
             (for-syntax 'module-that-exports-at-phase-0-only)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that a top-level binding doesn't interefere
;; with reference

(define very-confused-x 1)

(module m-that-defines-very-confused-x racket
  ;; this line is necessary, but you can require anything
  ;;(require (only-in racket/base))
  
  (define very-confused-x 10))

(require 'm-that-defines-very-confused-x)

(test 10
      'very-confused-x
      (parameterize ([current-namespace (module->namespace ''m-that-defines-very-confused-x)])
        ;; Note: #'very-confused-x will have top-level context
        ;; as well as the module context
        (eval #'very-confused-x)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure that re-expansion of a simple (in the sense of `require`
;; information kept for `module->namspace`) module body is ok

(module m racket/base
  (module mylang racket/base
    (require (for-syntax racket/base))
    (provide (rename-out [-#%module-begin #%module-begin]))
    (define-syntax (-#%module-begin stx)
      (syntax-case stx ()
        [(_ lng . rest)
         (with-syntax ([#%module-begin (datum->syntax #'lng '#%module-begin)])
           #`(#%plain-module-begin
              (require lng)
              (continue #%module-begin . rest)))]))
    (define-syntax (continue stx)
      (syntax-case stx ()
        [(_ lang-module-begin . rest)
         (let ([body-stx (local-expand
                          #'(lang-module-begin . rest)
                          'module-begin
                          (list))])
           (syntax-case body-stx (#%plain-module-begin)
             [(#%plain-module-begin . mod-body)
              #`(begin . mod-body)]))])))

  (module foo (submod ".." mylang) racket/base
          (module a-submod racket/base
            (define x 1)
            (provide x))
          (require 'a-submod)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
