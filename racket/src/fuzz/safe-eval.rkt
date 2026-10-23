#lang racket/base
;; Fuzz entry points for Racket CS.
;;
;; Each `fuzz-*` function takes the fuzzer's input as a byte string
;; and returns 0 for every expected outcome or 1 for a bug. It never
;; raises: the C driver calls it through `racket_apply`, which has no
;; exception handling.
;;
;; Untrusted code runs only inside `run-limited`, which gives it:
;;  - a fresh namespace and a weak code inspector, so protected modules
;;    (`#%unsafe`, `#%foreign`, `#%linklet`, `ffi/unsafe/vm`, ...) and
;;    `(#%declare #:unsafe)` are unavailable;
;;  - a security guard that permits only reading installed libraries;
;;  - a custodian with a memory limit;
;;  - a fuel limit counted in scheduler quanta, so every run halts
;;    without depending on the clock;
;;  - bounded output and no exit.

(require setup/dirs
         racket/string
         racket/fasl
         racket/extflonum)

(provide fuzz-read
         fuzz-eval
         fuzz-print-roundtrip
         fuzz-number
         fuzz-regexp
         fuzz-fasl
         run-limited
         bug-exn?
         fuzz-debug-port)

;; ----------------------------------------
;; Limits

(define memory-limit (* 256 1024 1024))
(define fuel-quanta 400)       ; scheduler quanta for untrusted code
(define output-limit (* 64 1024))
(define max-forms 256)

;; ----------------------------------------
;; Oracles

;; When a port, every exception caught from untrusted code is written
;; there; used to check the harness by hand, never while fuzzing.
(define fuzz-debug-port (make-parameter #f))

(define (note-exn e)
  (define p (fuzz-debug-port))
  (when p
    (fprintf p "  caught: ~a\n" (if (exn? e) (exn-message e) e))))

;; Chez reports a fault inside Scheme code as this exception, so a
;; memory-safety bug can look like an ordinary error.
(define (bug-exn? e)
  (and (exn? e)
       (let ([msg (exn-message e)])
         (or (string-contains? msg "invalid memory reference")
             (string-prefix? msg "internal error")))))

;; ----------------------------------------
;; Security guard: read-only access to installed libraries

(define allowed-roots
  (for/list ([d (in-list (append (list (find-collects-dir)
                                       (find-pkgs-dir)
                                       (find-share-dir)
                                       (find-lib-dir)
                                       (find-config-dir)
                                       (find-system-path 'addon-dir))
                                 (get-collects-search-dirs)))]
             #:when d)
    (path->string (simplify-path (path->complete-path d) #f))))

;; `simplify-path` without filesystem access, so the guard does not
;; call itself.
(define (allowed-path? p)
  (define s (path->string (simplify-path (path->complete-path p) #f)))
  (for/or ([r (in-list allowed-roots)])
    (string-prefix? s r)))

(define (deny who what)
  (raise (exn:fail (format "~a: ~a denied in fuzz evaluator" who what)
                   (current-continuation-marks))))

(define fuzz-guard
  (make-security-guard
   (current-security-guard)
   (lambda (who path modes)
     ;; Existence checks are harmless and the module name resolver
     ;; makes them everywhere; reading is limited to the libraries.
     (when path
       (unless (or (equal? modes '(exists))
                   (and (null? (remq* '(read exists) modes))
                        (allowed-path? path)))
         (deny who "file access"))))
   (lambda (who host port mode) (deny who "network access"))
   (lambda (who path target) (deny who "link creation"))))

;; ----------------------------------------
;; Output that keeps at most `output-limit` bytes and drops the rest

(define (make-bounded-output-port)
  (define count 0)
  (make-output-port
   'fuzz-output
   always-evt
   (lambda (bstr start end non-block? breakable?)
     (set! count (min output-limit (+ count (- end start))))
     (- end start))
   void))

;; ----------------------------------------
;; Namespaces

(define-namespace-anchor anchor)
(define harness-ns (namespace-anchor->namespace anchor))
(define orig-code-inspector (current-code-inspector))

;; Must be called with the weak code inspector installed, so that the
;; new namespace (and code compiled in it) is under that inspector.
(define (make-fuzz-namespace)
  (define ns (make-empty-namespace))
  (namespace-attach-module harness-ns 'racket/base ns)
  (parameterize ([current-namespace ns])
    (namespace-require 'racket/base))
  ns)

;; Installed libraries are loaded with the original inspector, as
;; `racket/sandbox` does; nothing else may be loaded as compiled code.
(define (make-load/use-compiled orig-handler)
  (lambda (path modname)
    (if (allowed-path? path)
        (parameterize ([current-code-inspector orig-code-inspector]
                       [current-security-guard (current-security-guard)])
          (orig-handler path modname))
        (deny 'load "loading code"))))

;; ----------------------------------------
;; Running untrusted work

;; Runs `thunk` (a procedure of no arguments that returns 0 or 1) under
;; the limits described at the top. Returns 0 or 1.
(define (run-limited thunk)
  (define cust (make-custodian))
  (custodian-limit-memory cust memory-limit cust)
  (define result 0)
  (define out (make-bounded-output-port))
  (define th
    (parameterize ([current-custodian cust]
                   [current-code-inspector (make-inspector orig-code-inspector)]
                   [current-inspector (make-inspector (current-inspector))]
                   [current-security-guard fuzz-guard]
                   [current-load/use-compiled
                    (make-load/use-compiled (current-load/use-compiled))]
                   [current-environment-variables (make-environment-variables)]
                   [current-command-line-arguments (vector)]
                   [current-input-port (open-input-bytes #"")]
                   [current-output-port out]
                   [current-error-port out]
                   [read-accept-reader #f]
                   [read-accept-lang #f]
                   [read-accept-compiled #f])
      (parameterize ([current-namespace (make-fuzz-namespace)])
        (thread
         (lambda ()
           (let/ec esc
             (parameterize ([exit-handler (lambda (v) (esc (void)))])
               (with-handlers ([(lambda (e) #t)
                                (lambda (e)
                                  (note-exn e)
                                  (when (bug-exn? e) (set! result 1)))])
                 (set! result (thunk))))))))))
  ;; Watchdog: each `(sleep 0)` lets the other threads run for up to
  ;; one quantum, so this loop bounds the work they can do.
  (let loop ([n 0])
    (cond
      [(thread-dead? th) (void)]
      [(>= n fuel-quanta) (note-exn "out of fuel")]
      [else (sleep 0) (loop (add1 n))]))
  (custodian-shutdown-all cust)
  result)

;; Wraps an entry point so that nothing escapes to C.
(define-syntax-rule (define-entry (name bstr) body ...)
  (define (name bstr)
    (with-handlers ([(lambda (e) #t)
                     (lambda (e) (if (bug-exn? e) 1 0))])
      body ...)))

;; ----------------------------------------
;; Reading forms

(define (read-forms in)
  (port-count-lines! in)
  (let loop ([acc '()] [n 0])
    (define v (read-syntax 'fuzz in))
    (if (or (eof-object? v) (= n max-forms))
        (reverse acc)
        (loop (cons v acc) (add1 n)))))

;; ----------------------------------------
;; Targets

;; Reader only.
(define-entry (fuzz-read bstr)
  (run-limited
   (lambda ()
     (read-forms (open-input-bytes bstr))
     0)))

;; Expand, compile and run each top-level form.
(define-entry (fuzz-eval bstr)
  (run-limited
   (lambda ()
     (for ([form (in-list (read-forms (open-input-bytes bstr)))])
       (with-handlers ([(lambda (e) (not (bug-exn? e))) note-exn])
         (eval (namespace-syntax-introduce form))))
     0)))

;; Readable data must read back as itself after `write`.
(define-entry (fuzz-print-roundtrip bstr)
  (run-limited
   (lambda ()
     (for/fold ([r 0]) ([stx (in-list (read-forms (open-input-bytes bstr)))])
       (define v (syntax->datum stx))
       (define s (let ([o (open-output-bytes)]) (write v o) (get-output-bytes o)))
       ;; `read-syntax`, like the first read, so strings are immutable
       ;; on both sides (`read` makes mutable strings, which
       ;; `equal-always?` hash tables compare by identity).
       (define v2 (syntax->datum (read-syntax 'fuzz (open-input-bytes s))))
       (if (or (equal? v v2) (has-unreadable? v))
           r
           1)))))

;; Values whose printed form is not expected to read back as `equal?`.
(define (has-unreadable? v)
  (let loop ([v v])
    (cond
      [(pair? v) (or (loop (car v)) (loop (cdr v)))]
      [(vector? v) (for/or ([x (in-vector v)]) (loop x))]
      [(box? v) (loop (unbox v))]
      [(hash? v) (for/or ([(k x) (in-hash v)]) (or (loop k) (loop x)))]
      [(prefab-struct-key v) (for/or ([x (in-vector (struct->vector v))]) (loop x))]
      [(symbol? v) (not (symbol-interned? v))]
      ;; +nan.0 is not `equal?` to itself in every representation
      [(and (flonum? v) (not (= v v))) #t]
      ;; Racket CS has no extflonum arithmetic; extflonums read back
      ;; as fresh, non-`equal?` values
      [(extflonum? v) #t]
      [else #f])))

;; A numeric literal must survive number->string and string->number.
(define-entry (fuzz-number bstr)
  (run-limited
   (lambda ()
     (define s (bytes->string/utf-8 bstr #\?))
     (define n (string->number s 10 'number-or-false 'decimal-as-exact))
     (cond
       [(and n (exact? n))
        (define s2 (number->string n))
        (if (equal? (string->number s2) n) 0 1)]
       [(and n (real? n) (= n n))
        (define s2 (number->string n))
        (if (eqv? (string->number s2) n) 0 1)]
       [else 0]))))

;; The input is a pattern and a subject separated by the first NUL byte.
(define-entry (fuzz-regexp bstr)
  (run-limited
   (lambda ()
     (define parts (regexp-split #rx#"\0" bstr))
     (define pat (car parts))
     (define subject (if (null? (cdr parts)) #"" (cadr parts)))
     (define rx (with-handlers ([exn:fail? (lambda (e) #f)])
                  (byte-pregexp pat)))
     (cond
       [rx
        (define m (regexp-match rx subject))
        (define p (regexp-match-positions rx subject))
        ;; The two must agree on whether, and where, there is a match.
        (cond
          [(and (not m) (not p)) 0]
          [(and m p
                (= (length m) (length p))
                (for/and ([s (in-list m)] [pos (in-list p)])
                  (if s
                      (and pos (equal? s (subbytes subject (car pos) (cdr pos))))
                      (not pos))))
           0]
          [else 1])]
       [else 0]))))

;; `racket/fasl` is implemented in Racket; decoding must not fail
;; except with `exn:fail`, and a decoded value must re-encode to an
;; equal value.
(define-entry (fuzz-fasl bstr)
  (run-limited
   (lambda ()
     (define v (with-handlers ([exn:fail? (lambda (e) (void))])
                 (fasl->s-exp bstr #:datum-intern? #f)))
     (cond
       [(void? v) 0]
       [else
        (define v2 (fasl->s-exp (s-exp->fasl v)))
        (if (or (equal? v v2) (has-unreadable? v)) 0 1)]))))
