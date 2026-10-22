;;; cse-census.ss
;;; Instrumentation for estimating the benefit of common-subexpression
;;; elimination. Included by cpcommonize.ss, so it runs once per compiled
;;; unit, after cp0, cptypes, and cpletrec.
;;;
;;; Controlled by environment variables, checked on each compilation:
;;;   CHEZ_CSE_CENSUS=<file>   append static counts (and examples) to <file>
;;;   CHEZ_CSE_DYNAMIC=1       also instrument code to count executions in the
;;;                            fxvector that is the global value of
;;;                            `$cse-census-counts`
;;;
;;; The census walks each expression in evaluation order, keeping a table of
;;; "available" candidate expressions. A candidate is a call to a pure (or
;;; mifoldable+discard) primitive or a record field reference whose operands
;;; are unassigned variables, constants, or other candidates. An expression is
;;; available at a point if an occurrence with the same key dominates that
;;; point (lexically, in a tree IR). A later occurrence of an available key is
;;; a "hit", i.e., something CSE could reuse.

(define cse-census
  (let ()
    (define cat-names
      '#(flonum fixnum generic-arith predicate record-pred check-pred record-ref/imm record-ref/mut
         $record-ref/imm $record-ref/mut $record-ref/var-rtd $record-ref/no-test
         pair-access length mutable-ref other-pure))
    (define ncat (vector-length cat-names))
    ;; counters per category
    (define cnt-names
      '#(occurrences hits hits/existing-var hits/no-call-between hits/no-store-between cross-lambda-hits))
    (define ncnt (vector-length cnt-names))
    (define C-occ 0)
    (define C-hit 1)
    (define C-hit-var 2)
    (define C-hit-nocall 3)
    (define C-hit-nostore 4)
    (define C-cross 5)
    (define (cat-index name)
      (let loop ([i 0])
        (cond
          [(fx= i ncat) ($oops 'cse-census "bad category ~s" name)]
          [(eq? (vector-ref cat-names i) name) i]
          [else (loop (fx+ i 1))])))
    (define mutable-cats '(record-ref/mut $record-ref/mut $record-ref/var-rtd $record-ref/no-test
                           pair-access mutable-ref))

    (define global-counts-sym '$cse-census-counts)
    (define init-globals
      (begin
        ($set-top-level-value! global-counts-sym (make-fxvector (fx* ncat ncnt) 0))
        ($set-top-level-value! '$cse-census-names (cons cat-names cnt-names))))

    (define (string-prefix? p s)
      (let ([n (string-length p)])
        (and (fx<= n (string-length s))
             (string=? p (substring s 0 n)))))

    (define cxr-names '(car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr cadddr cddddr
                         $car $cdr))
    (define length-names '(vector-length string-length bytevector-length fxvector-length flvector-length
                            $record-type-descriptor stencil-vector-mask))

    ;; returns category symbol or #f
    (define (primref-category pr)
      (let* ([flags (primref-flags pr)]
             [name (primref-name pr)]
             [s (symbol->string name)]
             [pure? (all-set? (prim-mask pure) flags)]
             [mifold? (all-set? (prim-mask (or mifoldable discard)) flags)])
        (cond
          [(eq? name '$record-ref) '$record-ref]
          [(not (or pure? mifold?)) #f]
          [(memq name cxr-names) 'pair-access]
          [(memq name length-names) 'length]
          [(not pure?) 'mutable-ref]
          [(or (string-prefix? "fl" s) (string-prefix? "$fl" s)
               (memq name '(fixnum->flonum real->flonum)))
           'flonum]
          [(or (string-prefix? "fx" s) (string-prefix? "$fx" s)) 'fixnum]
          [(all-set? (prim-mask arith-op) flags) 'generic-arith]
          [(memq name '(record? $sealed-record? record-instance? $record?)) 'record-pred]
          [(and (fx> (string-length s) 7)
                (string=? (substring s (fx- (string-length s) 7) (string-length s)) "-check?"))
           'check-pred]
          [(char=? (string-ref s (fx- (string-length s) 1)) #\?) 'predicate]
          [else 'other-pure])))

    (define (rtd-immutable-field? rtd index)
      (let ([flds ($object-ref 'scheme-object rtd (constant record-type-flds-disp))])
        (cond
          [(fixnum? flds)
           (not (bitwise-bit-set? ($object-ref 'scheme-object rtd (constant record-type-mpm-disp)) (fx+ index 1)))]
          [else
           (and (fx< index (length flds))
                (not (fld-mutable? (list-ref flds index))))])))

    (define-record-type entry
      (nongenerative)
      (sealed #t)
      (fields cat depth call-epoch store-epoch (mutable var?)))

    (lambda (x)
      (let ([static-file (getenv "CHEZ_CSE_CENSUS")]
            [dynamic? (let ([s (getenv "CHEZ_CSE_DYNAMIC")]) (and s (not (string=? s "")) (not (string=? s "0"))))])
        (if (not (or static-file dynamic?))
            x
            (let ([counts (make-fxvector (fx* ncat ncnt) 0)]
                  [avail (make-hashtable equal-hash equal?)]
                  [rtd-facts (make-eq-hashtable)]  ; prelex -> rtd expression from dominating test
                  [rhs-of (make-eq-hashtable)]     ; unassigned prelex -> bound expression
                  [aliases (make-eq-hashtable)]    ; prelex -> key of let-bound candidate
                  [prelex-ids (make-eq-hashtable)]
                  [undo '()]
                  [depth 0]
                  [call-epoch 0]
                  [store-epoch 0]
                  [examples '()]
                  [n-examples 0])
              (define (bump! cat c)
                (let ([i (fx+ (fx* (cat-index cat) ncnt) c)])
                  (fxvector-set! counts i (fx+ 1 (fxvector-ref counts i)))
                  i))
              (define pc (lambda () (make-preinfo-call)))
              (define (dyn-bump i e)
                (if dynamic?
                    (with-output-language (Lsrc Expr)
                      (let ([vec `(call ,(pc) ,(lookup-primref 3 '$top-level-value) (quote ,global-counts-sym))])
                        `(seq
                          (call ,(pc) ,(lookup-primref 3 'fxvector-set!) ,vec (quote ,i)
                            (call ,(pc) ,(lookup-primref 3 'fx+)
                              (call ,(pc) ,(lookup-primref 3 'fxvector-ref) ,vec (quote ,i))
                              (quote 1)))
                          ,e)))
                    e))
              (define (table-set! tbl k v)
                (set! undo (cons (list tbl k (hashtable-ref tbl k #f)) undo))
                (hashtable-set! tbl k v))
              (define (undo-to! mark)
                (let loop ()
                  (unless (eq? undo mark)
                    (let ([u (car undo)])
                      (set! undo (cdr undo))
                      (if (caddr u)
                          (hashtable-set! (car u) (cadr u) (caddr u))
                          (hashtable-delete! (car u) (cadr u)))
                      (loop)))))
              (define (prelex-key x)
                (or (hashtable-ref aliases x #f)
                    (list 'v (or (eq-hashtable-ref prelex-ids x #f)
                                 (let ([n (hashtable-size prelex-ids)])
                                   (eq-hashtable-set! prelex-ids x n)
                                   n))
                          (prelex-name x))))
              (define (trivial-key e)
                (nanopass-case (Lsrc Expr) e
                  [(ref ,maybe-src ,x) (and (not (prelex-assigned x)) (prelex-key x))]
                  [(quote ,d) (cons 'q d)]
                  [else #f]))
              (define (note-example! cat key)
                (when (fx< n-examples 20)
                  (set! n-examples (fx+ n-examples 1))
                  (set! examples (cons (cons cat key) examples))))
              ;; record an occurrence of a candidate; returns e, maybe instrumented
              (define (occurrence cat key e)
                (let ([ent (hashtable-ref avail key #f)]
                      [mutable? (memq cat mutable-cats)])
                  (let ([is (list (bump! cat C-occ))])
                    (define (fresh!)
                      (table-set! avail key (make-entry cat depth call-epoch store-epoch #f)))
                    (cond
                      [(not ent) (fresh!)]
                      [(fx< (entry-depth ent) depth)
                       (set! is (cons (bump! cat C-cross) is))
                       (fresh!)]
                      [else
                       (set! is (cons (bump! cat C-hit) is))
                       (note-example! cat key)
                       (when (entry-var? ent) (set! is (cons (bump! cat C-hit-var) is)))
                       (when (fx= (entry-call-epoch ent) call-epoch) (set! is (cons (bump! cat C-hit-nocall) is)))
                       (if (fx= (entry-store-epoch ent) store-epoch)
                           (set! is (cons (bump! cat C-hit-nostore) is))
                           (when mutable? (fresh!)))])
                    (fold-left (lambda (e i) (dyn-bump i e)) e is))))
              ;; For an rtd expression, returns a list of booleans (mutable? per field,
              ;; including parent fields), or #f if unknown. Recognizes a variable bound
              ;; to Racket's ($make-record-type-descriptor base name parent uid sealed opaque
              ;; '(count . mutable-mask) ...).
              (define (rtd-mutabilities rtd-e)
                (nanopass-case (Lsrc Expr) rtd-e
                  [(quote ,d)
                   (and (record-type-descriptor? d)
                        (let ([n (let ([flds ($object-ref 'scheme-object d (constant record-type-flds-disp))])
                                   (if (fixnum? flds) flds (length flds)))])
                          (let loop ([i 0])
                            (if (fx= i n) '() (cons (not (rtd-immutable-field? d i)) (loop (fx+ i 1)))))))]
                  [(ref ,maybe-src ,x)
                   (let ([rhs (hashtable-ref rhs-of x #f)])
                     (and rhs
                          (nanopass-case (Lsrc Expr) rhs
                            [(call ,preinfo ,pr ,e* ...)
                             (guard (memq (primref-name pr) '($make-record-type-descriptor $make-record-type-descriptor*
                                                              make-record-type-descriptor*))
                                    (fx>= (length e*) 7))
                             (let ([parent (list-ref e* 2)] [spec (list-ref e* 6)])
                               (let ([pm (nanopass-case (Lsrc Expr) parent
                                           [(quote ,d) (and (not d) '())]
                                           [else (rtd-mutabilities parent)])])
                                 (and pm
                                      (nanopass-case (Lsrc Expr) spec
                                        [(quote ,d)
                                         (and (pair? d) (fixnum? (car d)) (integer? (cdr d))
                                              (append pm
                                                      (let loop ([i 0])
                                                        (if (fx= i (car d))
                                                            '()
                                                            (cons (bitwise-bit-set? (cdr d) i) (loop (fx+ i 1)))))))]
                                        [else #f]))))]
                            [else #f])))]
                  [else #f]))
              (define (rtd-category rtd-e index)
                (if (not rtd-e)
                    '$record-ref/no-test
                    (let ([ms (rtd-mutabilities rtd-e)])
                      (if (and ms (fixnum? index) (fx>= index 0) (fx< index (length ms)))
                          (if (list-ref ms index) '$record-ref/mut '$record-ref/imm)
                          '$record-ref/var-rtd))))
              ;; test facts: (record? (ref x) rtd-e) or ($sealed-record? (ref x) rtd-e)
              (define (add-test-facts! e)
                (nanopass-case (Lsrc Expr) e
                  [(call ,preinfo ,pr (ref ,maybe-src ,x) ,e2)
                   (guard (memq (primref-name pr) '(record? $sealed-record? record-instance?)))
                   (table-set! rtd-facts x e2)]
                  [else (void)]))
              (define (no-return? e)
                (nanopass-case (Lsrc Expr) e
                  [(call ,preinfo ,e0 ,e* ...)
                   (or (preinfo-call-no-return? preinfo)
                       (nanopass-case (Lsrc Expr) e0
                         [,pr (all-set? (prim-mask abort-op) (primref-flags pr))]
                         [else #f]))]
                  [(seq ,e0 ,e1) (or (no-return? e0) (no-return? e1))]
                  [else #f]))
              (define (effectful-call!)
                (set! call-epoch (fx+ call-epoch 1))
                (set! store-epoch (fx+ store-epoch 1)))
              (define (Expr* e*)
                (map (lambda (e) (let-values ([(e k) (Expr e)]) e)) e*))
              (define (Lambda e)
                (nanopass-case (Lsrc Expr) e
                  [(case-lambda ,preinfo ,cl* ...)
                   (let ([saved-depth depth] [mark undo])
                     (set! depth (fx+ depth 1))
                     (let ([cl* (map (lambda (cl)
                                       (nanopass-case (Lsrc CaseLambdaClause) cl
                                         [(clause (,x* ...) ,interface ,body)
                                          (let-values ([(body k) (Expr body)])
                                            (with-output-language (Lsrc CaseLambdaClause)
                                              `(clause (,x* ...) ,interface ,body)))]))
                                     cl*)])
                       (undo-to! mark)
                       (set! depth saved-depth)
                       ;; a lambda body runs later, at unknown times
                       (with-output-language (Lsrc Expr) `(case-lambda ,preinfo ,cl* ...))))]
                  [else (let-values ([(e k) (Expr e)]) e)]))
              ;; returns (values new-e key-or-#f)
              (define (Expr e)
                (with-output-language (Lsrc Expr)
                  (nanopass-case (Lsrc Expr) e
                    [(ref ,maybe-src ,x) (values e (trivial-key e))]
                    [(quote ,d) (values e (trivial-key e))]
                    [(seq ,e0 ,e1)
                     (let*-values ([(e0 k0) (Expr e0)] [(e1 k1) (Expr e1)])
                       (values `(seq ,e0 ,e1) k1))]
                    [(if ,e0 ,e1 ,e2)
                     (let-values ([(e0 k0) (Expr e0)])
                       (let ([mark undo])
                         (cond
                           [(no-return? e2)
                            ;; facts and availability from the "then" branch survive the join
                            (let-values ([(e2 k2) (Expr e2)])
                              (undo-to! mark)
                              (add-test-facts! e0)
                              (let-values ([(e1 k1) (Expr e1)])
                                (values `(if ,e0 ,e1 ,e2) #f)))]
                           [(no-return? e1)
                            (let-values ([(e1 k1) (Expr e1)])
                              (undo-to! mark)
                              (let-values ([(e2 k2) (Expr e2)])
                                (values `(if ,e0 ,e1 ,e2) #f)))]
                           [else
                            (add-test-facts! e0)
                            (let-values ([(e1 k1) (Expr e1)])
                              (undo-to! mark)
                              (let-values ([(e2 k2) (Expr e2)])
                                (undo-to! mark)
                                (values `(if ,e0 ,e1 ,e2) #f)))])))]
                    [(case-lambda ,preinfo ,cl* ...) (values (Lambda e) #f)]
                    [(call ,preinfo0 (case-lambda ,preinfo1 (clause (,x* ...) ,interface ,body)) ,e* ...)
                     (guard (fx= interface (length e*)))
                     ;; let
                     (let ([ek* (map (lambda (e) (let-values ([(e k) (Expr e)]) (cons e k))) e*)])
                       (for-each (lambda (x ek)
                                   (let ([k (cdr ek)])
                                     (unless (prelex-assigned x) (hashtable-set! rhs-of x (car ek)))
                                     (when (and k (not (prelex-assigned x)))
                                       (table-set! aliases x k)
                                       (let ([ent (hashtable-ref avail k #f)])
                                         (when (and ent (fx= (entry-depth ent) depth))
                                           (entry-var?-set! ent #t))))))
                                 x* ek*)
                       (let-values ([(body kb) (Expr body)])
                         (values `(call ,preinfo0 (case-lambda ,preinfo1 (clause (,x* ...) ,interface ,body))
                                    ,(map car ek*) ...)
                                 kb)))]
                    [(call ,preinfo ,pr ,e* ...)
                     (let* ([ek* (map (lambda (e) (let-values ([(e k) (Expr e)]) (cons e k))) e*)]
                            [e* (map car ek*)]
                            [k* (map cdr ek*)]
                            [cat (primref-category pr)]
                            [new-e `(call ,preinfo ,pr ,e* ...)])
                       (cond
                         [(and cat (andmap (lambda (k) k) k*))
                          (let* ([cat (if (eq? cat '$record-ref)
                                          (let ([idx (and (= (length e*) 2)
                                                          (nanopass-case (Lsrc Expr) (cadr e*)
                                                            [(quote ,d) d]
                                                            [else #f]))]
                                                [rtd-e (and (pair? e*)
                                                            (nanopass-case (Lsrc Expr) (car e*)
                                                              [(ref ,maybe-src ,x)
                                                               (or (hashtable-ref rtd-facts x #f)
                                                                   (let ([rhs (hashtable-ref rhs-of x #f)])
                                                                     (and rhs
                                                                          (nanopass-case (Lsrc Expr) rhs
                                                                            [(call ,preinfo ,pr ,rtd-expr ,e* ...)
                                                                             (guard (eq? (primref-name pr) '$record))
                                                                             rtd-expr]
                                                                            [(record ,rtd ,rtd-expr ,e* ...) rtd-expr]
                                                                            [else #f]))))]
                                                              [else #f]))])
                                            (rtd-category rtd-e idx))
                                          cat)]
                                 [key (cons (primref-name pr) k*)])
                            (values (occurrence cat key new-e) key))]
                         [else
                          (unless (all-set? (prim-mask discard) (primref-flags pr))
                            (effectful-call!))
                          (values new-e #f)]))]
                    [(call ,preinfo ,e0 ,e* ...)
                     (let* ([e0 (Lambda e0)]
                            [e* (Expr* e*)])
                       (effectful-call!)
                       (values `(call ,preinfo ,e0 ,e* ...) #f))]
                    [(record-ref ,rtd ,type ,index ,e1)
                     (let-values ([(e1 k1) (Expr e1)])
                       (let ([new-e `(record-ref ,rtd ,type ,index ,e1)])
                         (if k1
                             (let ([key (list 'record-ref rtd index k1)]
                                   [cat (if (rtd-immutable-field? rtd index) 'record-ref/imm 'record-ref/mut)])
                               (values (occurrence cat key new-e) key))
                             (values new-e #f))))]
                    [(record-set! ,rtd ,type ,index ,e1 ,e2)
                     (let* ([e1 (Lambda e1)] [e2 (Lambda e2)])
                       (set! store-epoch (fx+ store-epoch 1))
                       (values `(record-set! ,rtd ,type ,index ,e1 ,e2) #f))]
                    [(set! ,maybe-src ,x ,e1)
                     (values `(set! ,maybe-src ,x ,(Lambda e1)) #f)]
                    [(letrec ([,x* ,e*] ...) ,body)
                     (let* ([e* (map Lambda e*)])
                       (let-values ([(body k) (Expr body)])
                         (values `(letrec ([,x* ,e*] ...) ,body) k)))]
                    [(letrec* ([,x* ,e*] ...) ,body)
                     (for-each (lambda (x e) (unless (prelex-assigned x) (hashtable-set! rhs-of x e))) x* e*)
                     (let* ([e* (map Lambda e*)])
                       (let-values ([(body k) (Expr body)])
                         (values `(letrec* ([,x* ,e*] ...) ,body) k)))]
                    [(record-type ,rtd ,e1) (values `(record-type ,rtd ,(Lambda e1)) #f)]
                    [(record-cd ,rcd ,rtd-expr ,e1)
                     (let* ([rtd-expr (Lambda rtd-expr)] [e1 (Lambda e1)])
                       (values `(record-cd ,rcd ,rtd-expr ,e1) #f))]
                    [(immutable-list (,e* ...) ,e1)
                     (let* ([e* (map Lambda e*)] [e1 (Lambda e1)])
                       (values `(immutable-list (,e* ...) ,e1) #f))]
                    [(immutable-vector (,e* ...) ,e1)
                     (let* ([e* (map Lambda e*)] [e1 (Lambda e1)])
                       (values `(immutable-vector (,e* ...) ,e1) #f))]
                    [(record ,rtd ,rtd-expr ,e* ...)
                     (let* ([rtd-expr (Lambda rtd-expr)] [e* (map Lambda e*)])
                       (values `(record ,rtd ,rtd-expr ,e* ...) #f))]
                    [(cte-optimization-loc ,box ,e1 ,exts)
                     (let-values ([(e1 k) (Expr e1)])
                       (values `(cte-optimization-loc ,box ,e1 ,exts) k))]
                    [(foreign (,conv* ...) ,name ,e1 (,arg-type* ...) ,result-type)
                     (values `(foreign (,conv* ...) ,name ,(Lambda e1) (,arg-type* ...) ,result-type) #f)]
                    [(fcallable (,conv* ...) ,e1 (,arg-type* ...) ,result-type)
                     (values `(fcallable (,conv* ...) ,(Lambda e1) (,arg-type* ...) ,result-type) #f)]
                    [else (values e #f)])))
              (let ([dbg (getenv "CHEZ_CSE_DEBUG")])
                (when dbg
                  (let ([op (open-file-output-port dbg (file-options no-fail no-truncate append)
                              (buffer-mode block) (native-transcoder))])
                    (parameterize ([print-gensym #f]) (pretty-print ($uncprep x) op))
                    (close-port op))))
              (let-values ([(new-x k) (Expr x)])
                (when static-file
                  (let ([op (open-file-output-port static-file
                              (file-options no-fail no-truncate append)
                              (buffer-mode block)
                              (native-transcoder))])
                    (put-string op "S")
                    (let loop ([i 0])
                      (unless (fx= i (fxvector-length counts))
                        (put-string op " ")
                        (put-string op (number->string (fxvector-ref counts i)))
                        (loop (fx+ i 1))))
                    (newline op)
                    (for-each (lambda (ex)
                                (put-string op "E ")
                                (put-datum op (car ex))
                                (put-string op " ")
                                (parameterize ([print-level 6] [print-length 6] [print-gensym #f])
                                  (put-datum op (cdr ex)))
                                (newline op))
                              (reverse examples))
                    (close-port op)))
                (if dynamic? new-x x))))))))
