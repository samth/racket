#lang scheme/base

(require scribble/manual
         scribble/struct
         setup/getinfo
         setup/main-collects
         setup/dirs
         scheme/list
         scheme/match
         "../config.rkt")

(provide make-start-page)

(define-struct sec (cat label))

(define sections
  (map (lambda (xs) (apply make-sec xs)) manual-sections))

(define (add-sections cat mk-sep l)
  (if (null? l)
    null
    (let loop ([l l] [key (if (equal? "" (caddar l)) (caar l) +inf.0)])
      (cond [(null? l) null]
            [(equal? (caar l) key) (cons (cadar l) (loop (cdr l) key))]
            [else (let ([lbl (caddar l)] ; currently always ""
                        [l (cons (cadar l) (loop (cdr l) (caar l)))]
                        [sep? (not (= (truncate (/ key 10))
                                      (truncate (/ (caar l) 10))))])
                    (if sep? (cons (mk-sep lbl) l) l))]))))

(define (get-docs all? tag)
  (let* ([recs (find-relevant-directory-records (list tag) 'all-available)]
         [infos (map get-info/full (map directory-record-path recs))]
         [main-dirs (parameterize ([current-library-collection-paths
                                    (list (find-collects-dir))])
                      (for/hash ([k (in-list (find-relevant-directories (list tag) 'no-planet))])
                        (values k #t)))]
         [docs (append-map
                (lambda (i rec)
                  (define dir (directory-record-path rec))
                  (define s (and (or all? (hash-ref main-dirs dir #f))
                                 i
                                 (i tag)))
                  (if (not s)
                    null
                    (filter-map
                     (lambda (d)
                       (if (and (not all?)
                                (pair? (cdr d))
                                (or (memq 'user-doc (cadr d))
                                    (memq 'user-doc-root (cadr d))))
                         #f
                         (let* ([new-cat (if ((length d) . > . 2)
                                           (caddr d)
                                           '(library))]
                                [sub-cat (and (list? new-cat)
                                              ((length new-cat) . > . 1)
                                              (cadr new-cat))])
                           (list
                            ;; Category
                            (let ([the-cat
                                   (if (pair? new-cat) (car new-cat) 'unknown)])
                              (or (and (eq? the-cat 'omit) the-cat)
                                  (ormap (lambda (sec)
                                           (and (eq? the-cat (sec-cat sec))
                                                the-cat))
                                         sections)
                                  'library))
                            ;; Priority
                            (if (and sub-cat (real? sub-cat)) sub-cat 0)
                            ;; Priority label (not used):
                            ""
                            ;; Path
                            (build-path dir (if (pair? d) (car d) "???"))
                            ;; Spec
                            (let ([spec (directory-record-spec rec)])
                              (list* (car spec)
                                     (if (pair? d) (car d) "UNKNOWN")
                                     (if (eq? 'planet (car spec))
                                         (list (append (cdr spec)
                                                       (list (directory-record-maj rec)
                                                             (list '= (directory-record-min rec)))))
                                         (cdr spec))))))))
                     s)))
                infos
                recs)])
    docs))

(define (maybe-cdr l)
  ;; drop initial separator, if there are any lines at all
  (if (null? l)
      null
      (cdr l)))

(define (make-start-page all?)
  (let* ([docs (get-docs all? 'scribblings)]
         [plain-line
          (lambda content
            (list (make-flow (list (make-paragraph content)))))]
         [line
          (lambda (spec)
            (plain-line (hspace 2) 
                        (if (element? spec)
                            spec
                            (other-manual spec #:underline? #f))))])
    (define (contents renderer part resolve-info)
      (make-table
       #f
       (maybe-cdr
        (append-map
         (lambda (sec)
           (let ([docs (filter (lambda (doc) (eq? (car doc) (sec-cat sec)))
                               docs)])
             (cond [(null? docs)
                    ;; Drop section if it contains no manuals.
                    null]
                   [else
                    (list*
                     (plain-line (hspace 1))
                     (plain-line (let loop ([s (sec-label sec)])
                                   (match s
                                     [(list 'elem parts ...)
                                      (apply elem (map loop parts))]
                                     [(list 'link text doc-mod-path)
                                      (seclink "top" #:doc doc-mod-path #:underline? #f text)]
                                     [(list 'link text doc-mod-path tag)
                                      (seclink tag #:doc doc-mod-path #:underline? #f text)]
                                     [_ s])))
                     (add-sections
                      (sec-cat sec)
                      (lambda (str)
                        (plain-line
                         (make-element (if (string=? str "") "sepspace" "septitle")
                                       (list 'nbsp str))))
                      (sort (map (lambda (doc)
                                   (list (cadr doc) (line (cadddr (cdr doc))) (caddr doc)))
                                 docs)
                            (lambda (ad bd)
                              (if (= (car ad) (car bd))
                                  (let ([str (lambda (x)
                                               (element->string
                                                (cadr (paragraph-content
                                                       (car (flow-paragraphs
                                                             (caadr x)))))
                                                renderer part resolve-info))])
                                    (string-ci<? (str ad) (str bd)))
                                  (> (car ad) (car bd)))))))])))
         sections))))
    (make-delayed-block contents)))
