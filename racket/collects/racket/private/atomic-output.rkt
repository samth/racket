(module atomic-output "pre-base.rkt"
  (require (prefix-in k: "pre-base.rkt"))

  (provide create-temporary-path
           call-with-atomic-output-file/prepare
           call-with-atomic-replacement)

  (define (create-temporary-path base-dir make-name create)
    (define tmpdir (find-system-path 'temp-dir))
    (let loop ([s (current-seconds)]
               [ms (inexact->exact (truncate (current-inexact-milliseconds)))]
               [tries 0])
      (define pth
        (path->complete-path
         (let ([n (make-name
                   ;; docs promise argument will be a string containing only digits
                   (format "~a~a" s ms))])
           (cond [base-dir (build-path base-dir n)]
                 [(relative-path? n) (build-path tmpdir n)]
                 [else n]))))
      (with-handlers ([(lambda (exn)
                         ;; FIXME: There should probably be a maximum number of
                         ;; tries for other cases, too.
                         ;; See: https://github.com/racket/racket/pull/3870#discussion_r741177829
                         (or (exn:fail:filesystem:exists? exn)
                             (and (exn:fail:filesystem:errno? exn)
                                  (let ([errno (exn:fail:filesystem:errno-errno exn)])
                                    (and (eq? 'windows (cdr errno))
                                         (eqv? (car errno) 5) ; ERROR_ACCESS_DENIED
                                         ;; On Windows, if the target path refers to a file
                                         ;; that has been deleted but is still open
                                         ;; somewhere, then an access-denied error is reported
                                         ;; instead of a file-exists error; there appears
                                         ;; to be no way to detect that it was really a
                                         ;; file-still-exists error. Try again for a while.
                                         ;; There's still a small chance that this will
                                         ;; fail, but it's vanishingly small at 32 tries.
                                         ;; If ERROR_ACCESS_DENIED really is the right
                                         ;; error (e.g., because the target directory is not
                                         ;; writable), we'll take longer to get there.
                                         (tries . < . 32))))))
                       (lambda (x)
                         ;; try again with a new name
                         ;; TODO: should this use `crypto-random-bytes`, like FreeBSD, per SEI CERT?
                         ;; See: https://github.com/racket/racket/pull/3870#issuecomment-957779099
                         (loop (- s (random 10))
                               (+ ms (random 10))
                               (add1 tries)))])
        (create pth))))

  (define (call-with-atomic-replacement path proc mode perms replace-permissions?)
    (define target (path->complete-path path))
    (define-values (base _name _dir?) (split-path target))
    (define (create create-file)
      (create-temporary-path
       (if (path? base) base (current-directory))
       (lambda (digits) (format "atomic-output-~a.tmp" digits))
       create-file))
    (call-with-atomic-output-file/prepare
     target (lambda (out _) (proc out)) #f #f
     (lambda ()
       (create
        (lambda (tmp-path)
          (values tmp-path
                  (k:open-output-file tmp-path mode 'error perms
                                      (and replace-permissions? 'replace-permissions))))))
     (lambda ()
       (create
        (lambda (tmp-path)
          (close-output-port (k:open-output-file tmp-path 'binary 'error #o666 #f))
          tmp-path)))))

  (define (call-with-atomic-output-file/prepare path proc guard rename-fail-handler prepare backup)
    (define (try-delete-file path)
      (with-handlers ([exn:fail:filesystem? void])
        (delete-file path)))
    (define bp (current-break-parameterization))
    (parameterize-break #f
      (define-values (tmp-path prepared-out)
        (parameterize ([current-security-guard (or guard (current-security-guard))])
          (prepare)))
      (define ok? #f)
      (dynamic-wind
       void
       (lambda ()
         (begin0
           (let ([out (or prepared-out
                          (parameterize ([current-security-guard (or guard (current-security-guard))])
                            (k:open-output-file tmp-path 'binary 'truncate/replace #o666 #f)))])
             (dynamic-wind
              void
              (lambda ()
                (call-with-break-parameterization bp (lambda () (proc out tmp-path))))
              (lambda ()
                (close-output-port out))))
           (set! ok? #t)))
       (lambda ()
         (parameterize ([current-security-guard (or guard (current-security-guard))])
           (if ok?
               (with-handlers ([void (lambda (exn)
                                       (try-delete-file tmp-path)
                                       (raise exn))])
                 (if (eq? (system-type) 'windows)
                     (cond
                       [rename-fail-handler
                        (let loop ()
                          (with-handlers* ([exn:fail:filesystem?
                                            (lambda (exn)
                                              (call-with-break-parameterization
                                               bp
                                               (lambda () (rename-fail-handler exn tmp-path)))
                                              (loop))])
                            (rename-file-or-directory tmp-path path #t)
                            void))]
                       [else
                        (let ([tmp-path2 (backup)])
                          (with-handlers ([exn:fail:filesystem? void])
                            (rename-file-or-directory path tmp-path2 #t))
                          (rename-file-or-directory tmp-path path #t)
                          (try-delete-file tmp-path2))])
                     (rename-file-or-directory tmp-path path #t)))
               (try-delete-file tmp-path))))))))
