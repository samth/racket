(module cons '#%kernel
  ;(#%provide cons list)
  (#%provide (rename -cons cons)
             (rename -list list))
  (define-values (ht) (make-hash))
  (exit-handler
   (let-values ([(old) (exit-handler)])
     (lambda args
       (write ht)
       (newline)
       (apply old args))))
  (define-values (freq) 0.0001)
  (define-values (-list)
    (lambda args
      (if (null? args) args
          (-cons (car args) (apply -list (cdr args))))))
  (define-values (-cons)
    (lambda (a b)
      (if (< (random) freq)
          (let-values ([(k) (continuation-mark-set->list (current-continuation-marks) 'performance-region)])
            (let-values ([(ref) (hash-ref ht k #f)])
              (if ref
                  (hash-set! ht k (+ 1 ref))
                  (hash-set! ht k 1))
              (cons a b)))
          (cons a b)))))
