#lang racket/base

(require racket/port)

(define (port-trial)
  (let* ([raw (open-input-bytes #"ap\303\251ple")]
         [converted (reencode-input-port raw "ISO-8859-1" #".!")])
    (list (read-bytes 3 converted)
          (read-bytes 2 raw)
          (read-bytes 4 converted)
          (read-bytes 5 converted))))

(define (port-result-ok? r)
  (and (equal? (list-ref r 0) #"ap\303")
       (equal? (list-ref r 1) #"\251p")
       (equal? (list-ref r 2) #"\203le")
       (eof-object? (list-ref r 3))))

(define (converter-trial)
  (let ([c (bytes-open-converter "ISO-8859-1" "UTF-8")])
    (unless c (error 'converter-trial "ISO-8859-1 converter unavailable"))
    (let ([dest (make-bytes 3)])
      (let-values ([(got used status)
                    (bytes-convert c #"ap\303" 0 3 dest)])
        (bytes-close-converter c)
        (list (subbytes dest 0 got) used status)))))

(define port-failures 0)
(define converter-failures 0)

(for ([i (in-range 200)])
  (let* ([port-result (port-trial)]
         [converter-result (converter-trial)])
    (unless (port-result-ok? port-result)
      (set! port-failures (add1 port-failures))
      (when (<= port-failures 5)
        (printf "port trial ~a: ~s\n" i port-result)))
    (unless (equal? converter-result (list #"ap" 2 'continues))
      (set! converter-failures (add1 converter-failures))
      (when (<= converter-failures 5)
        (printf "converter trial ~a: ~s\n" i converter-result)))))

(printf "Racket ~a, port failures ~a/200, converter failures ~a/200\n"
        (version) port-failures converter-failures)
(exit (if (zero? (+ port-failures converter-failures)) 0 1))
