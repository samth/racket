#lang racket/base
;; Report which iconv DLL the process loaded and how a short-output
;; ISO-8859-1 -> UTF-8 conversion behaves.
(require ffi/unsafe racket/port)

(define kernel32 (ffi-lib "kernel32.dll"))
(define GetModuleHandleW
  (get-ffi-obj "GetModuleHandleW" kernel32 (_fun _string/utf-16 -> _pointer)))
(define GetModuleFileNameW
  (get-ffi-obj "GetModuleFileNameW" kernel32
               (_fun _pointer _bytes _uint32 -> _uint32)))

(define (module-path name)
  (define h (GetModuleHandleW name))
  (and h
       (let* ([buf (make-bytes 2048)]
              [n (GetModuleFileNameW h buf 1024)])
         (cast (bytes-append (subbytes buf 0 (* 2 n)) #"\0\0")
               _bytes _string/utf-16))))

(define c (bytes-open-converter "ISO-8859-1" "UTF-8"))
(printf "converter: ~s\n" c)
(for ([n '("iconv-2.dll" "libiconv-2.dll" "iconv.dll" "libiconv.dll"
           "msvcrt.dll" "ucrtbase.dll" "vcruntime140.dll")])
  (printf "  ~a => ~a\n" n (module-path n)))
(when c
  (define dest (make-bytes 3))
  (define-values (got used status) (bytes-convert c #"ap\303" 0 3 dest))
  (printf "short convert: ~s ~s ~s (expect #\"ap\" 2 continues)\n"
          (subbytes dest 0 got) used status))
(when c
  (define raw (open-input-bytes #"ap\303\251ple"))
  (define conv (reencode-input-port raw "ISO-8859-1" #".!"))
  (define r1 (read-bytes 3 conv))
  (define r2 (read-bytes 2 raw))
  (define r3 (read-bytes 4 conv))
  (printf "port: ~s ~s ~s (expect #\"ap\\303\" #\"\\251p\" #\"\\203le\")\n" r1 r2 r3))
