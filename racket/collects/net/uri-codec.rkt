#lang racket/base

#|
People used to wonder why semicolons were the default.  We then
decided to switch the default back to ampersands --

  http://www.w3.org/TR/html401/appendix/notes.html#h-B.2.2

  We recommend that HTTP server implementors, and in particular, CGI
  implementors support the use of ";" in place of "&" to save authors
  the trouble of escaping "&" characters in this manner.

See more in PR8831.
|#

;;; <uri-codec.rkt> ---- En/Decode URLs and form-urlencoded data
;;; Time-stamp: <03/04/25 10:31:31 noel>
;;;
;;; Copyright (C) 2002 by Noel Welsh.
;;;
;;; This file is part of Net.

;;; Net is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.

;;; Net is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public
;;; License along with Net; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;; 02110-1301 USA.

;;; Author: Noel Welsh <noelwelsh@yahoo.com>
;;
;; Commentary:

;; The module provides functions to encode and decode strings using
;; the URI encoding rules given in RFC 2396, and to encode and decode
;; name/value pairs using the application/x-www-form-urlencoded
;; mimetype given the in HTML 4.0 specification.  There are minor
;; differences between the two encodings.

;; The URI encoding uses allows a few characters to be represented `as
;; is': a-z, A-Z, 0-9, -, _, ., !, ~, *, ', ( and ).  The remaining
;; characters are encoded as %xx, where xx is the hex representation
;; of the integer value of the character (where the mapping
;; character<->integer is determined by US-ASCII if the integer is
;; <128).

;; The encoding, inline with RFC 2396's recommendation, represents a
;; character as is, if possible.  The decoding allows any characters
;; to be represented by their hex values, and allows characters to be
;; incorrectly represented `as is'.

;; The rules for the application/x-www-form-urlencoded mimetype given
;; in the HTML 4.0 spec are:

;;   1. Control names and values are escaped. Space characters are
;;   replaced by `+', and then reserved characters are escaped as
;;   described in [RFC1738], section 2.2: Non-alphanumeric characters
;;   are replaced by `%HH', a percent sign and two hexadecimal digits
;;   representing the ASCII code of the character. Line breaks are
;;   represented as "CR LF" pairs (i.e., `%0D%0A').

;;   2. The control names/values are listed in the order they appear
;;   in the document. The name is separated from the value by `=' and
;;   name/value pairs are separated from each other by `&'.

;; NB: RFC 2396 supersedes RFC 1738.

;; This differs slightly from the straight encoding in RFC 2396 in
;; that `+' is allowed, and represents a space.  We follow this
;; convention, encoding a space as `+' and decoding `+' as a space.
;; There appear to be some brain-dead decoders on the web, so we also
;; encode `!', `~', `'', `(' and ')' using their hex representation.
;; This is the same choice as made by the Java URLEncoder.

;; Draws inspiration from encode-decode.scm by Kurt Normark and a code
;; sample provided by Eli Barzilay

(require racket/string)

(provide uri-encode                         uri-decode
         uri-path-segment-encode            uri-path-segment-decode
         uri-userinfo-encode                uri-userinfo-decode
         uri-unreserved-encode              uri-unreserved-decode
         uri-path-segment-unreserved-encode uri-path-segment-unreserved-decode
         form-urlencoded-encode             form-urlencoded-decode
         alist->form-urlencoded             form-urlencoded->alist
         current-alist-separator-mode)

(define (self-map-chars str) (map (λ (ch) (cons ch ch)) (string->list str)))

;; The characters that always map to themselves
(define alphanumeric-mapping
  (self-map-chars
   "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))

;; Characters that sometimes map to themselves
;; called 'mark' in RFC 3986
(define safe-mapping (self-map-chars "-_.!~*'()"))

;; The strict URI mapping
(define uri-mapping `(,@alphanumeric-mapping ,@safe-mapping))

;; The uri path segment mapping from RFC 3986
(define path-segment-extra-mapping (self-map-chars "@+,=$&:"))
(define uri-path-segment-mapping `(,@uri-mapping ,@path-segment-extra-mapping))

;; from RFC 3986
(define unreserved-mapping `(,@alphanumeric-mapping ,@(self-map-chars "-._~")))

;; The uri path segment mapping from RFC 3986
(define uri-path-segment-unreserved-mapping
  `(,@unreserved-mapping ,@path-segment-extra-mapping))

;; from RFC 3986
(define sub-delims-mapping (self-map-chars "!$&'()*+,;="))

;; The uri userinfo mapping from RFC 3986
(define uri-userinfo-mapping
  `(,@unreserved-mapping ,@sub-delims-mapping ,@(self-map-chars ":")))

;; The form-urlencoded mapping
(define form-urlencoded-mapping
  `(,@(self-map-chars ".-*_") (#\space . #\+) ,@alphanumeric-mapping))

(define (number->hex-string number)
  (define (hex n) (string-ref "0123456789ABCDEF" n))
  (string #\% (hex (quotient number 16)) (hex (modulo number 16))))

(define ascii-size 128)

;; (listof (cons char char)) -> (values (vectorof string) (vectorof int))
(define (make-codec-tables alist)
  (define encoding-table (build-vector ascii-size number->hex-string))
  (define decoding-table (build-vector ascii-size values))
  (for ([orig+enc (in-list alist)])
    (vector-set! encoding-table
                 (char->integer (car orig+enc))
                 (string (cdr orig+enc)))
    (vector-set! decoding-table
                 (char->integer (cdr orig+enc))
                 (char->integer (car orig+enc))))
  (values encoding-table decoding-table))

;; vector string -> string
(define (encode table str)
  ;; First, check for an ASCII string with no conversion needed:
  (if (for/and ([char (in-string str)])
        (define v (char->integer char))
        (and (< v ascii-size)
             (let ([s (vector-ref table v)])
               (and (= 1 (string-length s))
                    (eq? char (string-ref s 0))))))
      str
      (let ([out (open-output-string)])
        (for ([byte (in-bytes (string->bytes/utf-8 str))])
          (cond [(< byte ascii-size)
                 (write-string (vector-ref table byte) out)]
                [else
                 (write-string (number->hex-string byte) out)]))
        (get-output-string out))))

;; vector string -> string
(define (decode table str)
  ;; First, check for a string with no conversion needed:
  (if (for/and ([char (in-string str)])
        (define v (char->integer char))
        (if (< v ascii-size)
            (and (not (eqv? char #\%))
                 (eqv? v (vector-ref table v)))
            #t))
      (string-copy str)
      (decode/slow table str)))

;; Decodes into a byte string, which is then converted from UTF-8 with
;; #\uFFFD for invalid sequences, as `get-output-string' would.
(define (decode/slow table str)
  (define len (string-length str))
  ;; Each character becomes at most 4 bytes:
  (define out (make-bytes (* 4 len)))
  (define (write-char! c pos)
    (define v (char->integer c))
    (define n (char-utf-8-length c))
    (case n
      [(1) (bytes-set! out pos v)]
      [(2) (bytes-set! out pos (bitwise-ior #xC0 (arithmetic-shift v -6)))
           (bytes-set! out (+ pos 1) (bitwise-ior #x80 (bitwise-and v #x3F)))]
      [(3) (bytes-set! out pos (bitwise-ior #xE0 (arithmetic-shift v -12)))
           (bytes-set! out (+ pos 1) (bitwise-ior #x80 (bitwise-and (arithmetic-shift v -6) #x3F)))
           (bytes-set! out (+ pos 2) (bitwise-ior #x80 (bitwise-and v #x3F)))]
      [else (bytes-set! out pos (bitwise-ior #xF0 (arithmetic-shift v -18)))
            (bytes-set! out (+ pos 1) (bitwise-ior #x80 (bitwise-and (arithmetic-shift v -12) #x3F)))
            (bytes-set! out (+ pos 2) (bitwise-ior #x80 (bitwise-and (arithmetic-shift v -6) #x3F)))
            (bytes-set! out (+ pos 3) (bitwise-ior #x80 (bitwise-and v #x3F)))])
    (+ pos n))
  (define (hex-value c)
    (cond [(char<=? #\0 c #\9) (- (char->integer c) 48)]
          [(char<=? #\a c #\f) (- (char->integer c) 87)]
          [(char<=? #\A c #\F) (- (char->integer c) 55)]
          [else #f]))
  (let loop ([i 0] [pos 0])
    (cond
      [(= i len) (bytes->string/utf-8 out #\uFFFD 0 pos)]
      [else
       (define c (string-ref str i))
       (cond [(eqv? c #\%)
              (define hex-end (min len (+ i 3)))
              (define hi (and (< (+ i 1) len) (hex-value (string-ref str (+ i 1)))))
              (define lo (and (< (+ i 2) len) (hex-value (string-ref str (+ i 2)))))
              (define hex-n
                (if (and hi lo)
                    (+ (* 16 hi) lo)
                    ;; Rare cases, such as "%+1" or a "%" near the end, which
                    ;; `string->number' accepts or rejects
                    (and (< (+ i 1) len)
                         (string->number (substring str (+ i 1) hex-end) 16))))
              (cond [(exact-nonnegative-integer? hex-n) ;; not negative, fractional
                     ;; Note: write as byte to support multi-byte Unicode chars
                     (bytes-set! out pos hex-n)
                     (loop hex-end (add1 pos))]
                    [else
                     ;; Pass through failed %-escapes as-is, for compatibility with
                     ;; previous version of code.
                     (bytes-set! out pos 37) ;; #\%
                     (let pass ([j (+ i 1)] [pos (add1 pos)])
                       (if (= j hex-end)
                           (loop hex-end pos)
                           (pass (add1 j) (write-char! (string-ref str j) pos))))])]
             [(< (char->integer c) ascii-size)
              (bytes-set! out pos (vector-ref table (char->integer c)))
              (loop (add1 i) (add1 pos))]
             [else
              ;; This should probably error, but strings to be decoded might
              ;; come from misbehaving sources; maybe it's better to add some
              ;; parameter for a permissive mode; one source of such bad URLs
              ;; is user-defined strings where the string is entered directly
              ;; and not properly encoded -- similar justification to
              ;; browsers accepting unencoded chars in manually entered URLs.
              (loop (add1 i) (write-char! c pos))])])))

;; Utility for defining codecs
(define-syntax-rule (define-codecs [encoder decoder mapping] ...)
  (begin (define-values [encoder decoder]
           (let-values ([(v:en v:de) (make-codec-tables mapping)])
             (define (encoder str) (encode v:en str))
             (define (decoder str) (decode v:de str))
             (values encoder decoder)))
         ...))

;; All of these are string -> string
(define-codecs
  [uri-encode uri-decode uri-mapping]
  [uri-path-segment-encode uri-path-segment-decode uri-path-segment-mapping]
  [uri-userinfo-encode uri-userinfo-decode uri-userinfo-mapping]
  [uri-unreserved-encode uri-unreserved-decode unreserved-mapping]
  [uri-path-segment-unreserved-encode uri-path-segment-unreserved-decode
   uri-path-segment-unreserved-mapping]
  [form-urlencoded-encode form-urlencoded-decode form-urlencoded-mapping])

;; listof (cons string string) -> string
;; http://www.w3.org/TR/html401/appendix/notes.html#ampersands-in-uris
;; listof (cons symbol string) -> string
(define (alist->form-urlencoded args)
  (string-join
   (for/list ([arg (in-list args)])
     (define name  (form-urlencoded-encode (symbol->string (car arg))))
     (define value (and (cdr arg) (form-urlencoded-encode (cdr arg))))
     (if value (string-append name "=" value) name))
   (if (memq (current-alist-separator-mode) '(semi semi-or-amp)) ";" "&")))

;; string -> listof (cons string string)
;; http://www.w3.org/TR/html401/appendix/notes.html#ampersands-in-uris
(define (form-urlencoded->alist str)
  (define sep?
    (case (current-alist-separator-mode)
      [(semi) (lambda (c) (eqv? c #\;))]
      [(amp) (lambda (c) (eqv? c #\&))]
      [else (lambda (c) (or (eqv? c #\&) (eqv? c #\;)))]))
  (define len (string-length str))
  (define (keyval start end)
    ;; equal-pos = #f => no "=..." part
    (define equal-pos
      (for/first ([i (in-range start end)]
                  #:when (eqv? #\= (string-ref str i)))
        i))
    (cons (string->symbol (form-urlencoded-decode
                           (substring str start (or equal-pos end))))
          (and equal-pos (form-urlencoded-decode (substring str (add1 equal-pos) end)))))
  (if (equal? "" str) '()
      (let loop ([start 0] [i 0])
        (cond
          [(= i len) (list (keyval start i))]
          [(sep? (string-ref str i)) (cons (keyval start i) (loop (add1 i) (add1 i)))]
          [else (loop start (add1 i))]))))

(define current-alist-separator-mode
  (make-parameter 'amp-or-semi
    (lambda (s)
      (unless (memq s '(amp semi amp-or-semi semi-or-amp))
        (raise-type-error 'current-alist-separator-mode
                          "'amp, 'semi, 'amp-or-semi, or 'semi-or-amp"
                          s))
      s)))

;;; uri-codec.rkt ends here
