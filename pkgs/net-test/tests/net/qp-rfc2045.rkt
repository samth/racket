#lang racket
(require net/qp tests/eli-tester)

;; Test cases derived from:
;;
;; 1. RFC 2045 Section 6.7 ("Quoted-Printable Content-Transfer-Encoding")
;;    https://www.rfc-editor.org/rfc/rfc2045#section-6.7
;;
;; 2. mathiasbynens/quoted-printable test suite:
;;    https://github.com/mathiasbynens/quoted-printable/blob/master/tests/tests.js
;;
;; 3. ronomon/quoted-printable test suite:
;;    https://github.com/ronomon/quoted-printable
;;
;; Racket's qp-encode/qp-decode operate on byte strings.
;; qp-encode uses CRLF as the default line separator and wraps at 75 columns.
;; qp-decode handles both LF and CRLF soft line breaks.

(provide rfc2045-tests)
(module+ main (test do (rfc2045-tests)))
(define (rfc2045-tests)
  (test

   ;; ================================================================
   ;; RFC 2045 Rule #1: General 8-bit representation
   ;; Any octet may be represented as =XX where XX is uppercase hex.
   ;; ================================================================

   ;; The equals sign (=, 0x3D) MUST always be encoded
   (qp-decode #"=3D") => #"="
   (qp-decode #"foo=3Dbar") => #"foo=bar"

   ;; High bytes (128-255) must be encoded
   (qp-decode #"=80") => (bytes #x80)
   (qp-decode #"=FF") => (bytes #xFF)
   (qp-decode #"=C3=A9") => (bytes #xC3 #xA9) ; UTF-8 for 'e with acute'

   ;; Lowercase hex digits should also be accepted by decoders
   (qp-decode #"=3d") => #"="
   (qp-decode #"=c3=a9") => (bytes #xC3 #xA9)


   ;; ================================================================
   ;; RFC 2045 Rule #2: Literal representation
   ;; Printable ASCII (33-126) may represent themselves, except = (61).
   ;; ================================================================

   ;; Printable ASCII passes through encoding unchanged
   (qp-encode #"Hello") => #"Hello"
   (qp-encode #"abc123") => #"abc123"
   (qp-encode #"!\"#$%&'()*+,-./:;<>?@[\\]^_`{|}~")
   => #"!\"#$%&'()*+,-./:;<>?@[\\]^_`{|}~"

   ;; Decode of literal printable ASCII
   (qp-decode #"Hello") => #"Hello"
   (qp-decode #"The quick brown fox") => #"The quick brown fox"


   ;; ================================================================
   ;; RFC 2045 Rule #3: Whitespace
   ;; Space (32) and tab (9) may represent themselves EXCEPT at end of line.
   ;; Trailing whitespace must be encoded as =20 or =09.
   ;; ================================================================

   ;; Spaces within text are preserved
   (qp-encode #"hello world") => #"hello world"
   (qp-decode #"hello world") => #"hello world"

   ;; Tabs within text are preserved
   (qp-encode #"hello\tworld") => #"hello\tworld"
   (qp-decode #"hello\tworld") => #"hello\tworld"


   ;; ================================================================
   ;; RFC 2045 Rule #5: Soft Line Breaks
   ;; Lines must be no more than 76 characters. An = at end of line
   ;; indicates a soft break (not present in decoded output).
   ;; ================================================================

   ;; Soft line break with CRLF
   (qp-decode #"hello =\r\nworld") => #"hello world"

   ;; Soft line break with bare LF (common in practice)
   (qp-decode #"hello =\nworld") => #"hello world"

   ;; RFC example: "Now's the time for all folk to come..."
   ;; (from RFC 2045 Section 6.7)
   (qp-decode #"Now's the time =\r\nfor all folk to come=\r\n to the aid of their country.")
   => #"Now's the time for all folk to come to the aid of their country."


   ;; ================================================================
   ;; Encoding of the equals sign
   ;; (from mathiasbynens/quoted-printable test suite)
   ;; ================================================================

   ;; "truth=beauty" example from RFC
   (qp-decode #"truth=3Dbeauty") => #"truth=beauty"


   ;; ================================================================
   ;; Encoding/Decoding of 8-bit data (non-ASCII bytes)
   ;; ================================================================

   ;; UTF-8 encoded international text
   ;; "Iñtërnâtiônàlizætiøn" in UTF-8
   (qp-decode #"I=C3=B1t=C3=ABrn=C3=A2ti=C3=B4n=C3=A0liz=C3=A6ti=C3=B8n")
   => (string->bytes/utf-8 "I\u00F1t\u00EBrn\u00E2ti\u00F4n\u00E0liz\u00E6ti\u00F8n")

   ;; Single non-ASCII bytes
   (qp-decode #"=00") => (bytes 0)
   (qp-decode #"=01") => (bytes 1)
   (qp-decode #"=7F") => (bytes 127)
   (qp-decode #"=80") => (bytes 128)
   (qp-decode #"=FE") => (bytes 254)
   (qp-decode #"=FF") => (bytes 255)


   ;; ================================================================
   ;; Round-trip tests
   ;; ================================================================

   ;; Simple ASCII
   (qp-decode (qp-encode #"Hello, World!")) => #"Hello, World!"

   ;; Empty input
   (qp-encode #"") => #""
   (qp-decode #"") => #""

   ;; All printable ASCII characters (0x21-0x7E except 0x3D)
   do (for ([b (in-range 33 127)])
        (unless (= b 61) ; skip =
          (let ([bstr (bytes b)])
            (test (qp-decode (qp-encode bstr)) => bstr))))

   ;; All byte values 0-255 round-trip
   do (for ([b (in-range 256)])
        (let ([bstr (bytes b)])
          (test (qp-decode (qp-encode bstr)) => bstr)))


   ;; ================================================================
   ;; Edge cases from mathiasbynens/quoted-printable
   ;; ================================================================

   ;; Trailing = (incomplete sequence at end of input)
   ;; Racket's decoder outputs "=" for a trailing =
   (qp-decode #"foo=") => #"foo="

   ;; Encoded control characters
   (qp-decode #"=0D=0A") => #"\r\n"
   (qp-decode #"=0D") => #"\r"
   (qp-decode #"=0A") => #"\n"
   (qp-decode #"=09") => #"\t"
   (qp-decode #"=20") => #" "


   ;; ================================================================
   ;; Long line encoding (soft line break insertion)
   ;; Racket's encoder wraps at column 75 with a soft break.
   ;; ================================================================

   ;; 75 'x' characters should trigger a soft line break
   do (let ([input (make-bytes 75 (char->integer #\x))])
        (let ([encoded (qp-encode input)])
          ;; encoded should contain a soft line break
          (test (regexp-match? #rx#"=\r\n" encoded) => #t)
          ;; round-trip
          (test (qp-decode encoded) => input)))

   ;; Short string should NOT have a soft line break
   do (let ([input #"short"])
        (test (regexp-match? #rx#"=\r\n" (qp-encode input)) => #f))


   ;; ================================================================
   ;; Encoding of bytes that must be escaped
   ;; ================================================================

   ;; Null byte
   (qp-encode (bytes 0)) => #"=00"
   ;; DEL
   (qp-encode (bytes 127)) => #"=7F"
   ;; High bytes
   (qp-encode (bytes 128)) => #"=80"
   (qp-encode (bytes 255)) => #"=FF"
   ;; Equals sign
   (qp-encode #"=") => #"=3D"

   ))

(module+ test (require (submod ".." main))) ; for raco test & drdr
