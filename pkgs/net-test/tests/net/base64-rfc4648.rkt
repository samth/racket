#lang racket
(require net/base64 tests/eli-tester)

;; Test cases derived from RFC 4648 Section 10 ("Test Vectors"):
;; https://datatracker.ietf.org/doc/html/rfc4648#section-10
;;
;; Additional test vectors from:
;; - RFC 4648 Section 9 (illustrations)
;; - Common base64 test suites (Node.js, Go, Python standard libraries)
;;
;; Racket's base64-encode appends a line separator (default "\r\n") and wraps
;; at 72 characters. The decode tests use raw base64 without line separators,
;; while encode tests account for the trailing separator.

;; Helper: encode with newline separator for concise tests
(define (b64enc bstr)
  (base64-encode bstr #"\n"))

;; Helper: decode (ignores whitespace/line separators in input)
(define (b64dec bstr)
  (base64-decode bstr))

(provide rfc4648-tests)
(module+ main (test do (rfc4648-tests)))
(define (rfc4648-tests)
  (test

   ;; ================================================================
   ;; RFC 4648 Section 10 - Official Test Vectors
   ;; https://datatracker.ietf.org/doc/html/rfc4648#section-10
   ;; ================================================================

   ;; Empty input
   (b64enc #"") => #""
   (b64dec #"") => #""

   ;; "f" -> "Zg=="
   (b64enc #"f") => #"Zg==\n"
   (b64dec #"Zg==") => #"f"

   ;; "fo" -> "Zm8="
   (b64enc #"fo") => #"Zm8=\n"
   (b64dec #"Zm8=") => #"fo"

   ;; "foo" -> "Zm9v"
   (b64enc #"foo") => #"Zm9v\n"
   (b64dec #"Zm9v") => #"foo"

   ;; "foob" -> "Zm9vYg=="
   (b64enc #"foob") => #"Zm9vYg==\n"
   (b64dec #"Zm9vYg==") => #"foob"

   ;; "fooba" -> "Zm9vYmE="
   (b64enc #"fooba") => #"Zm9vYmE=\n"
   (b64dec #"Zm9vYmE=") => #"fooba"

   ;; "foobar" -> "Zm9vYmFy"
   (b64enc #"foobar") => #"Zm9vYmFy\n"
   (b64dec #"Zm9vYmFy") => #"foobar"


   ;; ================================================================
   ;; Classic test vectors used across multiple implementations
   ;; (Node.js buffer tests, Python base64 module, Go encoding/base64)
   ;; ================================================================

   ;; Single characters
   (b64dec #"YQ==") => #"a"
   (b64dec #"YWI=") => #"ab"
   (b64dec #"YWJj") => #"abc"
   (b64dec #"YWJjZA==") => #"abcd"
   (b64dec #"YWJjZGU=") => #"abcde"
   (b64dec #"YWJjZGVm") => #"abcdef"

   ;; The classic "Man" example (from RFC 4648 Section 9 illustration)
   ;; "Man" demonstrates the 3-byte to 4-character mapping:
   ;; M=77(01001101) a=97(01100001) n=110(01101110) -> TWFu
   (b64enc #"Man") => #"TWFu\n"
   (b64dec #"TWFu") => #"Man"

   ;; Padding examples from the RFC's illustration:
   ;; "Ma" (2 bytes, one = padding)
   (b64enc #"Ma") => #"TWE=\n"
   (b64dec #"TWE=") => #"Ma"

   ;; "M" (1 byte, two = padding)
   (b64enc #"M") => #"TQ==\n"
   (b64dec #"TQ==") => #"M"

   ;; Common phrases used in test suites
   (b64dec #"SGVsbG8gV29ybGQ=") => #"Hello World"
   (b64dec #"SGVsbG8sIFdvcmxkIQ==") => #"Hello, World!"
   (b64dec #"VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw==")
   => #"The quick brown fox jumps over the lazy dog"

   ;; Encoding of "Hello World" etc.
   (b64enc #"Hello World") => #"SGVsbG8gV29ybGQ=\n"
   (b64enc #"Hello, World!") => #"SGVsbG8sIFdvcmxkIQ==\n"

   ;; ================================================================
   ;; Binary data test vectors
   ;; ================================================================

   ;; Single bytes (boundary values)
   (b64enc (bytes 0)) => #"AA==\n"
   (b64dec #"AA==") => (bytes 0)

   (b64enc (bytes 255)) => #"/w==\n"
   (b64dec #"/w==") => (bytes 255)

   (b64enc (bytes 127)) => #"fw==\n"
   (b64dec #"fw==") => (bytes 127)

   (b64enc (bytes 128)) => #"gA==\n"
   (b64dec #"gA==") => (bytes 128)

   ;; Two bytes
   (b64enc (bytes 0 0)) => #"AAA=\n"
   (b64dec #"AAA=") => (bytes 0 0)

   (b64enc (bytes 255 255)) => #"//8=\n"
   (b64dec #"//8=") => (bytes 255 255)

   ;; Three bytes (no padding needed)
   (b64enc (bytes 0 0 0)) => #"AAAA\n"
   (b64dec #"AAAA") => (bytes 0 0 0)

   (b64enc (bytes 255 255 255)) => #"////\n"
   (b64dec #"////") => (bytes 255 255 255)

   ;; All base64 alphabet characters in output
   ;; bytes 0-63 encoded
   (b64dec #"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
   => (bytes 0 16 131 16 81 135 32 146 139 48 211 143 65 20 147 81 85 151
             97 150 155 113 215 159 130 24 163 146 89 167 162 154 171 178
             219 175 195 28 179 211 93 183 227 158 187 243 223 191)


   ;; ================================================================
   ;; Decoder robustness: whitespace handling
   ;; (RFC 4648 Section 3.3: implementations MUST reject non-alphabet
   ;; characters or handle whitespace in line-oriented implementations)
   ;; Racket's decoder skips non-alphabet characters.
   ;; ================================================================

   ;; Decode with embedded newlines (as produced by encoders with line wrapping)
   (b64dec #"Zm9v\r\nYmFy\r\n") => #"foobar"
   (b64dec #"Zm9v\nYmFy\n") => #"foobar"

   ;; Decode with spaces (some implementations produce these)
   (b64dec #"Zm9v YmFy") => #"foobar"


   ;; ================================================================
   ;; Line wrapping tests
   ;; Racket wraps at 72 base64 characters per line.
   ;; 72 base64 chars = 54 input bytes per line
   ;; ================================================================

   ;; Exactly 54 bytes: one full line, no wrapping
   (b64enc (make-bytes 54 65))
   => #"QUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFB\n"

   ;; 55 bytes: wraps to second line
   (b64enc (make-bytes 55 65))
   => #"QUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFB\nQQ==\n"

   ;; 57 bytes: wraps to second line (54+3)
   (b64enc (make-bytes 57 65))
   => #"QUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFB\nQUFB\n"


   ;; ================================================================
   ;; Round-trip tests for various lengths
   ;; (Verifies that encode(decode(x)) and decode(encode(x)) are identity)
   ;; ================================================================

   ;; All lengths 0 through 20
   do (for ([len (in-range 21)])
        (define input (make-bytes len (+ 65 (modulo len 26))))
        (test (b64dec (b64enc input)) => input))

   ;; Round-trip of all 256 byte values
   do (let ([all-bytes (list->bytes (for/list ([i (in-range 256)]) i))])
        (test (b64dec (b64enc all-bytes)) => all-bytes))

   ))

(module+ test (require (submod ".." main))) ; for raco test & drdr
