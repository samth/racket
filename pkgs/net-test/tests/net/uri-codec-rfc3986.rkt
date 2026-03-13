#lang racket
(require net/uri-codec tests/eli-tester)

;; Test cases derived from:
;;
;; 1. RFC 3986 ("Uniform Resource Identifier: Generic Syntax")
;;    https://www.rfc-editor.org/rfc/rfc3986
;;    Section 2.1 (Percent-Encoding), Section 2.2 (Reserved Characters),
;;    Section 2.3 (Unreserved Characters)
;;
;; 2. WPT (Web Platform Tests) percent-encoding test data:
;;    https://github.com/web-platform-tests/wpt/blob/master/url/resources/percent-encoding.json
;;    (UTF-8 subset only, as Racket's uri-codec operates on UTF-8)
;;
;; 3. WHATWG URL Standard application/x-www-form-urlencoded test cases:
;;    https://url.spec.whatwg.org/#urlencoded-parsing
;;
;; 4. Common test vectors from Node.js querystring, Python urllib, Go net/url

(provide rfc3986-tests)
(module+ main (test do (rfc3986-tests)))
(define (rfc3986-tests)
  (test

   ;; ================================================================
   ;; RFC 3986 Section 2.3 - Unreserved Characters
   ;; unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~"
   ;; These SHOULD NOT be percent-encoded.
   ;; ================================================================

   ;; Uppercase letters are unreserved
   (uri-encode "ABCDEFGHIJKLMNOPQRSTUVWXYZ") => "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   ;; Lowercase letters are unreserved
   (uri-encode "abcdefghijklmnopqrstuvwxyz") => "abcdefghijklmnopqrstuvwxyz"
   ;; Digits are unreserved
   (uri-encode "0123456789") => "0123456789"
   ;; Special unreserved characters
   (uri-encode "-._~") => "-._~"

   ;; Round-trip for unreserved
   (uri-decode (uri-encode "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~"))
   => "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~"


   ;; ================================================================
   ;; RFC 3986 Section 2.2 - Reserved Characters
   ;; reserved = gen-delims / sub-delims
   ;; gen-delims = ":" / "/" / "?" / "#" / "[" / "]" / "@"
   ;; sub-delims = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="
   ;;
   ;; Racket's uri-encode preserves sub-delims that are in the unreserved
   ;; set for URI components (!  '  (  )  *) and percent-encodes the rest.
   ;; ================================================================

   ;; Gen-delims should be encoded by uri-encode
   (uri-encode ":") => "%3A"
   (uri-encode "/") => "%2F"
   (uri-encode "?") => "%3F"
   (uri-encode "#") => "%23"
   (uri-encode "[") => "%5B"
   (uri-encode "]") => "%5D"
   (uri-encode "@") => "%40"

   ;; Sub-delims: some are preserved, some are encoded
   ;; (Racket's uri-encode keeps ! ' ( ) * as unreserved per RFC 3986)
   (uri-encode "!") => "!"
   (uri-encode "'") => "'"
   (uri-encode "(") => "("
   (uri-encode ")") => ")"
   (uri-encode "*") => "*"
   ;; These sub-delims are encoded
   (uri-encode "$") => "%24"
   (uri-encode "&") => "%26"
   (uri-encode "+") => "%2B"
   (uri-encode ",") => "%2C"
   (uri-encode ";") => "%3B"
   (uri-encode "=") => "%3D"

   ;; Decoding of all reserved characters
   (uri-decode "%3A") => ":"
   (uri-decode "%2F") => "/"
   (uri-decode "%3F") => "?"
   (uri-decode "%23") => "#"
   (uri-decode "%5B") => "["
   (uri-decode "%5D") => "]"
   (uri-decode "%40") => "@"
   (uri-decode "%24") => "$"
   (uri-decode "%26") => "&"
   (uri-decode "%2B") => "+"
   (uri-decode "%2C") => ","
   (uri-decode "%3B") => ";"
   (uri-decode "%3D") => "="


   ;; ================================================================
   ;; RFC 3986 Section 2.1 - Percent Encoding of non-ASCII (UTF-8)
   ;; UTF-8 octets are each percent-encoded individually.
   ;; ================================================================

   ;; 2-byte UTF-8 sequences
   ;; U+00A2 CENT SIGN: C2 A2
   (uri-encode "\u00A2") => "%C2%A2"
   (uri-decode "%C2%A2") => "\u00A2"

   ;; U+00E9 LATIN SMALL LETTER E WITH ACUTE: C3 A9
   (uri-encode "\u00E9") => "%C3%A9"
   (uri-decode "%C3%A9") => "\u00E9"

   ;; U+00F1 LATIN SMALL LETTER N WITH TILDE: C3 B1
   (uri-encode "\u00F1") => "%C3%B1"

   ;; 3-byte UTF-8 sequences
   ;; U+20AC EURO SIGN: E2 82 AC
   (uri-encode "\u20AC") => "%E2%82%AC"
   (uri-decode "%E2%82%AC") => "\u20AC"

   ;; U+2020 DAGGER: E2 80 A0
   ;; (from WPT percent-encoding.json, UTF-8 entry)
   (uri-encode "\u2020") => "%E2%80%A0"
   (uri-decode "%E2%80%A0") => "\u2020"

   ;; U+2212 MINUS SIGN: E2 88 92
   ;; (from WPT percent-encoding.json)
   (uri-encode "\u2212") => "%E2%88%92"
   (uri-decode "%E2%88%92") => "\u2212"

   ;; 4-byte UTF-8 sequences
   ;; U+10348 GOTHIC LETTER HWAIR: F0 90 8D 88
   (uri-encode "\U10348") => "%F0%90%8D%88"
   (uri-decode "%F0%90%8D%88") => "\U10348"

   ;; U+1F600 GRINNING FACE: F0 9F 98 80
   (uri-encode "\U1F600") => "%F0%9F%98%80"
   (uri-decode "%F0%9F%98%80") => "\U1F600"

   ;; Mixed ASCII and non-ASCII
   (uri-encode "caf\u00E9") => "caf%C3%A9"
   (uri-decode "caf%C3%A9") => "caf\u00E9"

   ;; CJK character: U+7238 (Chinese character)
   (uri-encode "\u7238") => "%E7%88%B8"
   (uri-decode "%E7%88%B8") => "\u7238"

   ;; Japanese: U+65E5 (day/sun)
   (uri-encode "\u65E5") => "%E6%97%A5"
   (uri-decode "%E6%97%A5") => "\u65E5"


   ;; ================================================================
   ;; Percent-encoding case insensitivity in decoding
   ;; RFC 3986 Section 2.1: "The uppercase hexadecimal digits 'A' through
   ;; 'F' are equivalent to the lowercase digits 'a' through 'f'"
   ;; ================================================================

   (uri-decode "%2f") => "/"
   (uri-decode "%2F") => "/"
   (uri-decode "%c3%a9") => "\u00E9"
   (uri-decode "%C3%A9") => "\u00E9"
   (uri-decode "%c3%A9") => "\u00E9"


   ;; ================================================================
   ;; Control characters and space
   ;; ================================================================

   ;; Space (0x20) is encoded
   (uri-encode " ") => "%20"
   (uri-decode "%20") => " "

   ;; Tab (0x09)
   (uri-encode "\t") => "%09"
   (uri-decode "%09") => "\t"

   ;; Null (0x00)
   (uri-encode "\0") => "%00"
   (uri-decode "%00") => "\0"

   ;; Newline (0x0A)
   (uri-encode "\n") => "%0A"
   (uri-decode "%0A") => "\n"

   ;; Carriage return (0x0D)
   (uri-encode "\r") => "%0D"
   (uri-decode "%0D") => "\r"

   ;; DEL (0x7F)
   (uri-encode "\177") => "%7F"
   (uri-decode "%7F") => "\177"


   ;; ================================================================
   ;; URI component-specific encoding (RFC 3986 Sections 3.x)
   ;; Racket provides specialized encoders for different URI components
   ;; ================================================================

   ;; Path segment encoding: allows @ and : but encodes / and ?
   (uri-path-segment-encode "foo/bar") => "foo%2Fbar"
   (uri-path-segment-encode "foo@bar") => "foo@bar"
   (uri-path-segment-encode "foo:bar") => "foo:bar"

   ;; Userinfo encoding: allows : but encodes @
   (uri-userinfo-encode "user:pass") => "user:pass"
   (uri-userinfo-encode "user@host") => "user%40host"

   ;; Round-trips for component encoders
   (uri-path-segment-decode (uri-path-segment-encode "hello world/foo@bar"))
   => "hello world/foo@bar"
   (uri-userinfo-decode (uri-userinfo-encode "user:p@ss w0rd"))
   => "user:p@ss w0rd"


   ;; ================================================================
   ;; application/x-www-form-urlencoded
   ;; (WHATWG URL Standard Section 5)
   ;; https://url.spec.whatwg.org/#urlencoded-parsing
   ;; ================================================================

   ;; Basic key=value parsing
   (form-urlencoded->alist "a=1&b=2") => '([a . "1"] [b . "2"])
   (form-urlencoded->alist "a=1&b=2&c=3") => '([a . "1"] [b . "2"] [c . "3"])

   ;; Plus sign decodes as space
   (form-urlencoded->alist "q=hello+world") => '([q . "hello world"])
   (alist->form-urlencoded '([q . "hello world"])) => "q=hello+world"

   ;; Percent-encoded values
   (form-urlencoded->alist "q=hello%20world") => '([q . "hello world"])

   ;; Empty values
   (form-urlencoded->alist "a=&b=") => '([a . ""] [b . ""])

   ;; Keys without values
   (form-urlencoded->alist "a&b") => '([a . #f] [b . #f])

   ;; Mixed
   (form-urlencoded->alist "a=1&b&c=") => '([a . "1"] [b . #f] [c . ""])

   ;; Empty string
   (form-urlencoded->alist "") => '()

   ;; Encoding special characters in form data
   (alist->form-urlencoded '([name . "John Doe"])) => "name=John+Doe"
   (alist->form-urlencoded '([email . "user@example.com"]))
   => "email=user%40example.com"
   (alist->form-urlencoded '([formula . "a=b&c=d"]))
   => "formula=a%3Db%26c%3Dd"

   ;; Non-ASCII in form data
   (form-urlencoded->alist
    (alist->form-urlencoded '([name . "caf\u00E9"])))
   => '([name . "caf\u00E9"])

   ;; Multiple values for same key (not merged, each appears separately)
   (form-urlencoded->alist "a=1&a=2") => '([a . "1"] [a . "2"])

   ;; Semicolon as separator (supported in amp-or-semi and semi modes)
   (form-urlencoded->alist "a=1;b=2") => '([a . "1"] [b . "2"])


   ;; ================================================================
   ;; Decoder robustness: malformed percent sequences
   ;; (Racket passes through invalid percent sequences unchanged)
   ;; ================================================================

   (uri-decode "%") => "%"
   (uri-decode "%P") => "%P"
   (uri-decode "%Pq") => "%Pq"
   (uri-decode "%G1") => "%G1"
   (uri-decode "100%") => "100%"
   (uri-decode "100%pure") => "100%pure"


   ;; ================================================================
   ;; Round-trip tests with various character sets
   ;; ================================================================

   ;; Latin-1 supplement
   (uri-decode (uri-encode "\u00E0\u00E1\u00E2\u00E3\u00E4\u00E5"))
   => "\u00E0\u00E1\u00E2\u00E3\u00E4\u00E5"

   ;; Greek
   (uri-decode (uri-encode "\u03B1\u03B2\u03B3\u03B4"))
   => "\u03B1\u03B2\u03B3\u03B4"

   ;; Cyrillic
   (uri-decode (uri-encode "\u0410\u0411\u0412\u0413"))
   => "\u0410\u0411\u0412\u0413"

   ;; Arabic
   (uri-decode (uri-encode "\u0627\u0628\u062A\u062B"))
   => "\u0627\u0628\u062A\u062B"

   ;; Korean
   (uri-decode (uri-encode "\uAC00\uAC01"))
   => "\uAC00\uAC01"

   ;; Emoji (4-byte UTF-8)
   (uri-decode (uri-encode "\U1F4A9\U1F600"))
   => "\U1F4A9\U1F600"

   ))

(module+ test (require (submod ".." main))) ; for raco test & drdr
