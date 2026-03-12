#lang racket
(require net/url-string tests/eli-tester)

;; Test cases derived from the Web Platform Tests (WPT) URL test suite:
;; https://github.com/web-platform-tests/wpt/blob/master/url/resources/urltestdata.json
;;
;; The WPT tests target the WHATWG URL Standard, while Racket's parser follows
;; RFC 3986. Test cases that depend on WHATWG-specific behavior are excluded:
;;   - Default port stripping (e.g., http port 80)
;;   - Special scheme single-slash authority inference (e.g., http:/example.com)
;;   - IDN/Punycode encoding
;;   - WHATWG-specific fragment percent-encoding normalization
;;   - WHATWG credential handling (username/password splitting)
;;
;; The included tests verify basic URL component decomposition that is
;; consistent between WHATWG and RFC 3986.

;; Helper: test that string->url produces expected components
(define (test-wpt-parse input expected-scheme expected-hostname expected-port
                       expected-hash expected-username expected-password)
  (define u (string->url input))
  (define scheme (url-scheme u))
  (define host (url-host u))
  (define port (url-port u))
  (define fragment (url-fragment u))
  (define expected-port-num
    (if (equal? expected-port "") #f (string->number expected-port)))
  (define expected-frag
    (if (equal? expected-hash "")
        ""
        (substring expected-hash 1)))
  (define expected-user
    (cond
      [(and (equal? expected-username "") (equal? expected-password "")) #f]
      [(equal? expected-password "") expected-username]
      [else (string-append expected-username ":" expected-password)]))
  (test
   scheme => expected-scheme
   (or host "") => expected-hostname
   port => expected-port-num
   (or fragment "") => expected-frag
   (url-user u) => expected-user))

(provide wpt-tests)
(module+ main (test do (wpt-tests)))
(define (wpt-tests)
  ;; WPT URL parsing tests (RFC 3986 compatible subset)

  ;; # Based on http://trac.webkit.org/browser/trunk/LayoutTests/fast/url/script-tests/path.js
  (test-wpt-parse "http://example.com/././foo" "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/./.foo" "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/foo/." "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/foo/./" "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/foo/bar/.." "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/foo/bar/../" "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/foo/..bar" "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/foo/bar/../ton" "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/foo/bar/../ton/../../a" "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/foo/../../.." "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/foo/../../../ton" "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/foo/%2e" "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/foo/%2e%2" "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/foo/%2e./%2e%2e/.%2e/%2e.bar" "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com////../.." "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/foo/bar//../.." "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/foo/bar//.." "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/foo" "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/%20foo" "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/foo%" "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/foo%2" "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/foo%2zbar" "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/foo%2Â©zbar" "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/foo%41%7a" "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/foo%00%51" "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/(%28:%3A%29)" "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/%3A%3a%3C%3c" "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/%7Ffp3%3Eju%3Dduvgw%3Dd" "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/@asdf%40" "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/你好你好" "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/‥/foo" "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/﻿/foo" "http" "example.com" "" "" "" "")
  (test-wpt-parse "http://example.com/‮/foo/‭/bar" "http" "example.com" "" "" "" "")

  ;; # Based on http://trac.webkit.org/browser/trunk/LayoutTests/fast/url/script-tests/relative.js
  (test-wpt-parse "http://www.google.com/foo?bar=baz#" "http" "www.google.com" "" "" "" "")
  (test-wpt-parse "http://www.google.com" "http" "www.google.com" "" "" "" "")
  (test-wpt-parse "http://www/foo%2Ehtml" "http" "www" "" "" "" "")
  (test-wpt-parse "http://www/foo/%2E/html" "http" "www" "" "" "" "")
  (test-wpt-parse "http://foo:81/" "http" "foo" "81" "" "" "")
  (test-wpt-parse "httpa://foo:80/" "httpa" "foo" "80" "" "" "")
  (test-wpt-parse "https://foo:80/" "https" "foo" "80" "" "" "")
  (test-wpt-parse "ftp://foo:80/" "ftp" "foo" "80" "" "" "")
  (test-wpt-parse "gopher://foo:70/" "gopher" "foo" "70" "" "" "")
  (test-wpt-parse "gopher://foo:443/" "gopher" "foo" "443" "" "" "")
  (test-wpt-parse "ws://foo:81/" "ws" "foo" "81" "" "" "")
  (test-wpt-parse "ws://foo:443/" "ws" "foo" "443" "" "" "")
  (test-wpt-parse "ws://foo:815/" "ws" "foo" "815" "" "" "")
  (test-wpt-parse "wss://foo:80/" "wss" "foo" "80" "" "" "")
  (test-wpt-parse "wss://foo:81/" "wss" "foo" "81" "" "" "")
  (test-wpt-parse "wss://foo:815/" "wss" "foo" "815" "" "" "")
  (test-wpt-parse "madeupscheme:/example.com/" "madeupscheme" "" "" "" "" "")
  (test-wpt-parse "ftps:/example.com/" "ftps" "" "" "" "" "")
  (test-wpt-parse "gopher:/example.com/" "gopher" "" "" "" "" "")
  (test-wpt-parse "data:/example.com/" "data" "" "" "" "" "")
  (test-wpt-parse "javascript:/example.com/" "javascript" "" "" "" "" "")
  (test-wpt-parse "mailto:/example.com/" "mailto" "" "" "" "" "")
  (test-wpt-parse "madeupscheme:example.com/" "madeupscheme" "" "" "" "" "")
  (test-wpt-parse "ftps:example.com/" "ftps" "" "" "" "" "")
  (test-wpt-parse "gopher:example.com/" "gopher" "" "" "" "" "")
  (test-wpt-parse "data:example.com/" "data" "" "" "" "" "")
  (test-wpt-parse "javascript:example.com/" "javascript" "" "" "" "" "")
  (test-wpt-parse "mailto:example.com/" "mailto" "" "" "" "" "")
  (test-wpt-parse "https://example.com/aaa/bbb/%2e%2e?query" "https" "example.com" "" "" "" "")

  ;; # Based on http://trac.webkit.org/browser/trunk/LayoutTests/fast/url/segments-userinfo-vs-host.html
  (test-wpt-parse "http://a:b@www.example.com" "http" "www.example.com" "" "" "a" "b")
  (test-wpt-parse "http://:b@www.example.com" "http" "www.example.com" "" "" "" "b")
  (test-wpt-parse "http://www.@pple.com" "http" "pple.com" "" "" "www." "")

  ;; Leading and trailing C0 control or space
  (test-wpt-parse "non-special:opaque  ?hi" "non-special" "" "" "" "" "")
  (test-wpt-parse "non-special:opaque  #hi" "non-special" "" "" "#hi" "" "")
  (test-wpt-parse "non-special:opaque  x?hi" "non-special" "" "" "" "" "")
  (test-wpt-parse "non-special:opaque  x#hi" "non-special" "" "" "#hi" "" "")

  ;; Domains with empty labels
  (test-wpt-parse "http://./" "http" "." "" "" "" "")
  (test-wpt-parse "http://../" "http" ".." "" "" "" "")

  ;; Non-special domains with empty labels
  (test-wpt-parse "h://." "h" "." "" "" "" "")

  ;; byte is ' and url is special
  (test-wpt-parse "http://host/?'" "http" "host" "" "" "" "")
  (test-wpt-parse "notspecial://host/?'" "notspecial" "host" "" "" "" "")

  ;; # make sure that relative URL logic works on known typically non-relative schemes too
  (test-wpt-parse "about:/../" "about" "" "" "" "" "")
  (test-wpt-parse "data:/../" "data" "" "" "" "" "")
  (test-wpt-parse "javascript:/../" "javascript" "" "" "" "" "")
  (test-wpt-parse "mailto:/../" "mailto" "" "" "" "" "")

  ;; # unknown schemes and their hosts
  (test-wpt-parse "sc://%/" "sc" "%" "" "" "" "")

  ;; # unknown scheme with path looking like a password
  (test-wpt-parse "sc::a@example.net" "sc" "" "" "" "" "")

  ;; # unknown scheme with bogus percent-encoding
  (test-wpt-parse "wow:%NBD" "wow" "" "" "" "" "")
  (test-wpt-parse "wow:%1G" "wow" "" "" "" "" "")

  ;; # unknown scheme with non-URL characters
  (test-wpt-parse "wow:￿" "wow" "" "" "" "" "")

  ;; # tests from jsdom/whatwg-url designed for code coverage
  (test-wpt-parse "http://127.0.0.1:10100/relative_import.html" "http" "127.0.0.1" "10100" "" "" "")
  (test-wpt-parse "http://facebook.com/?foo=%7B%22abc%22" "http" "facebook.com" "" "" "" "")
  (test-wpt-parse "https://localhost:3000/jqueryui@1.2.3" "https" "localhost" "3000" "" "" "")

  ;; # Non-special-URL path tests
  (test-wpt-parse "sc://?" "sc" "" "" "" "" "")
  (test-wpt-parse "sc://#" "sc" "" "" "" "" "")
  (test-wpt-parse "tftp://foobar.com/someconfig;mode=netascii" "tftp" "foobar.com" "" "" "" "")
  (test-wpt-parse "telnet://user:pass@foobar.com:23/" "telnet" "foobar.com" "23" "" "user" "pass")
  (test-wpt-parse "ut2004://10.10.10.10:7777/Index.ut2" "ut2004" "10.10.10.10" "7777" "" "" "")
  (test-wpt-parse "redis://foo:bar@somehost:6379/0?baz=bam&qux=baz" "redis" "somehost" "6379" "" "foo" "bar")
  (test-wpt-parse "rsync://foo@host:911/sup" "rsync" "host" "911" "" "foo" "")
  (test-wpt-parse "git://github.com/foo/bar.git" "git" "github.com" "" "" "" "")
  (test-wpt-parse "irc://myserver.com:6999/channel?passwd" "irc" "myserver.com" "6999" "" "" "")
  (test-wpt-parse "dns://fw.example.org:9999/foo.bar.org?type=TXT" "dns" "fw.example.org" "9999" "" "" "")
  (test-wpt-parse "ldap://localhost:389/ou=People,o=JNDITutorial" "ldap" "localhost" "389" "" "" "")
  (test-wpt-parse "git+https://github.com/foo/bar" "git+https" "github.com" "" "" "" "")
  (test-wpt-parse "urn:ietf:rfc:2648" "urn" "" "" "" "" "")
  (test-wpt-parse "tag:joe@example.org,2001:foo/bar" "tag" "" "" "" "" "")

  ;; Serialize /. in path
  (test-wpt-parse "non-spec:/.//" "non-spec" "" "" "" "" "")
  (test-wpt-parse "non-spec:/..//" "non-spec" "" "" "" "" "")
  (test-wpt-parse "non-spec:/a/..//" "non-spec" "" "" "" "" "")
  (test-wpt-parse "non-spec:/.//path" "non-spec" "" "" "" "" "")
  (test-wpt-parse "non-spec:/..//path" "non-spec" "" "" "" "" "")
  (test-wpt-parse "non-spec:/a/..//path" "non-spec" "" "" "" "" "")

  ;; # IPv6 in non-special-URLs
  (test-wpt-parse "blob:https://example.com:443/" "blob" "" "" "" "" "")
  (test-wpt-parse "blob:http://example.org:88/" "blob" "" "" "" "" "")
  (test-wpt-parse "blob:d3958f5c-0777-0845-9dcf-2cb28783acaf" "blob" "" "" "" "" "")
  (test-wpt-parse "blob:" "blob" "" "" "" "" "")

  ;; blob: in blob:
  (test-wpt-parse "blob:blob:" "blob" "" "" "" "" "")
  (test-wpt-parse "blob:blob:https://example.org/" "blob" "" "" "" "" "")

  ;; Non-http(s): in blob:
  (test-wpt-parse "blob:about:blank" "blob" "" "" "" "" "")
  (test-wpt-parse "blob:file://host/path" "blob" "" "" "" "" "")
  (test-wpt-parse "blob:ftp://host/path" "blob" "" "" "" "" "")
  (test-wpt-parse "blob:ws://example.org/" "blob" "" "" "" "" "")
  (test-wpt-parse "blob:wss://example.org/" "blob" "" "" "" "" "")

  ;; Percent-encoded http: in blob:
  (test-wpt-parse "blob:http%3a//example.org/" "blob" "" "" "" "" "")

  ;; Invalid IPv4 radix digits
  (test-wpt-parse "http://0x7f.0.0.0x7g" "http" "0x7f.0.0.0x7g" "" "" "" "")
  (test-wpt-parse "http://0X7F.0.0.0X7G" "http" "0x7f.0.0.0x7g" "" "" "" "")

  ;; Percent-encoded query and fragment
  (test-wpt-parse "http://example.org/test?#" "http" "example.org" "" "" "" "")
  (test-wpt-parse "http://example.org/test?<" "http" "example.org" "" "" "" "")
  (test-wpt-parse "http://example.org/test?>" "http" "example.org" "" "" "" "")
  (test-wpt-parse "http://example.org/test?⌣" "http" "example.org" "" "" "" "")
  (test-wpt-parse "http://example.org/test?%23%23" "http" "example.org" "" "" "" "")
  (test-wpt-parse "http://example.org/test?%GH" "http" "example.org" "" "" "" "")
  (test-wpt-parse "http://example.org/test?a#%GH" "http" "example.org" "" "#%GH" "" "")

  ;; UTF-8 percent-encode of C0 control percent-encode set and supersets

  ;; Last component looks like a number, but not valid IPv4
  (test-wpt-parse "http://foo.09.." "http" "foo.09.." "" "" "" "")

  ;; U+0000 and U+FFFF in various places
  (test-wpt-parse "https://x/￿y" "https" "x" "" "" "" "")
  (test-wpt-parse "https://x/?￿y" "https" "x" "" "" "" "")
  (test-wpt-parse "non-special:￿y" "non-special" "" "" "" "" "")
  (test-wpt-parse "non-special:x/￿y" "non-special" "" "" "" "" "")
  (test-wpt-parse "non-special:x/?￿y" "non-special" "" "" "" "" "")

  ;; Non-special schemes that some implementations might incorrectly treat as special
  (test-wpt-parse "data://example.com:8080/pathname?search#hash" "data" "example.com" "8080" "#hash" "" "")
  (test-wpt-parse "data:///test" "data" "" "" "" "" "")
  (test-wpt-parse "data://test/a/../b" "data" "test" "" "" "" "")
  (test-wpt-parse "javascript://example.com:8080/pathname?search#hash" "javascript" "example.com" "8080" "#hash" "" "")
  (test-wpt-parse "javascript:///test" "javascript" "" "" "" "" "")
  (test-wpt-parse "javascript://test/a/../b" "javascript" "test" "" "" "" "")
  (test-wpt-parse "mailto://example.com:8080/pathname?search#hash" "mailto" "example.com" "8080" "#hash" "" "")
  (test-wpt-parse "mailto:///test" "mailto" "" "" "" "" "")
  (test-wpt-parse "mailto://test/a/../b" "mailto" "test" "" "" "" "")
  (test-wpt-parse "intent://example.com:8080/pathname?search#hash" "intent" "example.com" "8080" "#hash" "" "")
  (test-wpt-parse "intent:///test" "intent" "" "" "" "" "")
  (test-wpt-parse "intent://test/a/../b" "intent" "test" "" "" "" "")
  (test-wpt-parse "urn://example.com:8080/pathname?search#hash" "urn" "example.com" "8080" "#hash" "" "")
  (test-wpt-parse "urn:///test" "urn" "" "" "" "" "")
  (test-wpt-parse "urn://test/a/../b" "urn" "test" "" "" "" "")
  (test-wpt-parse "turn://example.com:8080/pathname?search#hash" "turn" "example.com" "8080" "#hash" "" "")
  (test-wpt-parse "turn:///test" "turn" "" "" "" "" "")
  (test-wpt-parse "turn://test/a/../b" "turn" "test" "" "" "" "")
  (test-wpt-parse "stun://example.com:8080/pathname?search#hash" "stun" "example.com" "8080" "#hash" "" "")
  (test-wpt-parse "stun:///test" "stun" "" "" "" "" "")
  (test-wpt-parse "stun://test/a/../b" "stun" "test" "" "" "" "")
  (test-wpt-parse "w://x:0" "w" "x" "0" "" "" "")
  (test-wpt-parse "west://x:0" "west" "x" "0" "" "" "")
  (test-wpt-parse "android://x:0/a" "android" "x" "0" "" "" "")
  (test-wpt-parse "drivefs://x:0/a" "drivefs" "x" "0" "" "" "")
  (test-wpt-parse "chromeos-steam://x:0/a" "chromeos-steam" "x" "0" "" "" "")
  (test-wpt-parse "steam://x:0/a" "steam" "x" "0" "" "" "")
  (test-wpt-parse "materialized-view://x:0/a" "materialized-view" "x" "0" "" "" "")
  (test-wpt-parse "android-app://x:0" "android-app" "x" "0" "" "" "")
  (test-wpt-parse "chrome-distiller://x:0" "chrome-distiller" "x" "0" "" "" "")
  (test-wpt-parse "chrome-extension://x:0" "chrome-extension" "x" "0" "" "" "")
  (test-wpt-parse "chrome-native://x:0" "chrome-native" "x" "0" "" "" "")
  (test-wpt-parse "chrome-resource://x:0" "chrome-resource" "x" "0" "" "" "")
  (test-wpt-parse "chrome-search://x:0" "chrome-search" "x" "0" "" "" "")
  (test-wpt-parse "fuchsia-dir://x:0" "fuchsia-dir" "x" "0" "" "" "")
  (test-wpt-parse "isolated-app://x:0" "isolated-app" "x" "0" "" "" "")

  ;; IPv4-mapped IPv6 addresses
  (test
   (url-host (string->url "http://[::ffff:127.0.0.1]/")) => "::ffff:127.0.0.1"
   (url-host (string->url "http://[::127.0.0.1]/")) => "::127.0.0.1"
   (url-host (string->url "http://[0:0:0:0:0:ffff:192.168.1.1]/")) => "0:0:0:0:0:ffff:192.168.1.1"
   (url-host (string->url "http://[64:ff9b::192.0.2.1]/")) => "64:ff9b::192.0.2.1"
   (url-host (string->url "http://[fe80::1%25eth0]/")) => "fe80::1%25eth0"
   (url-port (string->url "http://[::ffff:127.0.0.1]:8080/")) => 8080
   (url-host (string->url "http://[::ffff:127.0.0.1]:8080/")) => "::ffff:127.0.0.1"
   ;; Round-trip for IPv4-mapped IPv6
   (url->string (string->url "http://[::ffff:127.0.0.1]/")) => "http://[::ffff:127.0.0.1]/"
   (url->string (string->url "http://[::ffff:127.0.0.1]:8080/path")) => "http://[::ffff:127.0.0.1]:8080/path"
   (url->string (string->url "http://[fe80::1%25eth0]/")) => "http://[fe80::1%25eth0]/"
  )

  ;; === WHATWG URL Standard mode tests ===
  ;; These test cases require WHATWG-specific behavior and are run with
  ;; (current-url-standard 'whatwg). They correspond to WPT test cases that
  ;; differ from RFC 3986 behavior.
  (parameterize ([current-url-standard 'whatwg])

    ;; Userinfo normalization: trailing colon stripped, empty credentials → #f
    (test-wpt-parse "https://test:@test" "https" "test" "" "" "test" "")
    (test-wpt-parse "https://:@test" "https" "test" "" "" "" "")
    (test-wpt-parse "non-special://test:@test/x" "non-special" "test" "" "" "test" "")
    (test-wpt-parse "non-special://:@test/x" "non-special" "test" "" "" "" "")
    (test-wpt-parse "http://@www.example.com" "http" "www.example.com" "" "" "" "")
    (test-wpt-parse "http://@pple.com" "http" "pple.com" "" "" "" "")
    (test-wpt-parse "http://a:@www.example.com" "http" "www.example.com" "" "" "a" "")
    (test-wpt-parse "http://:@www.example.com" "http" "www.example.com" "" "" "" "")

    ;; Userinfo percent-encoding preserved (not decoded)
    (test-wpt-parse "http://%25DOMAIN:foobar@foodomain.com/" "http" "foodomain.com" "" "" "%25DOMAIN" "foobar")

    ;; Default port stripping for special schemes
    (test-wpt-parse "http://foo:80/" "http" "foo" "" "" "" "")
    (test-wpt-parse "https://foo:443/" "https" "foo" "" "" "" "")
    (test-wpt-parse "ftp://foo:21/" "ftp" "foo" "" "" "" "")
    (test-wpt-parse "ws://foo:80/" "ws" "foo" "" "" "" "")
    (test-wpt-parse "wss://foo:443/" "wss" "foo" "" "" "" "")

    ;; Special scheme authority inference (single-slash and no-slash)
    (test-wpt-parse "http:/example.com/" "http" "example.com" "" "" "" "")
    (test-wpt-parse "ftp:/example.com/" "ftp" "example.com" "" "" "" "")
    (test-wpt-parse "https:/example.com/" "https" "example.com" "" "" "" "")
    (test-wpt-parse "ws:/example.com/" "ws" "example.com" "" "" "" "")
    (test-wpt-parse "wss:/example.com/" "wss" "example.com" "" "" "" "")
    (test-wpt-parse "http:example.com/" "http" "example.com" "" "" "" "")
    (test-wpt-parse "ftp:example.com/" "ftp" "example.com" "" "" "" "")
    (test-wpt-parse "https:example.com/" "https" "example.com" "" "" "" "")
    (test-wpt-parse "ws:example.com/" "ws" "example.com" "" "" "" "")
    (test-wpt-parse "wss:example.com/" "wss" "example.com" "" "" "" "")

    ;; Special scheme authority inference with credentials
    (test-wpt-parse "http:@www.example.com" "http" "www.example.com" "" "" "" "")
    (test-wpt-parse "http:/@www.example.com" "http" "www.example.com" "" "" "" "")
    (test-wpt-parse "http:a:b@www.example.com" "http" "www.example.com" "" "" "a" "b")
    (test-wpt-parse "http:/a:b@www.example.com" "http" "www.example.com" "" "" "a" "b")
    (test-wpt-parse "http::b@www.example.com" "http" "www.example.com" "" "" "" "b")
    (test-wpt-parse "http:/:b@www.example.com" "http" "www.example.com" "" "" "" "b")
    (test-wpt-parse "http:a:@www.example.com" "http" "www.example.com" "" "" "a" "")
    (test-wpt-parse "http:/a:@www.example.com" "http" "www.example.com" "" "" "a" "")

    ;; Fragment percent-encoding (WHATWG encodes space, <, >, `, non-ASCII)
    (test-wpt-parse "lolscheme:x x#x x" "lolscheme" "" "" "#x%20x" "" "")
    (test-wpt-parse "http://foo.bar/baz?qux#foo<bar" "http" "foo.bar" "" "#foo%3Cbar" "" "")
    (test-wpt-parse "http://foo.bar/baz?qux#foo>bar" "http" "foo.bar" "" "#foo%3Ebar" "" "")
    (test-wpt-parse "http://foo.bar/baz?qux#foo`bar" "http" "foo.bar" "" "#foo%60bar" "" "")
    (test-wpt-parse "http://example.org/test?a#%EF" "http" "example.org" "" "#%EF" "" "")
    (test-wpt-parse "data:text/plain,test#<foo> <bar>" "data" "" "" "#%3Cfoo%3E%20%3Cbar%3E" "" "")
    (test-wpt-parse "about:blank#<foo> <bar>" "about" "" "" "#%3Cfoo%3E%20%3Cbar%3E" "" "")

    ;; Non-special scheme host: case preserved, non-ASCII percent-encoded
    (test-wpt-parse "sc://ñ.test/" "sc" "%C3%B1.test" "" "" "" "")
    (test-wpt-parse "sc://ñ" "sc" "%C3%B1" "" "" "" "")
    (test-wpt-parse "sc://ñ?x" "sc" "%C3%B1" "" "" "" "")
    (test-wpt-parse "sc://ñ#x" "sc" "%C3%B1" "" "#x" "" "")
    (test-wpt-parse "non-special://%E2%80%A0/" "non-special" "%E2%80%A0" "" "" "" "")
    (test-wpt-parse "non-special://H%4fSt/path" "non-special" "H%4fSt" "" "" "" "")
    (test-wpt-parse "asdf://%43%7C/" "asdf" "%43%7C" "" "" "" "")
    (test-wpt-parse "sc://faß.ExAmPlE/" "sc" "fa%C3%9F.ExAmPlE" "" "" "" "")
  )
)

(module+ test (require (submod ".." main))) ; for raco test & drdr
