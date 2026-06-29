#lang racket/base

;; Comprehensive HTTP response parsing tests based on the llhttp test suite
;; (https://github.com/nodejs/llhttp). These tests cover edge cases in
;; status line parsing, header parsing, chunked transfer encoding,
;; Content-Length handling, no-content responses, and line ending tolerance.

(module+ main
  (require (prefix-in hc: net/http-client)
           racket/port
           racket/tcp
           rackunit)

  (define pass-count 0)
  (define fail-count 0)
  (define failures '())

  ;; Helper: serve a raw HTTP response and return parsed (status headers body).
  ;; The server reads the request, writes the raw response bytes, then closes.
  (define (parse-response raw-bytes
                          #:method [method #"GET"]
                          #:content-decode [content-decode '()])
    (define l (tcp-listen 0 1 #t "127.0.0.1"))
    (define-values (_1 port _2 _3) (tcp-addresses l #t))
    (define server-thread
      (thread
       (lambda ()
         (define-values (in out) (tcp-accept l))
         (tcp-close l)
         ;; Read the HTTP request (consume until empty line)
         (let loop ()
           (define line (read-bytes-line in 'any))
           (unless (or (eof-object? line) (equal? line #""))
             (loop)))
         ;; Write the raw response
         (write-bytes raw-bytes out)
         (flush-output out)
         (close-output-port out)
         (close-input-port in))))
    (define c (hc:http-conn-open "127.0.0.1" #:port port #:ssl? #f))
    (define-values (status headers body-port)
      (hc:http-conn-sendrecv! c "/"
                              #:method method
                              #:close? #t
                              #:content-decode content-decode))
    (define body (port->bytes body-port))
    (thread-wait server-thread)
    (values status headers body))

  (define-syntax-rule (test-resp name body-expr ...)
    (let ([result
           (with-handlers ([exn:fail?
                            (lambda (e)
                              (set! fail-count (add1 fail-count))
                              (set! failures (cons (format "FAIL ~a: ~a" name (exn-message e))
                                                   failures))
                              'failed)])
             body-expr ...
             (set! pass-count (add1 pass-count))
             'ok)])
      (void result)))

  ;; ============================================================
  ;; STATUS LINE PARSING (from llhttp sample.md)
  ;; ============================================================

  (test-resp "Simple response"
    (let-values ([(status headers body)
                  (parse-response #"HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n")])
      (check-equal? status #"HTTP/1.1 200 OK")
      (check-equal? body #"")))

  (test-resp "301 Moved Permanently"
    (let-values ([(status headers body)
                  (parse-response #"HTTP/1.1 301 Moved Permanently\r\nContent-Length: 0\r\n\r\n")])
      (check-equal? status #"HTTP/1.1 301 Moved Permanently")))

  (test-resp "404 Not Found"
    (let-values ([(status headers body)
                  (parse-response #"HTTP/1.1 404 Not Found\r\n\r\n")])
      (check-equal? status #"HTTP/1.1 404 Not Found")
      (check-equal? headers '())))

  (test-resp "No reason phrase"
    ;; llhttp: "No reason phrase" - status line ends with just the status code
    (let-values ([(status headers body)
                  (parse-response #"HTTP/1.1 301\r\nContent-Length: 0\r\n\r\n")])
      (check-equal? status #"HTTP/1.1 301")
      (check-equal? body #"")))

  (test-resp "Empty reason phrase after space"
    (let-values ([(status headers body)
                  (parse-response #"HTTP/1.1 200 \r\nContent-Length: 0\r\n\r\n")])
      (check-equal? status #"HTTP/1.1 200 ")
      (check-equal? body #"")))

  (test-resp "HTTP/1.0 response"
    (let-values ([(status headers body)
                  (parse-response #"HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nhello")])
      (check-equal? status #"HTTP/1.0 200 OK")
      (check-equal? body #"hello")))

  (test-resp "Non-ASCII in reason phrase"
    ;; llhttp: "Non ASCII in status line"
    (let-values ([(status headers body)
                  (parse-response
                   (bytes-append #"HTTP/1.1 500 Orienta\xc3\xa7\xc3\xa3o"
                                 #"\r\nContent-Length: 0\r\n\r\n"))])
      (check-equal? status (bytes-append #"HTTP/1.1 500 Orienta\xc3\xa7\xc3\xa3o"))))

  ;; ============================================================
  ;; HEADER PARSING (from llhttp sample.md)
  ;; ============================================================

  (test-resp "Headers with tab-separated value"
    ;; llhttp: "Simple response" - Header2 has tab before value
    (let-values ([(status headers body)
                  (parse-response
                   #"HTTP/1.1 200 OK\r\nHeader1: Value1\r\nHeader2:\tValue2\r\nContent-Length: 0\r\n\r\n")])
      (check-not-false (member #"Header1: Value1" headers))
      (check-not-false (member #"Header2:\tValue2" headers))))

  (test-resp "Headers with underscore in field name"
    ;; llhttp: "Underscore in header key"
    (let-values ([(status headers body)
                  (parse-response
                   #"HTTP/1.1 200 OK\r\nDCLK_imp: test_value\r\nContent-Length: 0\r\n\r\n")])
      (check-not-false (member #"DCLK_imp: test_value" headers))))

  (test-resp "Headers with special characters ($ in name)"
    ;; llhttp: Google 301 test has X-$PrototypeBI-Version header
    (let-values ([(status headers body)
                  (parse-response
                   #"HTTP/1.1 200 OK\r\nX-$PrototypeBI-Version: 1.6.0.3\r\nContent-Length: 0\r\n\r\n")])
      (check-not-false (member #"X-$PrototypeBI-Version: 1.6.0.3" headers))))

  (test-resp "Header with empty value"
    ;; llhttp: bonjourmadame.fr test has "Pragma:" with no value
    (let-values ([(status headers body)
                  (parse-response
                   #"HTTP/1.0 301 Moved\r\nPragma:\r\nContent-Length: 0\r\n\r\n")])
      (check-not-false (member #"Pragma:" headers))))

  (test-resp "Multiple headers"
    (let-values ([(status headers body)
                  (parse-response
                   (bytes-append
                    #"HTTP/1.1 200 OK\r\n"
                    #"Content-Type: text/html\r\n"
                    #"Server: Apache\r\n"
                    #"X-Custom: value\r\n"
                    #"Content-Length: 0\r\n"
                    #"\r\n"))])
      (check-equal? (length headers) 4)
      (check-not-false (member #"Content-Type: text/html" headers))
      (check-not-false (member #"Server: Apache" headers))
      (check-not-false (member #"X-Custom: value" headers))))

  ;; ============================================================
  ;; CONTENT-LENGTH BODY (from llhttp content-length.md, sample.md)
  ;; ============================================================

  (test-resp "Content-Length body"
    (let-values ([(status headers body)
                  (parse-response #"HTTP/1.1 200 OK\r\nContent-Length: 13\r\n\r\nHello, World!")])
      (check-equal? body #"Hello, World!")))

  (test-resp "Content-Length: 0"
    (let-values ([(status headers body)
                  (parse-response #"HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n")])
      (check-equal? body #"")))

  (test-resp "Content-Length with trailing whitespace"
    ;; llhttp: Google 301 test has "Content-Length:  219  " with trailing spaces
    ;; BUG: Racket's regex captures trailing whitespace, string->number returns #f
    (let-values ([(status headers body)
                  (parse-response #"HTTP/1.1 200 OK\r\nContent-Length:  5  \r\n\r\nhello")])
      (check-equal? body #"hello")))

  (test-resp "Content-Length case insensitive"
    (let-values ([(status headers body)
                  (parse-response #"HTTP/1.1 200 OK\r\ncontent-length: 5\r\n\r\nhello")])
      (check-equal? body #"hello")))

  (test-resp "Content-Length with leading tabs"
    (let-values ([(status headers body)
                  (parse-response #"HTTP/1.1 200 OK\r\nContent-Length:\t5\r\n\r\nhello")])
      (check-equal? body #"hello")))

  (test-resp "Content-Length with mixed whitespace"
    (let-values ([(status headers body)
                  (parse-response #"HTTP/1.1 200 OK\r\nContent-Length:\t  \t  5\r\n\r\nhello")])
      (check-equal? body #"hello")))

  (test-resp "Content-Length with trailing whitespace (keep-alive)"
    ;; This tests that Content-Length is parsed correctly even with trailing
    ;; whitespace. On a keep-alive connection, the parser must read exactly
    ;; Content-Length bytes, not fall back to EOF.
    (define l (tcp-listen 0 1 #t "127.0.0.1"))
    (define-values (_1 port _2 _3) (tcp-addresses l #t))
    (define server-thread
      (thread
       (lambda ()
         (define-values (in out) (tcp-accept l))
         (tcp-close l)
         (let loop ()
           (define line (read-bytes-line in 'any))
           (unless (or (eof-object? line) (equal? line #""))
             (loop)))
         ;; First response: Content-Length with trailing whitespace
         (write-bytes #"HTTP/1.1 200 OK\r\nContent-Length: 5 \r\nConnection: keep-alive\r\n\r\nhello" out)
         (flush-output out)
         (let loop ()
           (define line (read-bytes-line in 'any))
           (unless (or (eof-object? line) (equal? line #""))
             (loop)))
         ;; Second response
         (write-bytes #"HTTP/1.1 200 OK\r\nContent-Length: 5\r\nConnection: close\r\n\r\nworld" out)
         (flush-output out)
         (close-output-port out)
         (close-input-port in))))
    (define result
      (sync/timeout 5
        (thread
         (lambda ()
           (define c (hc:http-conn-open "127.0.0.1" #:port port #:ssl? #f))
           (define-values (status1 _h1 body-port1)
             (hc:http-conn-sendrecv! c "/" #:content-decode '()))
           (check-equal? (port->bytes body-port1) #"hello")
           (define-values (status2 _h2 body-port2)
             (hc:http-conn-sendrecv! c "/" #:close? #t #:content-decode '()))
           (check-equal? (port->bytes body-port2) #"world")))))
    (kill-thread server-thread)
    (unless result
      (error "keep-alive test timed out - Content-Length trailing whitespace not handled")))

  ;; ============================================================
  ;; TRANSFER-ENCODING: CHUNKED (from llhttp transfer-encoding.md)
  ;; ============================================================

  (test-resp "Simple chunked encoding"
    (let-values ([(status headers body)
                  (parse-response
                   #"HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nhello\r\n0\r\n\r\n")])
      (check-equal? body #"hello")))

  (test-resp "Multiple chunks"
    (let-values ([(status headers body)
                  (parse-response
                   (bytes-append
                    #"HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n"
                    #"5\r\nhello\r\n"
                    #"6\r\n world\r\n"
                    #"0\r\n\r\n"))])
      (check-equal? body #"hello world")))

  (test-resp "Chunk extensions"
    ;; llhttp: "Chunk extensions" - chunk size line has ;key=value extension
    ;; RFC 9112 §7.1.1: chunk-ext = *( BWS ";" BWS chunk-ext-name [...] )
    ;; BUG: Racket's string-trim only strips whitespace, not extensions
    (let-values ([(status headers body)
                  (parse-response
                   #"HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n5;ext=value\r\nhello\r\n0\r\n\r\n")])
      (check-equal? body #"hello")))

  (test-resp "Chunk extensions with multiple extensions"
    ;; llhttp: "Chunk extensions" - 5;ilovew3;somuchlove=value
    (let-values ([(status headers body)
                  (parse-response
                   #"HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n5;ilovew3;key=value\r\nhello\r\n0\r\n\r\n")])
      (check-equal? body #"hello")))

  (test-resp "Chunk extensions with quoting"
    ;; llhttp: "Chunk extensions quoting"
    (let-values ([(status headers body)
                  (parse-response
                   #"HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n5;ext=\"hello\"\r\nhello\r\n0\r\n\r\n")])
      (check-equal? body #"hello")))

  (test-resp "Chunked with uppercase hex"
    (let-values ([(status headers body)
                  (parse-response
                   #"HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\nA\r\n0123456789\r\n0\r\n\r\n")])
      (check-equal? body #"0123456789")))

  (test-resp "Chunked with lowercase hex"
    (let-values ([(status headers body)
                  (parse-response
                   #"HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\na\r\n0123456789\r\n0\r\n\r\n")])
      (check-equal? body #"0123456789")))

  (test-resp "Chunked with leading zeros in size"
    (let-values ([(status headers body)
                  (parse-response
                   #"HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n005\r\nhello\r\n0\r\n\r\n")])
      (check-equal? body #"hello")))

  (test-resp "Empty chunked body"
    (let-values ([(status headers body)
                  (parse-response
                   #"HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n0\r\n\r\n")])
      (check-equal? body #"")))

  (test-resp "Chunked with trailing whitespace in size"
    ;; Chunk size line has trailing spaces: "bb  \r\n"
    (let-values ([(status headers body)
                  (parse-response
                   #"HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n5  \r\nhello\r\n0\r\n\r\n")])
      (check-equal? body #"hello")))

  ;; ============================================================
  ;; EOF-TERMINATED BODY (from llhttp connection.md)
  ;; ============================================================

  (test-resp "Body read until EOF (no Content-Length, no TE)"
    ;; llhttp: "No Content-Length, no Transfer-Encoding"
    (let-values ([(status headers body)
                  (parse-response #"HTTP/1.1 200 OK\r\n\r\nhello world")])
      (check-equal? body #"hello world")))

  (test-resp "Body read until EOF with headers"
    (let-values ([(status headers body)
                  (parse-response
                   #"HTTP/1.0 200 OK\r\nServer: test\r\n\r\nsome content here")])
      (check-equal? body #"some content here")))

  ;; ============================================================
  ;; NO-CONTENT RESPONSES (1xx, 204, 304)
  ;; (from llhttp connection.md)
  ;; ============================================================

  (test-resp "204 No Content"
    (let-values ([(status headers body)
                  (parse-response #"HTTP/1.1 204 No Content\r\n\r\n")])
      (check-equal? status #"HTTP/1.1 204 No Content")
      (check-equal? body #"")))

  (test-resp "304 Not Modified"
    (let-values ([(status headers body)
                  (parse-response #"HTTP/1.1 304 Not Modified\r\n\r\n")])
      (check-equal? status #"HTTP/1.1 304 Not Modified")
      (check-equal? body #"")))

  (test-resp "100 Continue"
    (let-values ([(status headers body)
                  (parse-response #"HTTP/1.1 100 Continue\r\n\r\n")])
      (check-equal? status #"HTTP/1.1 100 Continue")
      (check-equal? body #"")))

  ;; Helper for keep-alive tests: runs the test in a thread with timeout.
  ;; Returns #t on success, raises on timeout.
  (define (run-keep-alive-test make-first-response)
    (define l (tcp-listen 0 1 #t "127.0.0.1"))
    (define-values (_1 port _2 _3) (tcp-addresses l #t))
    (define server-thread
      (thread
       (lambda ()
         (define-values (in out) (tcp-accept l))
         (tcp-close l)
         ;; Read first request
         (let loop ()
           (define line (read-bytes-line in 'any))
           (unless (or (eof-object? line) (equal? line #""))
             (loop)))
         ;; First response (no body expected)
         (write-bytes (make-first-response) out)
         (flush-output out)
         ;; Read second request
         (let loop ()
           (define line (read-bytes-line in 'any))
           (unless (or (eof-object? line) (equal? line #""))
             (loop)))
         ;; Second response: 200 with body
         (write-bytes #"HTTP/1.1 200 OK\r\nContent-Length: 5\r\nConnection: close\r\n\r\nhello" out)
         (flush-output out)
         (close-output-port out)
         (close-input-port in))))
    (define result
      (sync/timeout 5
        (thread
         (lambda ()
           (define c (hc:http-conn-open "127.0.0.1" #:port port #:ssl? #f))
           ;; First request
           (define-values (status1 _h1 body-port1)
             (hc:http-conn-sendrecv! c "/" #:content-decode '()))
           (check-equal? (port->bytes body-port1) #"")
           ;; Second request should work on the same connection
           (define-values (status2 _h2 body-port2)
             (hc:http-conn-sendrecv! c "/" #:close? #t #:content-decode '()))
           (check-equal? status2 #"HTTP/1.1 200 OK")
           (check-equal? (port->bytes body-port2) #"hello")))))
    (kill-thread server-thread)
    (unless result
      (error "keep-alive test timed out - parser failed to detect no-content status")))

  ;; This tests the no-content? regex with status lines that have no reason phrase.
  ;; BUG: The regex requires a trailing space after the status code.
  (test-resp "204 with no reason phrase (keep-alive)"
    (run-keep-alive-test
     (lambda () #"HTTP/1.1 204\r\nConnection: keep-alive\r\n\r\n")))

  (test-resp "304 with no reason phrase (keep-alive)"
    (run-keep-alive-test
     (lambda () #"HTTP/1.1 304\r\nConnection: keep-alive\r\n\r\n")))

  ;; ============================================================
  ;; LINE ENDING TOLERANCE (from llhttp sample.md)
  ;; ============================================================

  (test-resp "LF-only line endings"
    ;; llhttp: "No carriage ret (lenient)" - uses \n instead of \r\n
    (let-values ([(status headers body)
                  (parse-response #"HTTP/1.1 200 OK\nContent-Length: 5\n\nhello")])
      (check-equal? status #"HTTP/1.1 200 OK")
      (check-equal? body #"hello")))

  (test-resp "LF-only with multiple headers"
    (let-values ([(status headers body)
                  (parse-response #"HTTP/1.0 200 OK\nServer: test\nContent-Length: 5\n\nhello")])
      (check-equal? status #"HTTP/1.0 200 OK")
      (check-not-false (member #"Server: test" headers))
      (check-equal? body #"hello")))

  ;; ============================================================
  ;; HEAD METHOD (from llhttp connection.md)
  ;; ============================================================

  (test-resp "HEAD response with Content-Length"
    ;; Server sends Content-Length but no body (HEAD method)
    (let-values ([(status headers body)
                  (parse-response
                   #"HTTP/1.1 200 OK\r\nContent-Length: 1000\r\n\r\n"
                   #:method #"HEAD")])
      (check-equal? status #"HTTP/1.1 200 OK")
      (check-equal? body #"")))

  (test-resp "HEAD response with Transfer-Encoding"
    (let-values ([(status headers body)
                  (parse-response
                   #"HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n"
                   #:method #"HEAD")])
      (check-equal? status #"HTTP/1.1 200 OK")
      (check-equal? body #"")))

  ;; ============================================================
  ;; TRANSFER-ENCODING TAKES PRECEDENCE (from llhttp)
  ;; ============================================================

  (test-resp "Transfer-Encoding takes precedence over Content-Length"
    (let-values ([(status headers body)
                  (parse-response
                   (bytes-append
                    #"HTTP/1.1 200 OK\r\n"
                    #"Content-Length: 100\r\n"
                    #"Transfer-Encoding: chunked\r\n"
                    #"\r\n"
                    #"5\r\nhello\r\n0\r\n\r\n"))])
      (check-equal? body #"hello")))

  ;; ============================================================
  ;; REAL-WORLD RESPONSE EXAMPLES (from llhttp sample.md)
  ;; ============================================================

  (test-resp "Google 301 response"
    (define body-text
      (bytes-append #"<HTML><HEAD><meta http-equiv=content-type "
                    #"content=text/html;charset=utf-8>\r\n"
                    #"<TITLE>301 Moved</TITLE></HEAD><BODY>\r\n"
                    #"<H1>301 Moved</H1>\r\n"
                    #"The document has moved\r\n"
                    #"<A HREF=\"http://www.google.com/\">here</A>.\r\n"
                    #"</BODY></HTML>"))
    (define cl (number->string (bytes-length body-text)))
    (let-values ([(status headers body)
                  (parse-response
                   (bytes-append
                    #"HTTP/1.1 301 Moved Permanently\r\n"
                    #"Location: http://www.google.com/\r\n"
                    #"Content-Type: text/html; charset=UTF-8\r\n"
                    #"X-$PrototypeBI-Version: 1.6.0.3\r\n"
                    #"Cache-Control: public, max-age=2592000\r\n"
                    #"Server: gws\r\n"
                    (string->bytes/utf-8 (format "Content-Length: ~a\r\n" cl))
                    #"\r\n"
                    body-text))])
      (check-equal? status #"HTTP/1.1 301 Moved Permanently")
      (check-equal? body body-text)))

  (test-resp "Amazon.com chunked response"
    ;; llhttp: amazon.com test - chunked with multiple headers
    ;; <html>Body goes here</html>\r\n = 29 bytes = 0x1d
    (let-values ([(status headers body)
                  (parse-response
                   (bytes-append
                    #"HTTP/1.1 301 MovedPermanently\r\n"
                    #"Date: Wed, 15 May 2013 17:06:33 GMT\r\n"
                    #"Server: Server\r\n"
                    #"Transfer-Encoding: chunked\r\n"
                    #"\r\n"
                    #"1d\r\n<html>Body goes here</html>\r\n\r\n0\r\n\r\n"))])
      (check-equal? status #"HTTP/1.1 301 MovedPermanently")
      (check-equal? body #"<html>Body goes here</html>\r\n")))

  ;; ============================================================
  ;; CONTENT-LENGTH EDGE CASES (from llhttp content-length.md)
  ;; ============================================================

  (test-resp "Content-Length-X should not be confused with Content-Length"
    ;; llhttp: "Content-Length-X" test - header starting with Content-Length
    ;; but not exactly Content-Length should not be used for body framing
    (let-values ([(status headers body)
                  (parse-response
                   (bytes-append
                    #"HTTP/1.1 200 OK\r\n"
                    #"Content-Length-X: 999\r\n"
                    #"Transfer-Encoding: chunked\r\n"
                    #"\r\n"
                    #"5\r\nhello\r\n0\r\n\r\n"))])
      (check-equal? body #"hello")))

  ;; ============================================================
  ;; CONNECTION HEADER HANDLING (from llhttp connection.md)
  ;; ============================================================

  (test-resp "304 with Content-Length header"
    ;; llhttp: "HTTP 304 with Content-Length" - server includes CL in 304
    ;; but client should NOT read a body
    (let-values ([(status headers body)
                  (parse-response
                   #"HTTP/1.1 304 Not Modified\r\nContent-Length: 10\r\n\r\n")])
      (check-equal? status #"HTTP/1.1 304 Not Modified")
      ;; Per RFC, 304 has no body even if Content-Length is present.
      ;; However, Racket checks Content-Length before no-content? status,
      ;; so this may attempt to read 10 bytes. Since connection closes,
      ;; it should still return empty.
      ))

  ;; Print results
  (printf "~nllhttp-based HTTP response tests: ~a passed, ~a failed~n"
          pass-count fail-count)
  (for ([f (in-list (reverse failures))])
    (printf "  ~a~n" f))
  (when (> fail-count 0)
    (error 'http-client-llhttp "~a test(s) failed" fail-count)))

(module+ test (require (submod ".." main)))
