#lang distro-build/config
(require racket/format)

(define (build-dir-name)
  (case (current-mode)
    [("release") "release-build"]
    [else "build"]))
(define (dest-dir-name)
  (case (current-mode)
    [("release") "ci-release"]
    [else (~a "ci-snapshots/" (current-stamp))]))

(define server-base-url (~a "https://snapshots.racket-lang.org/" (dest-dir-name) "/"))

(define distro-content
  '("compiler-lib" "racket-lib"))

(define (prebuilt-racket)
  (or (and (getenv "PLTHOME") (~a (getenv "PLTHOME") "/racket/" "bin/" "racket"))
      "/usr/bin/racket"))

;; The overall configuration:
(sequential
 #:pkgs distro-content
 #:dist-base-url server-base-url
 #:site-dest (build-path (getenv "DISTRO_BUILD_SITE_DEST") (dest-dir-name))
 #:plt-web-style? #t
 #:site-title (format "Snapshot: ~a" (current-stamp))
 #:fail-on-client-failures #true
 (machine #:name "Racket BC (Ubuntu 18.04, x86_64)"
          #:racket (prebuilt-racket)
          #:versionless? #true
          #:j 6)
 (machine #:name "Racket CS (Ubuntu 18.04, x86_64)"
          ;; can't use the pre-built Racket with Racket CS
          #:versionless? #true
          #:dir "cs_build"
          #:j 6
          #:variant 'cs
          #:dist-suffix "cs"))
