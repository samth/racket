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
  '("racket-lib" "base" "compiler-lib"))


;; The overall configuration:
(parallel
 #:pkgs distro-content
 #:dist-base-url server-base-url
 #:site-dest (build-path (getenv "DISTRO_BUILD_SITE_DEST") (dest-dir-name))
 ;#:dir (~a (build-dir-name) "/plt")
 #:plt-web-style? #t
 #:site-title (format "Snapshot: ~a" (current-stamp))
 (machine #:name "Racket BC (Ubuntu 18.04, x86_64)"
          #:racket "/usr/bin/racket"
          #:versionless? #true)
 (machine #:name "Racket CS (Ubuntu 18.04, x86_64)"
          #:racket "/usr/bin/racket"
          #:versionless? #true
          #:dir "cs_build"
          #:variant 'cs
          #:dist-suffix "cs"))
