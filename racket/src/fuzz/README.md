Fuzzing Racket CS
=================

These files build libFuzzer targets for Racket CS. Each target boots
Racket once and calls one entry point of "safe-eval.rkt" on each input:

 * `fuzz-eval`: reads the input as Racket forms and evaluates them
 * `fuzz-read`: reads the input with `read-syntax`
 * `fuzz-print-roundtrip`: checks that printing what was read reads back
   as an `equal?` value
 * `fuzz-number`: checks that a number read with `string->number`
   survives `number->string`
 * `fuzz-regexp`: compiles a pattern with `byte-pregexp` and checks that
   `regexp-match` and `regexp-match-positions` agree on a subject
 * `fuzz-fasl`: decodes the input with `fasl->s-exp` and checks that
   re-encoding gives an `equal?` value

Untrusted input is evaluated only as safe Racket, so a crash is a bug in
Racket or Chez Scheme, not in the program. "safe-eval.rkt" runs it with
a weak code inspector, which makes `#%unsafe`, `#%foreign`, `#%linklet`,
`ffi/unsafe/vm`, and `(#%declare #:unsafe)` unavailable, plus a security
guard that only allows reading installed libraries, a memory limit, a
fuel limit, and bounded output. Besides an ASan report, the oracle is an
"invalid memory reference" or "internal error" exception, which is how
Chez Scheme reports a fault in Scheme code.

Building
--------

Configure Racket CS with sanitizer flags and `--enable-libs`, build, and
install:

    CC=clang CFLAGS="-O1 -g -fno-omit-frame-pointer -fsanitize=address,fuzzer-no-link" \
    LDFLAGS=-fsanitize=address \
      .../configure --enable-cs --disable-pthread --enable-libs --prefix=$PREFIX
    make && make install

Then build the targets, with boot files and modules embedded:

    CFLAGS="-g -O1 -fsanitize=address" LDFLAGS=-fsanitize=address \
      ./build-targets.sh $PREFIX <build>/cs/c/libracketcs.a <out-dir>

Coverage of Scheme code
-----------------------

Most of Racket CS is Scheme code, which clang's instrumentation does not
see. To give the fuzzer coverage of it, add `-DRACKET_COVERAGE_COUNTERS`
to `CFLAGS`, set `PLT_CS_COVERAGE_COUNTERS=1` while building and
installing Racket, and set `COVERAGE_COUNTERS=1` for "build-targets.sh".
The Chez Scheme compiler then adds a counter increment to each branch
(see `generate-coverage-counters`), and the driver registers the counters
with libFuzzer.

Seed corpora
------------

    racket make-corpus.rkt <racket-test-core>/tests/racket <dest-dir>

extracts expressions, strings, numbers, and regexps from the
"racket-test-core" tests.

OSS-Fuzz and OSS Verify
-----------------------

"oss-fuzz/" has draft project files for OSS-Fuzz; with them, the
`address` and `undefined` builds pass OSS-Fuzz's build checks.
"oss-verify/" has drafts for OSS Verify, which needs static riscv64
binaries: Racket CS cross-builds for `riscv64-linux-musl` with
`--disable-pthread --disable-curses`, and "standalone-main.c" runs a
target built that way on input files, such as under qemu.

Known limitation: in a target with embedded modules, expanding a use of
`racket/match` fails with a "namespace mismatch" error for the `state`
submodule of "syntax/parse/private/residual.rkt", so such inputs are
less useful there than with installed collections.
