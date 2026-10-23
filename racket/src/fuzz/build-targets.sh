#!/bin/sh
# Builds the libFuzzer targets, with the boot files and compiled modules
# embedded, so that a target needs no files at run time.
#
#   build-targets.sh <racket-prefix> <libracketcs.a> <out-dir>
#
# <racket-prefix> is the installation (made with `--enable-libs`) that
# <libracketcs.a> belongs to. Its `racket` compiles the embedded modules,
# so its boot files and machine code must match the library.
#
# Environment:
#   CC, CFLAGS, LDFLAGS    compiler and flags; LDFLAGS must include the
#                          fuzzing engine, as in -fsanitize=fuzzer
#   LIB_FUZZING_ENGINE     used in place of -fsanitize=fuzzer if set
#                          (as on OSS-Fuzz)
#   COVERAGE_COUNTERS=1    the installation was built with
#                          RACKET_COVERAGE_COUNTERS and
#                          PLT_CS_COVERAGE_COUNTERS
#   TARGETS                entry points to build (default: all)

set -e

R="$1"
LIB="$2"
OUT="$3"
SRC=`cd \`dirname "$0"\` && pwd`

CC=${CC:-clang}
ENGINE=${LIB_FUZZING_ENGINE:--fsanitize=fuzzer}
TARGETS=${TARGETS:-"eval read print-roundtrip number regexp fasl"}

if [ "$COVERAGE_COUNTERS" = 1 ] ; then
  EXTRA=-DRACKET_COVERAGE_COUNTERS
  PLT_CS_COVERAGE_COUNTERS=1
  export PLT_CS_COVERAGE_COUNTERS
else
  EXTRA=
fi

# Racket may itself be built with ASan, and leaks at exit are expected
ASAN_OPTIONS=detect_leaks=0
export ASAN_OPTIONS

mkdir -p "$OUT"
"$R/bin/racket" "$SRC/embed-modules.rkt" "$OUT/fuzz-modules.bin"

$CC -c -Ulinux -Uunix "$SRC/embed-data.S" -o "$OUT/embed-data.o" \
  -DPETITE_BOOT="$R/lib/racket/petite.boot" \
  -DSCHEME_BOOT="$R/lib/racket/scheme.boot" \
  -DRACKET_BOOT="$R/lib/racket/racket.boot" \
  -DFUZZ_MODULES="$OUT/fuzz-modules.bin"

for t in $TARGETS ; do
  $CC $CFLAGS $EXTRA -I"$R/include/racket" -DFUZZ_EMBEDDED \
    -DFUZZ_ENTRY="\"fuzz-$t\"" -c "$SRC/driver.c" -o "$OUT/driver-$t.o"
  $CC $CFLAGS $LDFLAGS $ENGINE "$OUT/driver-$t.o" "$OUT/embed-data.o" "$LIB" \
    -lz -lm -ldl -lpthread -lncurses -o "$OUT/racket_${t}_fuzzer"
  rm "$OUT/driver-$t.o"
done

rm "$OUT/embed-data.o" "$OUT/fuzz-modules.bin"
