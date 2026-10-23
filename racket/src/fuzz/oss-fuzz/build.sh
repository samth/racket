#!/bin/bash -eu
# OSS-Fuzz build script for Racket CS; see "racket/src/fuzz/README.md".

FUZZ=$SRC/racket/racket/src/fuzz
PREFIX=$WORK/racket

# The build runs the sanitized Racket, which leaks at exit and may hit
# UBSan reports in code that the fuzzers never reach
export ASAN_OPTIONS=detect_leaks=0
export UBSAN_OPTIONS=halt_on_error=0
# Compile Scheme code with coverage counters, for the fuzzer's feedback
export PLT_CS_COVERAGE_COUNTERS=1

mkdir -p $WORK/build
cd $WORK/build
$SRC/racket/racket/src/configure \
  --enable-cs --disable-pthread --enable-libs --prefix=$PREFIX \
  CC="$CC" CFLAGS="$CFLAGS -DRACKET_COVERAGE_COUNTERS" LDFLAGS="$CFLAGS"
make -j$(nproc)
make install

COVERAGE_COUNTERS=1 CFLAGS="$CFLAGS" LDFLAGS="" \
  $FUZZ/build-targets.sh $PREFIX $WORK/build/cs/c/libracketcs.a $OUT

# Seed corpora from the core tests
$PREFIX/bin/racket $FUZZ/make-corpus.rkt \
  $SRC/racket/pkgs/racket-test-core/tests/racket $WORK/corpus
for t in eval read number regexp ; do
  (cd $WORK/corpus/$t && zip -q -r $OUT/racket_${t}_fuzzer_seed_corpus.zip .)
done
cp $OUT/racket_read_fuzzer_seed_corpus.zip $OUT/racket_print-roundtrip_fuzzer_seed_corpus.zip

for t in eval read print-roundtrip number regexp fasl ; do
  printf '[libfuzzer]\nmax_len = 4096\ntimeout = 20\n' > $OUT/racket_${t}_fuzzer.options
done
