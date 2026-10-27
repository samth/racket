#!/bin/bash -eu
# OSS-Fuzz build script for Racket CS; see "racket/src/fuzz/README.md".

FUZZ=$SRC/racket/racket/src/fuzz
TARGETS="eval read print-roundtrip number regexp fasl"
export TARGETS

# The build runs the sanitized Racket, which leaks at exit and may hit
# UBSan reports in code that the fuzzers never reach
export ASAN_OPTIONS=detect_leaks=0
export UBSAN_OPTIONS=halt_on_error=0
# Compile Scheme code with coverage counters, for the fuzzer's feedback
export PLT_CS_COVERAGE_COUNTERS=1

# Up to two builds: the normal one ("racket_*" targets), and one configured
# with `--enable-check` ("racket_check_*" targets), where Racket's core
# layers (expander, io, regexp, thread, schemify) are compiled in safe
# mode and Chez Scheme's own code at optimize-level 2, so that a broken
# internal assumption raises an error instead of corrupting memory
build_variant () {
  local name=$1 ; shift
  local prefix=$WORK/$name
  mkdir -p $WORK/build-$name
  cd $WORK/build-$name
  $SRC/racket/racket/src/configure \
    --enable-cs --disable-pthread --enable-libs --prefix=$prefix "$@" \
    CC="$CC" CFLAGS="$CFLAGS -DRACKET_COVERAGE_COUNTERS" LDFLAGS="$CFLAGS"
  make -j$(nproc)
  make install
  TARGET_PREFIX=$name COVERAGE_COUNTERS=1 CFLAGS="$CFLAGS" LDFLAGS="" \
    $FUZZ/build-targets.sh $prefix $WORK/build-$name/cs/c/libracketcs.a $OUT
}

# the heap-check target has Chez Scheme check the whole heap after each
# input; it is slow, so it gets a longer timeout below
TARGETS="$TARGETS heap-eval" build_variant racket
# The check build is off for now: in a Racket CS built with
# `--enable-check`, the first call into Racket through the embedding
# API (`racket_apply` after `racket_boot`) never returns, so its fuzz
# targets fail at startup. Set RACKET_FUZZ_CHECK=1 to build them anyway.
if [ "${RACKET_FUZZ_CHECK:-0}" = 1 ] ; then
  build_variant racket_check --enable-check
fi

# Seed corpora from the core tests
$WORK/racket/bin/racket $FUZZ/make-corpus.rkt \
  $SRC/racket/pkgs/racket-test-core/tests/racket $WORK/corpus
for t in eval read number regexp fasl ; do
  (cd $WORK/corpus/$t && zip -q -r $OUT/racket_${t}_fuzzer_seed_corpus.zip .)
done
cp $OUT/racket_read_fuzzer_seed_corpus.zip $OUT/racket_print-roundtrip_fuzzer_seed_corpus.zip

for t in $TARGETS ; do
  printf '[libfuzzer]\nmax_len = 4096\ntimeout = 20\n' > $OUT/racket_${t}_fuzzer.options
  if [ -f $OUT/racket_check_${t}_fuzzer ] ; then
    # safe-mode code is slower
    printf '[libfuzzer]\nmax_len = 4096\ntimeout = 40\n' > $OUT/racket_check_${t}_fuzzer.options
    if [ -f $OUT/racket_${t}_fuzzer_seed_corpus.zip ] ; then
      cp $OUT/racket_${t}_fuzzer_seed_corpus.zip $OUT/racket_check_${t}_fuzzer_seed_corpus.zip
    fi
  fi
done
cp $OUT/racket_eval_fuzzer_seed_corpus.zip $OUT/racket_heap-eval_fuzzer_seed_corpus.zip
printf '[libfuzzer]\nmax_len = 4096\ntimeout = 60\n' > $OUT/racket_heap-eval_fuzzer.options
