#!/bin/bash -eu
# Draft OSS Verify build script for Racket CS: static riscv64 musl, no
# threads, boot files and modules embedded in each target.
#
# Racket CS cross-builds with a Racket and a Chez Scheme for the build
# machine, so this first builds those natively, without threads to match
# the target's machine type (a6le, not ta6le).

SRCDIR=$SRC/racket/racket/src
FUZZ=$SRCDIR/fuzz

# 1. Native Racket for the build machine
mkdir -p $WORK/host && cd $WORK/host
CC=cc CFLAGS=-O2 LDFLAGS= $SRCDIR/configure \
  --enable-cs --disable-pthread --prefix=$WORK/host-racket
make -j$(nproc)
make install
HOST_RACKET=$WORK/host-racket/bin/racket

# 2. Cross-build for riscv64; `--disable-curses` avoids needing a
# riscv64 ncurses, and zlib and LZ4 come from the Chez Scheme sources;
# tools such as zuo that run during the build need the native compiler
mkdir -p $WORK/target && cd $WORK/target
$SRCDIR/configure --host=riscv64-linux-musl \
  --enable-cs --disable-pthread --disable-curses --enable-libs \
  --enable-racket=$HOST_RACKET --enable-scheme=$WORK/host/cs/c \
  --prefix=$WORK/target-racket \
  CC="$CC" CFLAGS="$CFLAGS" LDFLAGS="-static" CC_FOR_BUILD="cc -O2"
make -j$(nproc)
# a cross `raco setup` with parallel workers loses the cross compiler,
# so compile collections with one worker
make install PLT_SETUP_OPTIONS="-j 1"
R=$WORK/target-racket

# 3. The module bundle must hold riscv64 machine code. For now this runs
# the riscv64 Racket under qemu (about 30 seconds); TODO: produce it
# with the host Racket in cross mode instead, if OSS Verify does not
# allow running target programs during the build.
qemu-riscv64 $R/bin/racket $FUZZ/embed-modules.rkt $WORK/fuzz-modules.bin

$CC -c -Ulinux -Uunix $FUZZ/embed-data.S -o $WORK/embed-data.o \
  -DPETITE_BOOT=$R/lib/racket/petite.boot \
  -DSCHEME_BOOT=$R/lib/racket/scheme.boot \
  -DRACKET_BOOT=$R/lib/racket/racket.boot \
  -DFUZZ_MODULES=$WORK/fuzz-modules.bin

for t in eval read print-roundtrip number regexp fasl ; do
  $CC $CFLAGS -static -I$R/include/racket -DFUZZ_EMBEDDED \
    -DFUZZ_ENTRY="\"fuzz-$t\"" $FUZZ/driver.c $WORK/embed-data.o \
    $R/lib/libracketcs.a $LIB_FUZZING_ENGINE -lm \
    -o $OUT/racket_${t}_fuzzer
done
