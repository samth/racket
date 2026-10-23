# Draft: the base image and the riscv64 toolchain are whatever OSS Verify
# provides; this assumes $CC targets static riscv64 musl, and that a native
# C compiler (`cc`) and qemu-user are also available.
FROM TODO-oss-verify-base-builder
RUN git clone --depth 1 https://github.com/racket/racket racket
# portable-bytecode boot files, so that Chez Scheme builds without an
# existing Racket or Chez Scheme
RUN make -C racket pb-fetch
WORKDIR $SRC/racket
COPY build.sh $SRC/
