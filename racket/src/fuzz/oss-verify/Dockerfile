# OSS Verify build for Racket CS. project.yaml names this file with
# `dockerfile:`, so the build context is the Racket repository at the
# commit that OSS Verify builds.
ARG BASE_BUILDER=oss-verify-base-builder
FROM ${BASE_BUILDER}
ARG REPO
ARG COMMIT
COPY . $SRC/racket
# portable-bytecode boot files, so that Chez Scheme builds without an
# existing Racket or Chez Scheme; the Makefile pins their version
RUN make -C $SRC/racket pb-fetch
COPY racket/src/fuzz/oss-verify/build.sh $SRC/
WORKDIR $SRC/racket
