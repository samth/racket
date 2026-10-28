FROM gcr.io/oss-fuzz-base/base-builder
RUN apt-get update && apt-get install -y libncurses-dev zlib1g-dev
RUN git clone --depth 1 https://github.com/racket/racket racket
# portable-bytecode boot files, so that Chez Scheme builds without an
# existing Racket or Chez Scheme
RUN make -C racket pb-fetch
WORKDIR $SRC/racket
COPY build.sh $SRC/
