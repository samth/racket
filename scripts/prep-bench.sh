#!/bin/bash
# Copy benchmarks into bench-<build> and compile them with that build.
set -e
for b in "$@"; do
  mkdir -p /tmp/rkt-conc/bench-$b
  cp /tmp/rkt-conc/bench/*.rkt /tmp/rkt-conc/bench-$b/
  cd /tmp/rkt-conc/bench-$b
  for f in *.rkt; do
    /tmp/rkt-conc/$b/racket/bin/racket -l racket/base -l compiler/cm -e "(managed-compile-zo \"$f\")"
  done
  echo "prepared $b"
done
