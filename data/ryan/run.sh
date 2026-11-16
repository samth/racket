#!/bin/bash
# Expansion time and allocation for each generated program, master vs scope-sets-r4 (the
# build-plot trees), alternating trees per program on one P-core. bench.rkt reports the
# minimum of RUNS in-process expansions after a warm-up.
set -u
H=/tmp/scopesets/ryan
B=/tmp/scopesets/buildplot
CORE=${CORE:-4}
RUNS=${RUNS:-5}
OUT=$H/results.txt
: > $OUT
cd $H
for f in $(ls progs/*.rkt | sort -V); do
  for v in master sets; do
    r=$(PLTADDONDIR=$B/addon-$v taskset -c $CORE $B/$v/racket/bin/racket bench.rkt $RUNS $f 2>&1 | tail -n 1)
    echo "$v $r" | tee -a $OUT
  done
done
