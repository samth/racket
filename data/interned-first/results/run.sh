#!/bin/bash
# Interned-first prototype: expansion time and allocation (bench.rkt, min of 5 after a
# warm-up) of the interned-introducer microbenchmarks and the deep Rhombus programs, for
# master (build-plot tree), racket-as (scope-sets-r4 7a7c482433) and racket-int
# (scope-sets-interned-first cd126ece77). The two new binaries run as bin/racket of lane1,
# a copy of the build-plot scope-sets-r4 tree. All runs on P-core 3.
set -u
R=/tmp/scopesets/ryan
I=/tmp/scopesets/ifirst
L=/tmp/scopesets/hv/lane1/racket
OUT=$I/results.txt
: > $OUT
cd $R
for f in progs/intro-10-20000.rkt progs/intro-100-20000.rkt progs/intro-1000-20000.rkt \
         progs/rhm-block-100.rkt progs/rhm-block-300.rkt progs/rhm-block-1000.rkt \
         progs/rhm-fun-100.rkt progs/rhm-fun-300.rkt progs/rhm-fun-1000.rkt; do
  r=$(PLTADDONDIR=/tmp/scopesets/buildplot/addon-master taskset -c 3 \
        /tmp/scopesets/buildplot/master/racket/bin/racket bench.rkt 5 $f 2>&1 | tail -n 1)
  echo "master $r" | tee -a $OUT
  for v in as int; do
    cp $I/racket-$v $L/bin/racket
    r=$(PLTADDONDIR=/tmp/scopesets/hv/addon-lane1 taskset -c 3 $L/bin/racket bench.rkt 5 $f 2>&1 | tail -n 1)
    echo "$v $r" | tee -a $OUT
  done
done
