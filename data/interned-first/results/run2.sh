#!/bin/bash
# racket-as vs racket-int on the two repeatable compile workloads, in lane1 (a copy of the
# build-plot scope-sets-r4 tree) on P-core 3: typed/racket with raco make into a fresh
# PLTCOMPILEDROOTS root, and the Rhombus -lib packages after rh-clean.sh with one raco make
# of the aggregator module. Three rounds, alternating which binary goes first.
set -u
H=/tmp/scopesets/hv
I=/tmp/scopesets/ifirst
O=$I/compile
T=$H/lane1/racket
mkdir -p $O
for round in 1 2 3; do
  if [ $((round % 2)) = 1 ]; then order="as int"; else order="int as"; fi
  for v in $order; do
    cp $I/racket-$v $T/bin/racket
    rm -rf $I/root
    (cd $T &&
     PLTADDONDIR=$H/addon-lane1 /usr/bin/time -v -o $O/$v-typed-$round.time \
       taskset -c 3 perf stat -e cpu_core/instructions/u,cpu_core/cycles/u -o $O/$v-typed-$round.perf \
       env PLT_EXPANDER_TIMES=1 PLTCOMPILEDROOTS=$I/root:same \
       $T/bin/racket -l- raco make share/pkgs/typed-racket-lib/typed/racket.rkt \
       > $O/$v-typed-$round.log 2> $O/$v-typed-$round.err)
    echo "$v typed $round exit $?" >> $O/status.txt
    bash $H/rh-clean.sh $T
    (cd $T &&
     PLTADDONDIR=$H/addon-lane1 /usr/bin/time -v -o $O/$v-rhombus-$round.time \
       taskset -c 3 perf stat -e cpu_core/instructions/u,cpu_core/cycles/u -o $O/$v-rhombus-$round.perf \
       env PLT_EXPANDER_TIMES=1 \
       $T/bin/racket -l- raco make /tmp/scopesets/rhagg/lane1/all.rkt \
       > $O/$v-rhombus-$round.log 2> $O/$v-rhombus-$round.err)
    echo "$v rhombus $round exit $?" >> $O/status.txt
  done
done
rm -rf $I/root
echo done >> $O/status.txt
