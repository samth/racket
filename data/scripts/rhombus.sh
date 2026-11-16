#!/bin/bash
# Times a from-scratch compile of the Rhombus implementation in master and sets: raco make
# (one process) on every module of the Rhombus *-lib packages (rh-lib-files.txt, tests and
# docs excluded), writing compiled files to a fresh PLTCOMPILEDROOTS root so the installed
# tree is untouched. Pinned to one P-core; RUNS rounds, alternating trees.
set -u
H=/tmp/scopesets/buildplot
O=$H/rhombus
CORE=3
RUNS=${RUNS:-3}
mkdir -p $O
for i in $(seq 1 $RUNS); do
  for v in master sets; do
    R=$H/$v/racket
    cd $R/share/pkgs
    PLTADDONDIR=$H/addon-$v /usr/bin/time -v -o $O/$v-$i.time \
      taskset -c $CORE perf stat -e cpu_core/instructions/u,cpu_core/cycles/u -o $O/$v-$i.perf \
      env PLT_EXPANDER_TIMES=1 PLT_LINKLET_TIMES=1 PLTCOMPILEDROOTS=$O/root-$v-$i:same \
      $R/bin/racket -l- raco make $(cat $H/rh-lib-files.txt) > $O/$v-$i.log 2> $O/$v-$i.err
    echo "$v $i exit $?" >> $O/status.txt
  done
done
echo done >> $O/status.txt
