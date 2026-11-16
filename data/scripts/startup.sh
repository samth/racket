#!/bin/bash
# Startup and load measurements, master vs sets, alternating on one P-core.
# Writes startup/<variant>-<case>-<run>.{perf,time} and startup/mem.txt
set -u
H=/tmp/scopesets/buildplot
O=$H/startup
CORE=3
RUNS=10
mkdir -p $O
unset DISPLAY WAYLAND_DISPLAY
declare -A CASES=(
  [n]="-n -e ''"
  [base]="-l racket/base -e ''"
  [racket]="-l racket -e ''"
  [typed]="-l typed/racket -e ''"
  [drracket]="-l racket/base -l drracket/private/link -e ''"
)
for v in master sets; do
  mkdir -p $O/$v
  cp $H/mem-after-load.rkt $O/$v/
  PLTADDONDIR=$H/addon-$v $H/$v/racket/bin/raco make $O/$v/mem-after-load.rkt
done
# warm the page cache
for v in master sets; do for c in "${!CASES[@]}"; do
  PLTADDONDIR=$H/addon-$v eval $H/$v/racket/bin/racket ${CASES[$c]} > /dev/null 2>&1
done; done
for i in $(seq 1 $RUNS); do
  for c in n base racket typed drracket; do
    for v in master sets; do
      PLTADDONDIR=$H/addon-$v eval /usr/bin/time -f "'%e %U %S %M'" -o $O/$v-$c-$i.time \
        taskset -c $CORE perf stat -x, -e cpu_core/instructions/u,cpu_core/cycles/u -o $O/$v-$c-$i.perf \
        $H/$v/racket/bin/racket ${CASES[$c]} > /dev/null 2> $O/$v-$c-$i.err
    done
  done
done
: > $O/mem.txt
for i in 1 2 3; do
  for m in racket/base racket typed/racket; do
    for v in master sets; do
      echo "$v $m $(PLTADDONDIR=$H/addon-$v taskset -c $CORE $H/$v/racket/bin/racket $O/$v/mem-after-load.rkt $m)" >> $O/mem.txt
    done
  done
done
echo done
