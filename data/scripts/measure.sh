#!/bin/bash
# Usage: measure.sh NAME CORE -- the build-plot measurement: raco setup -j 1 on one P-core
set -u
H=/tmp/scopesets/buildplot
n=$1; core=$2
export PLTADDONDIR=$H/addon-$n
R=$H/run-$n
mkdir -p $R
cd $H/$n
date +%s > $R/start
/usr/bin/time -v -o $R/time.txt \
  taskset -c $core perf stat -e cpu_core/instructions/u,cpu_core/cycles/u -o $R/perf.txt \
  env PLT_EXPANDER_TIMES=1 PLT_LINKLET_TIMES=1 \
  racket/bin/racket -W "debug@GC error" -l- raco setup -j 1 > $R/setup.log 2> $R/setup.err
echo $? > $R/exit
date +%s > $R/end
