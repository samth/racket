#!/bin/bash
# Add-scopes isolation: compile each program with each binary via compiler/cm's
# managed-compile-zo after deleting the program's compiled/ directory; report the
# PLT_EXPANDER_TIMES expand time and allocation and the process wall time. One warm-up
# process, then 5 measured processes per (program, binary); binaries alternate within each
# repetition. P-core 3. master runs the build-plot master tree; fix (853ff8ccea), as
# (7a7c482433) and int (cd126ece77) run as bin/racket of lane1, a copy of the build-plot
# scope-sets-r4 tree.
set -u
A=/tmp/scopesets/addsc
L=/tmp/scopesets/hv/lane1/racket
OUT=$A/raw.txt
: > $OUT
declare -A BIN=([fix]=/tmp/scopesets/hv/bins/racket-fix [as]=/tmp/scopesets/ifirst/racket-as [int]=/tmp/scopesets/ifirst/racket-int)

one() {  # variant program rep
  local v=$1 f=$2 rep=$3 R addon
  if [ $v = master ]; then
    R=/tmp/scopesets/buildplot/master/racket/bin/racket; addon=/tmp/scopesets/buildplot/addon-master
  else
    R=$L/bin/racket; addon=/tmp/scopesets/hv/addon-lane1
    cmp -s ${BIN[$v]} $R || cp ${BIN[$v]} $R
  fi
  rm -rf -- "$A/progs/compiled"
  local s e o
  s=$(date +%s%N)
  o=$(PLTADDONDIR=$addon PLT_EXPANDER_TIMES=1 taskset -c 3 $R -l racket/base -l compiler/cm \
        -e "(managed-compile-zo \"$A/progs/$f\")" 2>&1)
  e=$(date +%s%N)
  echo "$v $f $rep $(echo "$o" | grep -E '^  expand ' | awk '{print $2, $4}') $(( (e-s)/1000000 ))" >> $OUT
  echo "$o" | grep -i -m2 'error' >&2
}

for f in x-fixed-4000.rkt x-fixed-8000.rkt interned-flat-10000.rkt; do
  for v in master fix as int; do one $v $f warmup; done
  for rep in 1 2 3 4 5; do
    for v in master fix as int; do one $v $f $rep; done
  done
done
echo done >> $OUT
