#!/bin/bash
# usage: run1.sh racket-binary file  -> prints expand ms, expand alloc KB, total ms
R=$1; f=$2
rm -rf $(dirname $f)/compiled
s=$(date +%s%N)
o=$(PLT_EXPANDER_TIMES=1 $R -l racket/base -l compiler/cm -e "(managed-compile-zo \"$f\")" 2>&1)
e=$(date +%s%N)
echo "$(echo "$o" | grep -E '^  expand ' | awk '{print $2, $4}') $(( (e-s)/1000000 ))"
echo "$o" | grep -i -m2 'error\|expected' >&2
