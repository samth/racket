#!/bin/bash
# Usage: det.sh VARIANT CPUS -- compiles a set of modules from scratch twice into fresh
# compiled-file roots (run1: one process; run2: 6 places, reversed target order),
# leaving the installed tree untouched.
set -u
H=/tmp/scopesets/buildplot
v=$1; cpus=$2
export PLTADDONDIR=$H/addon-$v
R=$H/$v/racket
D=$H/det/$v
rm -rf $D; mkdir -p $D
TARGETS="racket typed/racket scribble/manual pict json net/url rackunit"
REV="rackunit net/url json pict scribble/manual typed/racket racket"
files() { for m in "$@"; do $R/bin/racket -e "(display (collection-file-path \"main.rkt\" \"$m\"))" 2>/dev/null || true; echo; done; }
t1=$(for m in $TARGETS; do $R/bin/racket -l racket/base -e "(display (resolved-module-path-name (module-path-index-resolve (module-path-index-join '(lib \"$m\") #f))))"; echo; done)
t2=$(echo "$t1" | tac)
echo "$t1" > $D/targets.txt
PLTCOMPILEDROOTS="$D/run1:same" taskset -c ${cpus%%-*} $R/bin/racket -l- raco make $t1 > $D/run1.log 2>&1
echo "run1 exit $?" >> $D/run1.log
PLTCOMPILEDROOTS="$D/run2:same" taskset -c $cpus $R/bin/racket -l- raco make -j 6 $t2 > $D/run2.log 2>&1
echo "run2 exit $?" >> $D/run2.log
