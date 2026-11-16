#!/bin/bash
# Compiles the det.sh targets from scratch with each tree moved in turn to the same
# path, /tmp/scopesets/buildplot/tree, so absolute paths embedded in .zo files match.
# Restores the tree names afterward.
set -u
H=/tmp/scopesets/buildplot
P=$H/tree
for v in master sets; do
  [ -e $P ] && { echo "$P exists"; exit 1; }
  mv $H/$v $P
  export PLTADDONDIR=$H/addon-$v
  rm -rf $H/det/same-$v
  t=$(for m in racket typed/racket scribble/manual pict json net/url rackunit; do
        $P/racket/bin/racket -l racket/base -e "(display (resolved-module-path-name (module-path-index-resolve (module-path-index-join '(lib \"$m\") #f))))"; echo; done)
  PLTCOMPILEDROOTS="$H/det/same-$v:same" taskset -c 8 $P/racket/bin/racket -l- raco make $t > $H/det/same-$v.log 2>&1
  echo "exit $?" >> $H/det/same-$v.log
  mv $P $H/$v
done
echo done
