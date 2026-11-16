#!/bin/bash
# Usage: install.sh NAME -- installs main-distribution + main-distribution-test
# (no setup) from the local archive catalog into installation scope.
set -u
H=/tmp/scopesets/buildplot
n=$1
export PLTADDONDIR=$H/addon-$n
cd $H/$n
taskset -c 8-19 racket/bin/raco pkg install -i --auto --no-setup \
  --catalog file://$H/archive/catalog main-distribution main-distribution-test \
  > $H/install-$n.log 2>&1
echo "install $n exit $?" >> $H/install-$n.log
racket/bin/raco pkg show -i -l --all > $H/pkgshow-$n.txt 2>&1
