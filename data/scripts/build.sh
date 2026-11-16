#!/bin/bash
# Usage: build.sh NAME  -- builds minimal Racket CS in /tmp/scopesets/buildplot/NAME
set -u
H=/tmp/scopesets/buildplot
n=$1
export PLTADDONDIR=$H/addon-$n
mkdir -p $PLTADDONDIR
cd $H/$n
/usr/bin/time -v -o $H/build-$n.time taskset -c 8-19 make base CPUS=12 > $H/build-$n.log 2>&1
echo "build $n exit $?" >> $H/build-$n.log
