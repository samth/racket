#!/bin/bash
# fastpath vs workerspin: spectral (7 rounds, 3 CPU settings), then controls
# (5 rounds). Raw outputs: /tmp/rkt-conc/results/<bench>/<build>-<round>.{out,perf}
set -u
R=/tmp/rkt-conc/results
BUILDS="fastpath workerspin"

run() { # name build round cpus env... -- args...
  local name=$1 b=$2 i=$3 cpus=$4; shift 4
  local envs=()
  while [ "$1" != "--" ]; do envs+=("$1"); shift; done; shift
  mkdir -p $R/$name
  local rkt=/tmp/rkt-conc/$b/racket/bin/racket
  local ts=()
  [ "$cpus" != all ] && ts=(taskset -c $cpus)
  ( cd /tmp/rkt-conc/bench-$b &&
    env "${envs[@]}" "${ts[@]}" perf stat -e task-clock -o $R/$name/$b-$i.perf \
      $rkt "$@" > $R/$name/$b-$i.out 2>&1 )
}

for i in $(seq 1 7); do
  for b in $BUILDS; do
    run ws47-spectral-4cpu $b $i 4-7 N=5500 -- spectral-pt.rkt
    run ws47-spectral-5cpu $b $i 3-7 N=5500 -- spectral-pt.rkt
    run ws47-spectral-all $b $i all N=5500 -- spectral-pt.rkt
  done
done
for i in $(seq 1 5); do
  for b in $BUILDS; do
    run ws47-bt-threads $b $i 4-7 X=1 -- bt-threads.rkt 21
    run ws47-stress-w4 $b $i 4-7 W=4 -- stress.rkt
    run ws47-stagger $b $i 4-7 X=1 -- stagger.rkt
    run ws47-idle $b $i all X=1 -- idle.rkt
  done
done
