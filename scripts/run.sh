#!/bin/bash
# Interleaved benchmark runs: 5 rounds, each round runs every build.
# Raw outputs go to /tmp/rkt-conc/results/<bench>/<build>-<round>.{out,perf}
set -u
R=/tmp/rkt-conc/results
BUILDS="master fastpath spin"
ROUNDS=${ROUNDS:-5}

run() { # name build round cpus env... -- args...
  local name=$1 b=$2 i=$3 cpus=$4; shift 4
  local envs=()
  while [ "$1" != "--" ]; do envs+=("$1"); shift; done; shift
  mkdir -p $R/$name
  local rkt=/tmp/rkt-conc/$b/racket/bin/racket
  ( cd /tmp/rkt-conc/bench-$b &&
    env "${envs[@]}" taskset -c $cpus perf stat -e $EVENTS -o $R/$name/$b-$i.perf \
      $rkt "$@" > $R/$name/$b-$i.out 2>&1 )
}

for i in $(seq 1 $ROUNDS); do
  for b in $BUILDS; do
    EVENTS=cpu_core/instructions/u,task-clock
    run hash-seq $b $i 2 X=1 -- hash-par.rkt
    run hash-par $b $i 2 PAR=1 -- hash-par.rkt
    EVENTS=task-clock
    run bt-threads $b $i 0-3 X=1 -- bt-threads.rkt 21
    run bt-par $b $i 0-3 X=1 -- bt-par.rkt 21
    run stress-w4 $b $i 0-3 W=4 -- stress.rkt
    run stress-w12 $b $i 0-3 W=12 -- stress.rkt
    run spectral-03 $b $i 0-3 N=5500 -- spectral-pt.rkt
    run spectral-04 $b $i 0-4 N=5500 -- spectral-pt.rkt
  done
done
