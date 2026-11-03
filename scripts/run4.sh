#!/bin/bash
# Hash-lock prototypes: 5 interleaved rounds over every build.
# Raw outputs: /tmp/rkt-conc/results/h-<bench>/<build>-<round>.{out,perf}
# CPU 2 is avoided (another job is pinned there).
set -u
R=/tmp/rkt-conc/results
BUILDS=${BUILDS:-"master fastpath spin p-yield p-park p-seq p-bias"}
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
    run h-hash-seq $b $i 5 X=1 -- hash-par.rkt
    run h-hash-seq-always $b $i 5 PLT_HASH_ALWAYS_LOCK=1 -- hash-par.rkt
    run h-hash-par $b $i 5 PAR=1 -- hash-par.rkt
    run h-compile $b $i 5 X=1 -- compile-big.rkt
    run h-compile-always $b $i 5 PLT_HASH_ALWAYS_LOCK=1 -- compile-big.rkt
    EVENTS=task-clock
    run h-stress-w4 $b $i 4-7 W=4 -- stress.rkt
    run h-stress-w12 $b $i 4-7 W=12 -- stress.rkt
    run h-shared-read $b $i 4-7 R=4 -- shared-read.rkt
    run h-shared-read-1 $b $i 4-7 R=1 -- shared-read.rkt
    run h-shared-write $b $i 3-7 R=4 WRITER=1 -- shared-read.rkt
    run h-private $b $i 4-7 W=4 -- private-tables.rkt
  done
done
