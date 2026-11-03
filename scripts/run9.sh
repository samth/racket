#!/bin/bash
# Pool-worker spin variants: 5 interleaved rounds. A config is
# build:spin-msecs:mode; the p-ws2 build reads the spin settings from
# the environment. Raw outputs: /tmp/rkt-conc/results/w-<bench>/<config>-<round>.{out,perf}
set -u
R=/tmp/rkt-conc/results
CONFIGS=${CONFIGS:-"fastpath:-:- workerspin:-:- p-ws2:0.25:none p-ws2:0.5:none p-ws2:1:none p-ws2:2:none p-ws2:2:yield p-ws2:2:gc"}
ROUNDS=${ROUNDS:-5}

run() { # name config round cpus args...
  local name=$1 cfg=$2 i=$3 cpus=$4; shift 4
  local b ms mode
  IFS=: read b ms mode <<< "$cfg"
  local label=$b
  [ "$ms" != "-" ] && label=ws-$ms-$mode
  mkdir -p $R/$name
  ( cd /tmp/rkt-conc/bench-$b &&
    PLT_WORKER_SPIN_MSECS=$ms PLT_WORKER_SPIN_MODE=$mode N=5500 \
      taskset -c $cpus perf stat -e task-clock -o $R/$name/$label-$i.perf \
      /tmp/rkt-conc/$b/racket/bin/racket "$@" > $R/$name/$label-$i.out 2>&1 )
}

for i in $(seq 1 $ROUNDS); do
  for c in $CONFIGS; do
    for cpus in 4-7 3-7; do
      tag=${cpus/-/}
      run w-spectral-$tag $c $i $cpus spectral-pt.rkt
      run w-stagger-$tag $c $i $cpus stagger.rkt
      run w-bt-threads-$tag $c $i $cpus bt-threads.rkt 21
    done
    run w-idle $c $i 4-7 idle.rkt
  done
done
