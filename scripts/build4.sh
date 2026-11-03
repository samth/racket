#!/bin/bash
# Build the seqpark4 variants in parallel, pinned to CPUs 8-19
cd /tmp/rkt-conc
for v in seqpark4 seqpark4t seqpark4r; do
  ( cd p-$v &&
    RACKET_HASH_LOCK=$v taskset -c 8-19 nice make base CPUS=4 PKGS="" > ../results/build-p-$v.log 2>&1
    echo "p-$v exit $?" ) &
done
wait
grep -h -A3 "^Exception" results/build-p-seqpark4*.log | head
