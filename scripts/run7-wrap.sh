#!/bin/bash
# Run run7.sh (seqpark3 comparison) while logging the machine load
cd /tmp/rkt-conc
sed -e 's/^BUILDS=.*/BUILDS=${BUILDS:-"master p-seqpark p-seqpark2 p-seqpark2b p-seqpark3 p-seqpark3b"}/' \
    -e 's/run t-/run u-/g' run6.sh > run7.sh
chmod +x run7.sh
log_load() {
  while true; do
    uptime >> results/u-load.txt
    ps -eo pcpu,psr,args --sort=-pcpu --no-headers | head -n 3 | cut -c1-120 >> results/u-load.txt
    sleep 30
  done
}
log_load &
LP=$!
time ./run7.sh
kill $LP
BUILDS="master p-seqpark p-seqpark2 p-seqpark2b p-seqpark3 p-seqpark3b" PREFIX=u- \
  python3 -I summarize.py > results/summary-seqpark3.md
grep -l -i "error\|exception" results/u-*/*.out
grep load results/u-load.txt | awk '{print $(NF-2)}' | tr '\n' ' '
echo
