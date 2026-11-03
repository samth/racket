#!/bin/bash
# Run run8.sh (seqpark3 comparison) while logging the machine load
cd /tmp/rkt-conc
chmod +x run8.sh
log_load() {
  while true; do
    uptime >> results/v-load.txt
    ps -eo pcpu,psr,args --sort=-pcpu --no-headers | head -n 3 | cut -c1-120 >> results/v-load.txt
    sleep 30
  done
}
log_load &
LP=$!
time ./run8.sh
kill $LP
BUILDS="master p-seqpark p-seqpark3 p-seqpark4t p-seqpark4r p-seqpark4" PREFIX=v- \
  python3 -I summarize.py > results/summary-seqpark4.md
grep -l -i "error\|exception" results/v-*/*.out
grep load results/v-load.txt | awk '{print $(NF-2)}' | tr '\n' ' '
echo
