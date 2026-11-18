#!/bin/bash
# Interned-prefix measurement: the instrumented interned-first expander, run from source
# with racket/src/expander/run.rkt on the scope-sets-r4 build-plot tree's racket, P-core 3.
#   rhombus      compile the `rhombus` language and everything it requires, from source
#   shrubbery    compile `shrubbery` and its requirements, from source
#   rhm-*-300    compile one deeply nested Rhombus program, which also compiles `rhombus`;
#                summarize.rkt subtracts the `rhombus` run to isolate the program
#   typed-racket compile `typed/racket` from source, as a non-Rhombus comparison
# run.rkt's cache mode (-c) fails with this host, so every run compiles from source.
set -u
H=/tmp/scopesets/hx/int-stats
E=$H/racket/src/expander
O=$H/results
# lane2 is a scratch copy of the build-plot scope-sets-r4 tree in which
# rhombus-lib/rhombus/private/amalgam.rkt has `#:no-demod` enabled: Rhombus's amalgam step
# runs the demodularizer at compile time, which needs the compilation manager and fails
# under run.rkt
R=/tmp/scopesets/hv/lane2/racket/bin/racket
export PLTADDONDIR=/tmp/scopesets/hv/addon-lane2
mkdir -p $O
cd $E
run() {  # name args...
  local name=$1; shift
  taskset -c 3 $R run.rkt "$@" > /dev/null 2> $O/$name.err
  echo "$name exit $?" >> $O/status.txt
  grep '^PREFIX-STATS' $O/$name.err > $O/$name.stats
}
run rhombus -l rhombus
run shrubbery -l shrubbery
run rhm-fun-300 -t /tmp/scopesets/ryan/progs/rhm-fun-300.rkt
run rhm-block-300 -t /tmp/scopesets/ryan/progs/rhm-block-300.rkt
run typed-racket -l typed/racket
echo done >> $O/status.txt
