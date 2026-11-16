#!/bin/bash
# Usage: mi-compare.sh N -- for N files sampled from det/same-still-differ.txt, compiles the
# source to machine-independent form in master and sets and lists which bundle keys differ.
set -u
H=/tmp/scopesets/buildplot
N=$1
mkdir -p $H/det/mi
: > $H/det/mi/summary.txt
shuf -n $N --random-source=<(yes) $H/det/same-still-differ.txt | while read zo a b; do
  rel=${zo#/tmp/scopesets/buildplot/tree/racket/}
  src=$(echo "$rel" | sed 's|/compiled/\([^/]*\)_\([a-z]*\)\.zo$|/\1.\2|')
  tag=$(echo "$rel" | tr '/' '_')
  for v in master sets; do
    PLTADDONDIR=$H/addon-$v taskset -c 8-19 $H/$v/racket/bin/racket $H/mi-dump.rkt $H/$v/racket/$src $H/det/mi/$tag-$v.rktd > $H/det/mi/$tag-$v.err 2>&1
  done
  python3 -I - "$H/det/mi/$tag-master.rktd" "$H/det/mi/$tag-sets.rktd" "$src" >> $H/det/mi/summary.txt <<'EOF'
import sys
def load(p, v):
    try: lines = open(p).read().split('\n')
    except OSError: return None
    lines = [l.replace(f'buildplot/{v}/', 'buildplot/X/') for l in lines]
    return dict(zip(lines[0::2], lines[1::2]))
a = load(sys.argv[1], 'master'); b = load(sys.argv[2], 'sets')
if a is None or b is None:
    print(f'{sys.argv[3]}: FAILED'); sys.exit()
keys = sorted(set(a) | set(b))
diff = [k for k in keys if a.get(k) != b.get(k)]
print(f'{sys.argv[3]}: {len(keys)} keys, differing: {" ".join(diff) or "none"}')
EOF
done
