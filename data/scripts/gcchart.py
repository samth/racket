#!/usr/bin/env python3
"""Memory-over-CPU-time chart data from the debug@GC logs of the build-plot runs.
Writes variant,cpu_s,mem_after_mb,mem_before_mb,major. Keeps every major GC and,
for minor GCs, the one with the highest pre-GC memory in each of 650 CPU-time buckets."""
import gzip, re, sys

H = '/tmp/scopesets/buildplot'
GC = re.compile(r'^GC: \d+:(\w+?)\d* @ ([\d,]+)K\(\+[\d,]+K\); free ([\d,]+)K\(-?[\d,]+K\) \d+ms @ (\d+)')
num = lambda s: int(s.replace(',', ''))
BUCKETS = 650

out = open(f'{H}/results/gc-memory.csv', 'w')
out.write('variant,cpu_s,mem_after_mb,mem_before_mb,major\n')
for v in ['master', 'sets']:
    rows = []
    for line in gzip.open(f'{H}/results/{v}/setup.err.gz', 'rt', errors='replace'):
        m = GC.match(line)
        if not m or m.group(1) == 'atexit':
            continue
        before = num(m.group(2)); freed = num(m.group(3)); cpu = int(m.group(4))
        rows.append((cpu / 1000, (before - freed) / 1024, before / 1024, m.group(1) == 'MAJ'))
    end = rows[-1][0]
    best = {}
    keep = []
    for r in rows:
        if r[3]:
            keep.append(r)
        else:
            b = int(r[0] / end * BUCKETS)
            if b not in best or r[2] > best[b][2]:
                best[b] = r
    keep += best.values()
    keep.sort()
    for cpu, after, before, major in keep:
        out.write(f'{v},{cpu:.3f},{after:.1f},{before:.1f},{int(major)}\n')
    print(v, 'gcs', len(rows), 'kept', len(keep), 'majors', sum(r[3] for r in rows),
          'end cpu s', end, 'max after', max(r[1] for r in rows), 'max before', max(r[2] for r in rows),
          'max after (major)', max(r[1] for r in rows if r[3]))
