#!/usr/bin/env python3
# Bucket `perf script -F tid,cpu,time` samples into 2 ms slices and print,
# for each slice, which tid ran most on each CPU (. = idle).
import collections, sys

workers = [int(t) for t in sys.argv[2].split(",")]
main = int(sys.argv[3])
names = {main: "M", **{w: str(i) for i, w in enumerate(workers)}}
b = collections.defaultdict(lambda: collections.defaultdict(collections.Counter))
for line in open(sys.argv[1]):
    f = line.split()
    if len(f) < 3:
        continue
    tid, cpu, t = int(f[0]), int(f[1].strip("[]")), float(f[2].rstrip(":"))
    b[int(t * 500)][cpu][tid] += 1
start = min(b)
idle_with_missing = 0
total = 0
for k in sorted(b):
    row = []
    on = set()
    idle = 0
    for cpu in range(4):
        c = b[k][cpu]
        if not c:
            row.append("?")
            continue
        tid, _ = c.most_common(1)[0]
        on.add(tid)
        if tid == 0:
            idle += 1
        row.append("." if tid == 0 else names.get(tid, "x"))
    missing = [names[w] for w in workers if w not in on]
    total += 1
    if idle and len(missing) and len(on & set(workers)) > 0:
        idle_with_missing += 1
    print(f"{(k - start) * 2:5d}ms {''.join(row)}  missing:{''.join(missing)}")
print(f"slices with an idle CPU while some worker ran nowhere (and others ran): {idle_with_missing} of {total}")
