#!/usr/bin/env python3
# Run racket under taskset -c 0-3, and as soon as the pool's worker OS
# threads appear (tasks 2-5), pin worker k to CPU k.
import os, subprocess, sys, time

p = subprocess.Popen(["taskset", "-c", "0-3"] + sys.argv[1:])
pinned = False
while p.poll() is None and not pinned:
    try:
        tids = sorted(int(t) for t in os.listdir(f"/proc/{p.pid}/task"))
    except FileNotFoundError:
        break
    if len(tids) >= 5:
        for k, tid in enumerate(tids[1:5]):
            os.sched_setaffinity(tid, {k})
        pinned = True
        print("pinned", tids[1:5], file=sys.stderr)
    time.sleep(0.0002)
p.wait()
