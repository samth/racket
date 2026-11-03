#!/usr/bin/env python3
# Run a command, poll /proc/PID/task/* until exit, and print per-thread
# CPU time, context switches, and scheduler wait time (schedstat).
import os, subprocess, sys, time

tck = os.sysconf("SC_CLK_TCK")
p = subprocess.Popen(sys.argv[1:])
last = {}
while p.poll() is None:
    try:
        tids = os.listdir(f"/proc/{p.pid}/task")
    except FileNotFoundError:
        break
    for tid in tids:
        d = f"/proc/{p.pid}/task/{tid}"
        try:
            stat = open(d + "/stat").read()
            status = open(d + "/status").read()
            sched = open(d + "/schedstat").read().split()
        except (FileNotFoundError, ProcessLookupError):
            continue
        comm = stat[stat.index("(") + 1:stat.rindex(")")]
        f = stat[stat.rindex(")") + 2:].split()
        ut, st = int(f[11]), int(f[12])
        vol = nonvol = 0
        for line in status.splitlines():
            if line.startswith("voluntary_ctxt_switches"):
                vol = int(line.split()[1])
            elif line.startswith("nonvoluntary_ctxt_switches"):
                nonvol = int(line.split()[1])
        last[tid] = (comm, ut, st, vol, nonvol, int(sched[0]), int(sched[1]), int(sched[2]))
    time.sleep(0.0005)
print(f"{'tid':>8} {'comm':16} {'utime_ms':>9} {'stime_ms':>9} {'run_ms':>8} {'wait_ms':>8} {'slices':>7} {'vol_cs':>7} {'nonvol_cs':>9}")
for tid, (comm, ut, st, vol, nonvol, run, wait, sl) in sorted(last.items(), key=lambda kv: int(kv[0])):
    print(f"{tid:>8} {comm:16} {ut*1000/tck:9.0f} {st*1000/tck:9.0f} {run/1e6:8.1f} {wait/1e6:8.1f} {sl:7d} {vol:7d} {nonvol:9d}")
