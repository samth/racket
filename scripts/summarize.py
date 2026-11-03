#!/usr/bin/env python3
# Summarize /tmp/rkt-conc/results/<bench>/<build>-<round>.{out,perf}:
# median [min-max] per metric per build.
import os, re, statistics, sys

R = "/tmp/rkt-conc/results"
BUILDS = os.environ.get("BUILDS", "master fastpath spin").split()
PREFIX = os.environ.get("PREFIX", "")

def perf(path):
    d = {}
    txt = open(path).read()
    m = re.search(r"([\d,.]+)\s+msec task-clock", txt)
    if m: d["task-clock ms"] = float(m.group(1).replace(",", ""))
    m = re.search(r"([\d,]+)\s+cpu_core/instructions/u", txt)
    if m: d["instr (M)"] = int(m.group(1).replace(",", "")) / 1e6
    m = re.search(r"([\d.]+) seconds time elapsed", txt)
    if m: d["wall ms"] = float(m.group(1)) * 1000
    return d

def out(bench, path):
    d = {}
    txt = open(path).read()
    if "error" in txt.lower() or "bad count" in txt or "lost own write" in txt:
        d["ERROR"] = 1
    m = re.search(r"ref (\d+) set! (\d+)", txt)
    if m:
        d["ref ms"] = int(m.group(1)); d["set! ms"] = int(m.group(2))
    m = re.match(r"([\d.]+) ms ", txt)
    if m: d["compute ms"] = float(m.group(1))
    m = re.search(r"total ([\d.]+) ms; per pass median: span ([\d.]+)", txt)
    if m:
        d["stagger total ms"] = float(m.group(1)); d["pass span ms"] = float(m.group(2))
    m = re.search(r"(readers|private|compile|writers) ([\d.]+) ms", txt)
    if m: d[m.group(1) + " ms"] = float(m.group(2))
    m = re.search(r"process cpu (\d+) ms", txt)
    if m: d["idle-sleep cpu ms"] = int(m.group(1))
    return d

def fmt(v):
    v = sorted(v)
    med = statistics.median(v)
    f = (lambda x: f"{x:.0f}") if med >= 10 else (lambda x: f"{x:.2f}")
    return f"{f(med)} [{f(v[0])}-{f(v[-1])}]"

for bench in sorted(os.listdir(R)):
    p = os.path.join(R, bench)
    if not os.path.isdir(p) or not bench.startswith(PREFIX): continue
    rows = {}
    for b in BUILDS:
        vals = {}
        for fn in sorted(os.listdir(p)):
            if not fn.startswith(b + "-"): continue
            base = os.path.join(p, fn[:-4] if fn.endswith(".out") else fn[:-5])
            if fn.endswith(".out"):
                d = {**out(bench, base + ".out"), **perf(base + ".perf")}
                for k, v in d.items(): vals.setdefault(k, []).append(v)
        rows[b] = vals
    keys = [k for k in ["ref ms", "set! ms", "instr (M)", "compute ms", "stagger total ms", "pass span ms",
                      "idle-sleep cpu ms", "readers ms", "private ms", "compile ms", "writers ms", "wall ms", "task-clock ms", "ERROR"]
            if any(k in rows[b] for b in BUILDS)]
    print(f"\n## {bench}")
    print("| build | n | " + " | ".join(keys) + " |")
    print("|---|---|" + "---|" * len(keys))
    for b in BUILDS:
        n = len(rows[b].get("task-clock ms", []))
        print(f"| {b} | {n} | " + " | ".join(fmt(rows[b][k]) if k in rows[b] else "" for k in keys) + " |")
