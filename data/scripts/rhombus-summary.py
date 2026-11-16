#!/usr/bin/env python3
"""Summarize rhombus.sh runs: min and median per tree of wall, CPU, instructions, cycles,
peak RSS, and the expander's expand/eval/compile/read times and allocation."""
import re, statistics as st, glob, os

O = '/tmp/scopesets/buildplot/rhombus'

def one(v, i):
    t = open(f'{O}/{v}-{i}.time').read()
    w = re.search(r'Elapsed \(wall clock\) time \(h:mm:ss or m:ss\): (\S+)', t).group(1)
    wall = sum(float(p) * 60 ** k for k, p in enumerate(reversed(w.split(':'))))
    cpu = float(re.search(r'User time \(seconds\): (\S+)', t).group(1)) + \
          float(re.search(r'System time \(seconds\): (\S+)', t).group(1))
    rss = int(re.search(r'Maximum resident set size \(kbytes\): (\d+)', t).group(1)) / 1024
    p = open(f'{O}/{v}-{i}.perf').read()
    num = lambda s: int(s.replace(',', ''))
    ins = num(re.search(r'([\d,]+)\s+cpu_core/instructions/u', p).group(1))
    cyc = num(re.search(r'([\d,]+)\s+cpu_core/cycles/u', p).group(1))
    e = open(f'{O}/{v}-{i}.err').read()
    d = dict(wall=wall, cpu=cpu, ins=ins, cyc=cyc, rss=rss)
    for r in ['expand', 'eval', 'compile', 'read']:
        m = re.search(rf'^  {r}\s+([\d.]+)\s+(\d+)\s+([\d,]+)', e, re.M)
        d[r] = float(m.group(1)) / 1000
        d[r + '_alloc'] = num(m.group(3)) / 1024 / 1024  # GB
    d['zo'] = len(glob.glob(f'{O}/root-{v}-{i}/**/*.zo', recursive=True))
    return d

runs = {v: [one(v, i) for i in range(1, 4) if os.path.exists(f'{O}/{v}-{i}.err')] for v in ['master', 'sets']}
rows = [('wall (s)', 'wall'), ('user+sys (s)', 'cpu'), ('instructions', 'ins'), ('cycles', 'cyc'),
        ('peak RSS (MB)', 'rss'), ('expand (s)', 'expand'), ('expand alloc (GB)', 'expand_alloc'),
        ('eval (s)', 'eval'), ('compile (s)', 'compile'), ('read (s)', 'read'), ('.zo written', 'zo')]
print(f"runs: master {len(runs['master'])}, sets {len(runs['sets'])}")
print('| metric | master min | master median | sets min | sets median | median change |')
print('|---|---|---|---|---|---|')
for label, k in rows:
    m = [r[k] for r in runs['master']]; s = [r[k] for r in runs['sets']]
    f = (lambda x: f'{x:,.0f}') if k in ('ins', 'cyc', 'zo') else (lambda x: f'{x:,.2f}')
    print(f'| {label} | {f(min(m))} | {f(st.median(m))} | {f(min(s))} | {f(st.median(s))} | '
          f'{100 * (st.median(s) - st.median(m)) / st.median(m):+.1f}% |')
