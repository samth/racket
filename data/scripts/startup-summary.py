#!/usr/bin/env python3
"""Summarize startup.sh output: min and median wall, instructions, peak RSS per case."""
import re, statistics as st

O = '/tmp/scopesets/buildplot/startup'
CASES = [('n', "racket -n -e ''"), ('base', "racket -l racket/base -e ''"),
         ('racket', "racket -l racket -e ''"), ('typed', "racket -l typed/racket -e ''"),
         ('drracket', "racket -l racket/base -l drracket/private/link -e ''")]

def load(v, c, i):
    e, u, s, m = open(f'{O}/{v}-{c}-{i}.time').read().split()[-4:]
    p = open(f'{O}/{v}-{c}-{i}.perf').read()
    ins = int(re.search(r'^(\d+),,cpu_core/instructions/u', p, re.M).group(1))
    return float(e), float(u) + float(s), ins, int(m) / 1024

print('| command | metric | master min | master median | sets min | sets median | median change |')
print('|---|---|---|---|---|---|---|')
for c, label in CASES:
    d = {v: [load(v, c, i) for i in range(1, 11)] for v in ['master', 'sets']}
    for k, name, fmt in [(0, 'wall s', '{:.3f}'), (2, 'instructions', '{:,.0f}'), (3, 'peak RSS MB', '{:.1f}')]:
        mm = [x[k] for x in d['master']]; ss = [x[k] for x in d['sets']]
        ch = 100 * (st.median(ss) - st.median(mm)) / st.median(mm)
        print(f'| `{label}` | {name} | ' + ' | '.join(fmt.format(x) for x in
              [min(mm), st.median(mm), min(ss), st.median(ss)]) + f' | {ch:+.2f}% |')
