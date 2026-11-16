#!/usr/bin/env python3
"""Summarize build-plot style runs in /tmp/scopesets/buildplot/run-<variant>."""
import re, sys, os

H = '/tmp/scopesets/buildplot'
VARIANTS = ['master', 'sets', 'ctx', 'bix']
GC_RE = re.compile(r'^GC: \S+ @ ([\d,]+)K\(\+([\d,]+)K\); free [\d,]+K\(-?[\d,]+K\) (\d+)ms @ (\d+)')

def num(s): return int(s.replace(',', ''))

def hms(s):
    h, m, x = (int(v) for v in s.split(':'))
    return h * 3600 + m * 60 + x

def summarize(v):
    R = f'{H}/run-{v}'
    d = {'variant': v}
    t = open(f'{R}/time.txt').read()
    d['wall'] = t_wall = re.search(r'Elapsed \(wall clock\) time \(h:mm:ss or m:ss\): (\S+)', t).group(1)
    parts = [float(p) for p in t_wall.split(':')]
    d['wall_s'] = sum(p * 60 ** i for i, p in enumerate(reversed(parts)))
    d['user_s'] = float(re.search(r'User time \(seconds\): (\S+)', t).group(1))
    d['sys_s'] = float(re.search(r'System time \(seconds\): (\S+)', t).group(1))
    d['cpu_s'] = d['user_s'] + d['sys_s']
    d['maxrss_mb'] = int(re.search(r'Maximum resident set size \(kbytes\): (\d+)', t).group(1)) / 1024
    p = open(f'{R}/perf.txt').read()
    d['instr'] = num(re.search(r'([\d,]+)\s+cpu_core/instructions/u', p).group(1))
    d['cycles'] = num(re.search(r'([\d,]+)\s+cpu_core/cycles/u', p).group(1))
    peak = peak_tot = gc_ms = 0; last_cpu = 0; ngc = 0; major = 0
    other = []
    for line in open(f'{R}/setup.err', errors='replace'):
        m = GC_RE.match(line)
        if m:
            mem, adm, ms, cpu = num(m.group(1)), num(m.group(2)), int(m.group(3)), int(m.group(4))
            peak = max(peak, mem); peak_tot = max(peak_tot, mem + adm)
            gc_ms += ms; last_cpu = cpu; ngc += 1
            if 'MAJ' in line or 'maj' in line: major += 1
        elif not line.startswith('GC:'):
            other.append(line.rstrip('\n'))
    d.update(gc_peak_mb=peak / 1024, gc_peak_total_mb=peak_tot / 1024, gc_s=gc_ms / 1000,
             ngc=ngc, nmajor=major, gc_last_cpu_s=last_cpu / 1000)
    d['err_other'] = other
    log = open(f'{R}/setup.log', errors='replace').read().splitlines()
    phases = []
    for line in log:
        m = re.match(r'raco setup: --- (.*?) ---\s+\[(\d+:\d+:\d+)\]', line)
        if m: phases.append((m.group(1), hms(m.group(2))))
    end = int(open(f'{R}/end').read()); start = int(open(f'{R}/start').read())
    d['phases'] = phases
    d['exit'] = open(f'{R}/exit').read().strip()
    errs = [l for l in log if re.search(r'error|failed', l, re.I) and 'making' not in l]
    d['errlines'] = errs
    return d

def phase_split(d):
    # durations between consecutive phase headers; last phase until process end
    ph = d['phases']
    out = {}
    import time
    end_tod = None
    for i, (name, t) in enumerate(ph):
        nxt = ph[i + 1][1] if i + 1 < len(ph) else None
        if nxt is not None:
            dur = (nxt - t) % 86400
            key = re.sub(r'\s+', ' ', name)
            out[key] = out.get(key, 0) + dur
    return out

def pct(a, b): return 100.0 * (a - b) / b

if __name__ == '__main__':
    ds = {v: summarize(v) for v in VARIANTS if os.path.exists(f'{H}/run-{v}/exit')}
    keys = [('wall_s', 'wall (s)'), ('cpu_s', 'user+sys (s)'), ('user_s', 'user (s)'),
            ('instr', 'instructions/u'), ('cycles', 'cycles/u'), ('maxrss_mb', 'peak RSS (MB)'),
            ('gc_peak_mb', 'GC-log peak mem (MB)'), ('gc_peak_total_mb', 'GC-log peak mem+admin (MB)'),
            ('gc_s', 'total GC time (s)'), ('ngc', '# GCs')]
    print('| metric | ' + ' | '.join(ds) + ' |')
    print('|---|' + '---|' * len(ds))
    for k, label in keys:
        vals = []
        for v in ds:
            x = ds[v][k]
            vals.append(f'{x:,.0f}' if isinstance(x, int) or x > 1000 else f'{x:,.1f}')
        print(f'| {label} | ' + ' | '.join(vals) + ' |')
    print()
    for base in ['master', 'sets']:
        if base not in ds: continue
        print(f'Percent change vs {base}:')
        print('| metric | ' + ' | '.join(v for v in ds if v != base) + ' |')
        print('|---|' + '---|' * (len(ds) - 1))
        for k, label in keys:
            print(f'| {label} | ' + ' | '.join(f'{pct(ds[v][k], ds[base][k]):+.2f}%' for v in ds if v != base) + ' |')
        print()
    print('Phase durations (s, from setup.log timestamps; a phase runs until the next header):')
    allph = []
    for v in ds:
        for k in phase_split(ds[v]):
            if k not in allph: allph.append(k)
    print('| phase | ' + ' | '.join(ds) + ' |')
    print('|---|' + '---|' * len(ds))
    for k in allph:
        print(f'| {k} | ' + ' | '.join(str(phase_split(ds[v]).get(k, '')) for v in ds) + ' |')
    print()
    for v in ds:
        d = ds[v]
        print(f'== {v}: exit {d["exit"]}, error-ish setup.log lines: {len(d["errlines"])}')
        for l in d['errlines'][:40]: print('   ' + l)
        print(f'-- {v}: non-GC stderr ({len(d["err_other"])} lines):')
        for l in d['err_other'][-80:]: print('   ' + l)
        print()
