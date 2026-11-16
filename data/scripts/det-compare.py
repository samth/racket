#!/usr/bin/env python3
"""Compare .zo files compiled into the fresh roots det/<v>/run1, det/<v>/run2 against each
other and against the installed tree's .zo (compiled by the full raco setup)."""
import os, sys

H = '/tmp/scopesets/buildplot'

def root_zos(v, run):
    base = f'{H}/det/{v}/{run}'
    out = {}
    for d, _, fs in os.walk(base):
        for f in fs:
            if f.endswith('.zo'):
                p = os.path.join(d, f)
                out[p[len(base):]] = p  # absolute path of the original location
    return out

for v in sys.argv[1:]:
    r1 = root_zos(v, 'run1'); r2 = root_zos(v, 'run2')
    def cmp(x, y, label):
        common = sorted(set(x) & set(y))
        diff = [k for k in common if open(x[k], 'rb').read() != open(y[k], 'rb').read()]
        print(f'{v} {label}: {len(common)} common, {len(common) - len(diff)} identical, {len(diff)} differ')
        for k in diff[:8]:
            print('   ', k)
        return diff
    cmp(r1, r2, 'run1 vs run2')
    inst = {k: k for k in r1 if os.path.exists(k)}
    cmp(r1, inst, 'run1 vs installed')
    inst2 = {k: k for k in r2 if os.path.exists(k)}
    cmp(r2, inst2, 'run2 vs installed')
