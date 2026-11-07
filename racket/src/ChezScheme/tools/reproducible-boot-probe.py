#!/usr/bin/env python3
"""Compare fresh explicitly seeded/default bootquick builds at fixed source/build paths.

Requires a native bootstrap host. Writes only below a new evidence directory.
This is not a PB-bootstrap or cross-platform performance benchmark.
"""
from pathlib import Path
import argparse
import hashlib
import json
import shutil
import subprocess
import tempfile
import time

p = argparse.ArgumentParser(description=__doc__)
p.add_argument('--source', type=Path, default=Path(__file__).resolve().parents[1])
p.add_argument('--zuo', type=Path, required=True)
p.add_argument('--host-scheme', required=True, help='native command, including explicit -B arguments if needed')
p.add_argument('--machine', default='ta6le')
p.add_argument('--sessionkey', required=True, help='explicit CI seed; configure defaults remain unchanged')
p.add_argument('--seed-only', action='store_true', help='only the pair of explicitly seeded builds')
p.add_argument('--output', type=Path, help='must not exist; default is a fresh temporary directory')
a = p.parse_args()
source, zuo = a.source.resolve(), a.zuo.resolve()
if a.output:
    out = a.output.resolve(); out.mkdir(parents=True)
else:
    out = Path(tempfile.mkdtemp(prefix='chez-repro-'))
work = out / 'work'
rows = []
names = ['petite.boot','scheme.boot','scheme.h','equates.h',
         'gc-ocd.inc','gc-oce.inc','gc-par.inc','heapcheck.inc']
def save(): (out / 'results.json').write_text(json.dumps(rows, indent=2)+'\n')
def run(label, args):
    t = time.monotonic()
    with (out / (label+'.log')).open('w') as log:
        r = subprocess.run(args, cwd=work, stdout=log, stderr=subprocess.STDOUT)
    rows.append(dict(label=label, seconds=time.monotonic()-t, status=r.returncode, command=args)); save()
    assert r.returncode == 0, (label, (out / (label+'.log')).read_text()[-2000:])
for label in (['seeded-1','seeded-2'] if a.seed_only else ['seeded-1','seeded-2','default-1','default-2']):
    if work.exists(): shutil.rmtree(work)
    work.mkdir()
    run(label+'-configure', [str(source/'configure'), *(['--as-is'] if '--as-is)' in (source/'configure').read_text() else []),'--force','--disable-curses',
                            '--disable-x11','-m='+a.machine,'--workarea='+a.machine,'ZUO='+str(zuo)]
        + (['--sessionkey='+a.sessionkey] if label.startswith('seeded') else []))
    run(label+'-bootquick', [str(zuo),'.','-j','4','bootquick','--host-scheme',a.host_scheme,a.machine])
    boot = work / 'boot' / a.machine
    rows.append(dict(label=label, hashes={n: hashlib.sha256((boot/n).read_bytes()).hexdigest() for n in names})); save()
h = {r['label']:r['hashes'] for r in rows if 'hashes' in r}
assert h['seeded-1'] == h['seeded-2'], 'Explicitly seeded boot builds differed'
if not a.seed_only:
    assert {n for n in names if h['default-1'][n] != h['default-2'][n]} == {'petite.boot','scheme.boot'}
print('Explicit CI seed: 8/8 identical.' + ('' if a.seed_only else ' Default: two changing boot images.') + ' Evidence:', out)
