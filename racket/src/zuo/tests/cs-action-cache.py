#!/usr/bin/env python3
"""Measure the CS command-rule pilot with a prebuilt native Chez host.

Fixed roots, static schemified sources, trusted local CAS. Does not measure a
whole Racket/PB bootstrap, package compilation, or CI. Python is test-only.
"""
from pathlib import Path
import argparse
import hashlib
import json
import os
import shutil
import subprocess
import tempfile
import time

p = argparse.ArgumentParser(description=__doc__)
p.add_argument('--zuo', type=Path, required=True)
p.add_argument('--native', type=Path, required=True, help='native workarea containing bin/ta6le and boot/ta6le')
p.add_argument('--source', type=Path, default=Path(__file__).resolve().parents[2])
p.add_argument('--machine', default='ta6le')
p.add_argument('--toolchain-id', required=True, help='identity of this fixed runner/platform/sysroot/toolchain')
p.add_argument('--output', type=Path, help='must not exist')
a = p.parse_args()
source, zuo, host = a.source.resolve(), a.zuo.resolve(), a.native.resolve()
if a.output:
    out = a.output.resolve(); out.mkdir(parents=True)
else: out = Path(tempfile.mkdtemp(prefix='cs-action-cache-'))
work = out / 'work'; work.mkdir()
# JSON string quoting matches these Scheme path string literals.
q = lambda path: json.dumps(str(path))
boots = [host/'boot'/a.machine/(name+'.boot') for name in ['petite','scheme']]
driver = out / 'main.zuo'
driver.write_text('''#lang zuo
(require SOURCE_LIB)
(define cs-targets-at (dynamic-require SOURCE_CS 'targets-at))
(define (targets-at at-dir vars)
  (cs-targets-at at-dir
    (hash-set* vars 'SCHEME SCHEME
      'scheme-args (list "-B" BOOT1 "-B" BOOT2)
      'schemify "f" 'UNSAFE_COMP "yes" 'COMPRESS_COMP "--compress" 'DISABLE_WPO "f")
    (list BOOT1 BOOT2)))
(build/command-line* targets-at)
'''.replace('SOURCE_LIB',q(source/'lib.zuo')).replace('SOURCE_CS',q(source/'cs/main.zuo'))
   .replace('SCHEME\n',q(host/'bin'/a.machine/'scheme')+'\n').replace('BOOT1',q(boots[0])).replace('BOOT2',q(boots[1])))
env = dict(os.environ, ZUO_CS_ACTIONS='1', ZUO_CS_TOOLCHAIN_ID=a.toolchain_id,
           ZUO_ACTION_CACHE=str(out/'cache'), ZUO_ACTION_CACHE_LOG='1')
rows = []
def run(label):
    start = time.monotonic()
    r = subprocess.run([str(zuo),'-X',str(source/'zuo/lib'),str(driver),'-j','4'],cwd=work,env=env,capture_output=True,text=True)
    log = r.stdout+r.stderr; (out/(label+'.log')).write_text(log)
    rows.append(dict(label=label,seconds=time.monotonic()-start,status=r.returncode,
                     cache_hits=log.count('action cache hit '),cache_stores=log.count('action cache stored ')))
    (out/'results.json').write_text(json.dumps(rows,indent=2)+'\n')
    assert r.returncode==0,log[-2000:]
def manifest(): return {p.name:hashlib.sha256(p.read_bytes()).hexdigest() for p in work.iterdir() if p.suffix in ['.so','.wpo']}
run('cold'); before=manifest(); assert len(before)==19 and rows[-1]['cache_stores']==10
run('warm'); assert rows[-1]['cache_stores']==0
shutil.rmtree(work);work.mkdir();run('restore');assert manifest()==before and rows[-1]['cache_hits']==10
(work/'main.wpo').unlink();run('missing-wpo');assert manifest()==before and rows[-1]['cache_hits']==1
(work/'chezpart.wpo').write_bytes(b'bad-wpo');run('corrupt-wpo');assert manifest()==before
(out/'outputs.json').write_text(json.dumps(before,indent=2)+'\n')
print(json.dumps(rows,indent=2));print('Evidence:',out)
