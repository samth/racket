#!/usr/bin/env python3
"""Test native fixpoint action caching with a real supplied native compiler.

Uses copied sources, compiler and seed boots; never edits the supplied paths.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import time

p = argparse.ArgumentParser(description=__doc__)
p.add_argument('--zuo', type=Path, required=True)
p.add_argument('--source', type=Path, required=True)
p.add_argument('--compiler', type=Path, required=True)
p.add_argument('--boots', type=Path, required=True)
p.add_argument('--machine', default='ta6le')
p.add_argument('--output', type=Path, required=True)
a = p.parse_args()
root = a.output.resolve()
root.mkdir(parents=True)
source = root / 'source'
shutil.copytree(a.source, source, ignore=shutil.ignore_patterns('.git', '_zuo*.db'))
compiler = root / 'compiler/bin' / a.machine / 'scheme'
boots = root / 'compiler/boot' / a.machine
compiler.parent.mkdir(parents=True)
boots.mkdir(parents=True)
shutil.copy2(a.compiler, compiler)
for name in ['petite.boot', 'scheme.boot']:
    shutil.copy2(a.boots / name, boots / name)
work = root / 'work'
template = ('m=' + a.machine + '\nhostm=' + a.machine + '\nhostworkarea=../compiler\n'
            'SCHEMEHEAPDIRS=' + str(boots) + '\n'
            'sessionKey=native-cache-regression\ncacheBootstrap=yes\n')
driver = root / 'driver.zuo'
driver.write_text('#lang zuo\n(build/command-line ((dynamic-require '
                  + json.dumps(str(source / 's/build.zuo'))
                  + ' \'targets-at) (make-at-dir "work/s")))\n')
env = dict(os.environ, ZUO_CACHE_ACTIONS='1', ZUO_ACTION_TOOLCHAIN_ID='native-regression-sdk',
           ZUO_ACTION_CACHE=str(root / 'cache'), ZUO_ACTION_CACHE_LOG='1')
rows = []

def fresh():
    if work.exists():
        shutil.rmtree(work)
    (work / 's').mkdir(parents=True)
    (work / 'Mf-config').write_text(template)

def run(label):
    start = time.monotonic()
    result = subprocess.run([str(a.zuo.resolve()), str(driver), 'work/s/bootstrap'],
                            cwd=root, env=env, capture_output=True, text=True)
    (root / (label + '.stdout')).write_text(result.stdout)
    (root / (label + '.stderr')).write_text(result.stderr)
    log = result.stdout + result.stderr
    assert result.returncode == 0, log[-5000:]
    row = dict(label=label, seconds=time.monotonic() - start,
               hits=log.count('action cache hit '), stored=log.count('action cache stored '))
    rows.append(row)
    (root / 'results.json').write_text(json.dumps(rows, indent=2) + '\n')
    print(json.dumps(row), flush=True)
    return row

def hashes():
    return {str(p.relative_to(work)): hashlib.sha256(p.read_bytes()).hexdigest()
            for p in work.rglob('*') if p.is_file() and p.name not in
            ['_zuo.db', '_zuo_tc.db', 'Mf-config']}

fresh()
assert run('cold')['stored'] == 1
reference = hashes()
assert len(reference) > 80, len(reference)
fresh()
assert run('empty-tree')['hits'] == 1
assert hashes() == reference
(work / 'boot' / a.machine / 'equates.h').unlink()
assert run('missing-child')['hits'] == 1
assert hashes() == reference
# Source contents matter even if an input retains its timestamp and size.
layout = source / 's/layout.ss'
original = layout.read_bytes()
old = layout.stat()
changed = original.replace(b'Copyright', b'CopyrighT', 1)
assert changed != original and len(changed) == len(original)
layout.write_bytes(changed)
os.utime(layout, ns=(old.st_atime_ns, old.st_mtime_ns))
assert run('source-change')['stored'] == 1
layout.write_bytes(original)
assert run('source-revert')['hits'] == 1
assert hashes() == reference
print('native bootstrap command cache checks passed:', root)
