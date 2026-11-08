#!/usr/bin/env python3
"""Reversible regressions for an isolated, already built Racket CS checkout.

Usage: python3 tests/cs-dependencies.py --zuo /path/zuo \
    --source /isolated/racket/src --build /isolated/build --output /new/evidence
Never run mutation regressions against a checkout with edits to these inputs.
"""
from pathlib import Path
import argparse
import json
import os
import re
import subprocess
import time

p = argparse.ArgumentParser(description=__doc__)
p.add_argument('--zuo', type=Path, required=True)
p.add_argument('--source', type=Path, required=True)
p.add_argument('--build', type=Path, required=True)
p.add_argument('--output', type=Path, required=True)
a = p.parse_args()
src, work, out = a.source.resolve(), a.build.resolve(), a.output.resolve()
out.mkdir(parents=True)
driver = out / 'targets.zuo'
driver.write_text('#lang zuo\n(define ts ((dynamic-require ' + json.dumps(str(src/'cs/c/build.zuo'))
                  + " 'targets-at) (make-at-dir \"cs/c\") (hash)))\n(build/command-line ts)\n")
rows = []
def run(label, target, ok=True):
    start = time.monotonic()
    r = subprocess.run([str(a.zuo.resolve()), str(driver), '-j', '1', 'cs/c/'+target],
                       cwd=work, capture_output=True, text=True)
    (out/(label+'.stdout')).write_text(r.stdout)
    (out/(label+'.stderr')).write_text(r.stderr)
    rows.append(dict(label=label, status=r.returncode, seconds=time.monotonic()-start))
    (out/'results.json').write_text(json.dumps(rows, indent=2)+'\n')
    assert (r.returncode == 0) == ok, r.stdout + r.stderr
    if not ok:
        log = r.stdout+r.stderr
        assert ('dependency-regression' in log or 'invalid sharp-sign prefix #<' in log), log
    print(label, r.returncode, flush=True)
def mutate(label, source, target, transform):
    artifact = work/'cs/c'/target
    original, artifact_bytes = source.read_bytes(), artifact.read_bytes()
    source_stat, artifact_stat = source.stat(), artifact.stat()
    try:
        run(label+'-control', target)
        source.write_bytes(transform(original))
        os.utime(source, ns=(source_stat.st_atime_ns, source_stat.st_mtime_ns+2_000_000_000))
        # The warm build itself must detect the invalid input. No output or
        # dependency database is deleted to force the action.
        run(label+'-warm-reject', target, ok=False)
    finally:
        source.write_bytes(original)
        os.utime(source, ns=(source_stat.st_atime_ns, source_stat.st_mtime_ns))
        artifact.write_bytes(artifact_bytes)
        artifact.chmod(artifact_stat.st_mode & 0o777)
        os.utime(artifact, ns=(artifact_stat.st_atime_ns, artifact_stat.st_mtime_ns))
for label, relative, target in [
    ('convert-boot', 'cs/c/convert-to-boot.ss', 'racket.boot'),
    ('same-boot', 'cs/c/same-boot.ss', 'check-bootstrap-same-stamp'),
    ('to-vfasl', 'cs/c/to-vfasl.ss', 'petite-v.boot'),
    ('api-header', 'cs/c/api.h', 'boot.o')]:
    error = b'\n#error dependency-regression-header\n' if relative.endswith('.h') else b'\n#<dependency-regression-script>\n'
    mutate(label, src/relative, target, lambda b, error=error: b+error)
for key, target, option in [('CFLAGS', 'boot.o', '-fdependency-regression-CFLAGS'),
                            ('CPPFLAGS', 'boot.o', '-fdependency-regression-CPPFLAGS'),
                            ('LDFLAGS', 'raw_racketcs', '-Wl,--dependency-regression-LDFLAGS')]:
    def change(b, key=key, option=option):
        updated, count = re.subn(rb'(?m)^('+key.encode()+rb'\s*=.*)$',
                                 lambda m: m[0]+b' '+option.encode(), b)
        assert count == 1, key
        return updated
    mutate(key.lower(), work/'cs/c/Makefile', target, change)
print('Racket CS source, header and command-configuration regressions passed')
