#!/usr/bin/env python3
"""Check copied-header outputs and narrow kernel dependencies in an isolated build."""
from pathlib import Path
import argparse
import hashlib
import json
import os
import shutil
import subprocess
import tempfile

p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--zuo',type=Path,required=True)
p.add_argument('--source',type=Path,required=True)
p.add_argument('--workarea',type=Path,required=True)
p.add_argument('--output',type=Path,required=True)
p.add_argument('--driver',type=Path,help='optional adapter for embedded workareas')
a=p.parse_args();src=a.source.resolve();work=a.workarea.resolve();out=a.output.resolve();out.mkdir(parents=True)
rows=[]
def objects():return {p.name:p.stat().st_mtime_ns for p in (work/'c').glob('*.o')}
def run(label,target='libkernel.a'):
    r=subprocess.run([str(a.zuo.resolve()),str(a.driver.resolve()) if a.driver else '.','-j','4',target],cwd=work/'c',capture_output=True,text=True)
    (out/(label+'.log')).write_text(r.stdout+r.stderr)
    assert r.returncode==0,r.stdout+r.stderr
    return r
run('control')
with tempfile.TemporaryDirectory(prefix='compression-dependencies-') as temp:
    backup=Path(temp)/'work';shutil.copytree(work,backup,symlinks=True)
    def restore():
        shutil.rmtree(work);shutil.copytree(backup,work,symlinks=True)
    def record(label,changed):
        rows.append(dict(label=label,changed_objects=sorted(changed)))
        (out/'results.json').write_text(json.dumps(rows,indent=2)+'\n')
        print(label,sorted(changed),flush=True)
    for relative in ['lz4/lib/lz4.h','zlib/zconf.h']:
        restore();header=work/relative
        if not header.is_file():continue  # system compression library
        before=objects();header.unlink()
        try:
            run('missing-'+header.name,'new-io.o')
            assert header.is_file()
            changed=[n for n,t in objects().items() if before[n]!=t]
            record('missing-'+header.name,changed);assert changed==[],changed
        finally:restore()
    # A compression implementation edit changes its archive, not C headers.
    implementation=src/'lz4/lib/lz4.c';original=implementation.read_bytes();st=implementation.stat()
    restore();before=objects();archive=work/'lz4/lib/liblz4.a';sha=hashlib.sha256(archive.read_bytes()).hexdigest()
    try:
        implementation.write_bytes(original+b'\nint zuo_dependency_regression_marker(void);\nint zuo_dependency_regression_marker(void) { return 7; }\n')
        os.utime(implementation,ns=(st.st_atime_ns,st.st_mtime_ns+2_000_000_000))
        run('lz4-implementation')
        assert hashlib.sha256(archive.read_bytes()).hexdigest()!=sha
        changed=[n for n,t in objects().items() if before[n]!=t]
        record('lz4-implementation',changed);assert changed==[],changed
    finally:
        implementation.write_bytes(original);os.utime(implementation,ns=(st.st_atime_ns,st.st_mtime_ns));restore()
    for relative,expected in [('lz4/lib/lz4frame.h',['compress-io.o']),
                              ('c/sort.h',['gc-011.o','gc-ocd.o','gc-oce.o','gc-par.o','prim5.o','segment.o'])]:
        restore();header=src/relative;original=header.read_bytes();st=header.stat();before=objects()
        try:
            header.write_bytes(original+b'\n/* dependency precision regression */\n')
            os.utime(header,ns=(st.st_atime_ns,st.st_mtime_ns+2_000_000_000))
            run('header-'+header.name)
            changed=[n for n,t in objects().items() if before[n]!=t]
            record('header-'+header.name,changed);assert sorted(changed)==expected,changed
        finally:
            header.write_bytes(original);os.utime(header,ns=(st.st_atime_ns,st.st_mtime_ns));restore()
print('Copied-header and compression dependency regressions passed')
