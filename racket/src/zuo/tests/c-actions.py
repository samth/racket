#!/usr/bin/env python3
"""Real compiler/archive/link cache reuse and invalidation, no build snapshots."""
from pathlib import Path
import os
import shutil
import subprocess
import sys
import tempfile

tests = Path(__file__).resolve().parent
lib = tests.parent / 'lib'
zuo = Path(sys.argv[1]).resolve()
root = Path(tempfile.mkdtemp(prefix='zuo-c-actions-'))
work = root / 'work'
work.mkdir()
driver = root / 'c-actions-driver.zuo'
shutil.copyfile(tests / driver.name, driver)
env = dict(os.environ, ZUO_ACTION_CACHE=str(root / 'cache'), ZUO_ACTION_CACHE_LOG='1',
           ZUO_ACTION_TOOLCHAIN_ID='test-sdk-v1', ZUO_ACTION_COVERAGE_LOG='1')
(work / 'value.c').write_text('#include "value.h"\nint value(void) { return VALUE; }\n')
(work / 'value.h').write_text('#define VALUE 42\n')
(work / 'main.c').write_text('int value(void); int main(void) { return value() != 42; }\n')
outputs = ['main', 'main.o', 'value.o', 'libvalue.a']

def run(*variables, ok=True):
    p = subprocess.run([str(zuo), '-X', str(lib), str(driver), *variables, 'all'],
                       cwd=work, env=env, capture_output=True, text=True)
    log = p.stdout + p.stderr
    assert (p.returncode == 0) == ok, log
    return log

def fresh():
    for name in outputs + ['_zuo.db', '_zuo_tc.db']:
        (work / name).unlink(missing_ok=True)

assert run().count('action cache stored ') == 4
reference = {name: (work / name).read_bytes() for name in outputs}
fresh()
assert run().count('action cache hit ') == 4
assert {name: (work / name).read_bytes() for name in outputs} == reference
subprocess.run([str(work / 'main')], check=True)
(work / 'value.h').write_text('#define VALUE 43\n')
log = run()
assert log.count('action cache stored ') == 3, log  # object, archive, link
assert subprocess.run([str(work / 'main')]).returncode == 1
fresh()
assert run().count('action cache hit ') == 4
log = run('CFLAGS=-O0')
assert log.count('action cache stored ') == 4, log
# Timestamp identity is not content identity, including incremental calls.
old = (work / 'value.h').stat()
(work / 'value.h').write_text('#define VALUE 42\n')
os.utime(work / 'value.h', ns=(old.st_atime_ns, old.st_mtime_ns))
run('CFLAGS=-O0')
subprocess.run([str(work / 'main')], check=True)
env['ZUO_ACTION_TOOLCHAIN_ID'] = 'test-sdk-v2'
fresh()
assert run().count('action cache stored ') == 4
for flag in ['-MD', '-gsplit-dwarf', '--coverage', '@args', '-include hidden.h', '; echo unsafe']:
    fresh()
    before = set((root / 'cache/actions').iterdir())
    run('CFLAGS=' + flag, ok=False)
    assert set((root / 'cache/actions').iterdir()) == before
print('C command cache checks passed:', root)
