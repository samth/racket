#!/usr/bin/env python3
"""POSIX integration tests for the opt-in command-rule prototype.

Usage: python3 tests/action-cache.py /absolute/path/to/zuo
Python is only a test dependency; the cache implementation is Zuo.
"""
from pathlib import Path
import concurrent.futures
import os
import shutil
import subprocess
import sys
import tempfile

if os.name != 'posix':
    raise SystemExit('Prototype cache tests require POSIX')
TESTS = Path(__file__).resolve().parent
LIB = TESTS.parent / 'lib'
EXE = Path(sys.argv[1]).resolve()
ROOT = Path(tempfile.mkdtemp(prefix='zuo-action-cache-'))
FIX = ROOT / 'fixtures'
FIX.mkdir()
for name in ['action-cache-driver.zuo', 'action-cache-producer.zuo']:
    shutil.copyfile(TESTS / name, FIX / name)
CACHE = ROOT / 'cache'
WORK = ROOT / 'work'
WORK.mkdir()
(WORK / 'input').write_text('input-one')
env = os.environ.copy()
env.update(ZUO_ACTION_CACHE=str(CACHE), ZUO_ACTION_CACHE_LOG='1')

def run(*variables, work=WORK, ok=True):
    p = subprocess.run([str(EXE), '-X', str(LIB), str(FIX / 'action-cache-driver.zuo'),
                        'LIB=' + str(LIB), *variables], cwd=work, env=env,
                       capture_output=True, text=True)
    if ok: assert p.returncode == 0, p.stdout + p.stderr
    else: assert p.returncode != 0, 'Expected failure'
    return p.stdout + p.stderr

def count(work=WORK):
    return (work / 'runs').read_text().count('run\n') if (work / 'runs').exists() else 0

def fresh(work=WORK):
    for name in ['a', 'b', '_zuo.db', '_zuo_tc.db']:
        (work / name).unlink(missing_ok=True)

try:
    assert 'stored' in run()
    assert count() == 1
    run()
    assert count() == 1
    fresh()
    assert 'hit' in run()
    assert count() == 1 and (WORK / 'b').exists()
    # Missing co-output, with existing database, must restore the whole group.
    (WORK / 'b').unlink()
    assert 'hit' in run() and count() == 1
    # Command configuration, environment, toolchain identity, and source changes.
    run('VALUE=two'); assert count() == 2
    run('VALUE=two', 'ENV=other'); assert count() == 3
    run('VALUE=two', 'ENV=other', 'TOOLCHAIN=second'); assert count() == 4
    (WORK / 'input').write_text('source-change-longer')
    run(); assert count() == 5
    # Producer implementation is a dependency, not an opaque closure.
    producer = FIX / 'action-cache-producer.zuo'
    producer.write_text(producer.read_text() + '\n; producer revision\n')
    run(); assert count() == 6
    # Actual executable bytes change (a harmless ELF trailer), not just a tag.
    tool = ROOT / 'tool'
    shutil.copy2(EXE, tool)
    run('TOOL=' + str(tool)); assert count() == 7
    with tool.open('ab') as out: out.write(b'\ncache-test-trailer\n')
    run('TOOL=' + str(tool)); assert count() == 8
    # Storage corruption and malformed metadata fall back to execution.
    fresh()
    for blob in (CACHE / 'blobs').iterdir(): blob.write_bytes(b'corrupt')
    assert 'rejected' in run() and count() == 9
    fresh()
    for action in (CACHE / 'actions').iterdir(): action.write_text('malformed\n')
    run(); assert count() == 10
    # Executable mode survives publication and restoration.
    run('MODE=executable'); assert count() == 11
    assert (WORK / 'a').stat().st_mode & 0o111
    fresh()
    assert 'hit' in run('MODE=executable') and count() == 11
    assert (WORK / 'a').stat().st_mode & 0o111
    # Stale co-outputs must not make an incomplete producer look successful.
    before = set((CACHE / 'actions').iterdir())
    run('FAIL=incomplete', ok=False)
    assert set((CACHE / 'actions').iterdir()) == before
    # Failed, incomplete, and input-mutating actions never publish an entry.
    for failure in ['yes', 'incomplete', 'mutate']:
        fresh()
        before = set((CACHE / 'actions').iterdir())
        run('FAIL=' + failure, ok=False)
        assert set((CACHE / 'actions').iterdir()) == before
    # Symlink identity matters even when the referenced bytes are equal.
    fresh()
    (WORK / 'input').unlink()
    (WORK / 'source-a').write_text('identical')
    (WORK / 'source-b').write_text('identical')
    (WORK / 'input').symlink_to('source-a')
    previous = count(); run(); assert count() == previous + 1
    (WORK / 'input').unlink(); (WORK / 'input').symlink_to('source-b')
    # Remove incremental DBs: this specifically tests artifact identity.
    fresh(); previous = count(); run(); assert count() == previous + 1
    # The same command rules remain usable with storage disabled.
    before = set((CACHE / 'actions').iterdir())
    env.pop('ZUO_ACTION_CACHE')
    fresh()
    previous = count(); run(); assert count() == previous + 1
    assert set((CACHE / 'actions').iterdir()) == before
    env['ZUO_ACTION_CACHE'] = str(CACHE)
    # Several processes publish identical blobs under distinct action keys.
    dirs = []
    for i in range(6):
        d = ROOT / ('parallel-' + str(i)); d.mkdir(); (d / 'input').write_text('shared')
        dirs.append(d)
    with concurrent.futures.ThreadPoolExecutor(max_workers=6) as pool:
        list(pool.map(lambda d: run(work=d), dirs))
    for d in dirs:
        fresh(d); assert 'hit' in run(work=d) and count(d) == 1
    print('action-cache integration checks passed:', ROOT)
finally:
    # Retain diagnostics on failure; successful runs can be removed by caller.
    pass
