#!/usr/bin/env python3
"""Linux integration tests; requires cc with static libc, bubblewrap and strace.

Run from a writable directory outside /tmp:
  python3 tests/action-sandbox.py /absolute/path/to/zuo
Missing sandbox capabilities are test failures, never an unsandboxed fallback.
"""
from pathlib import Path
import json
import os
import shutil
import subprocess
import sys
import tempfile

TESTS = Path(__file__).resolve().parent
LIB = TESTS.parent / 'lib'
RUNNER = LIB / 'zuo/private/action-sandbox.py'
EXE = Path(sys.argv[1]).resolve()
BWRAP = shutil.which('bwrap')
STRACE = shutil.which('strace')
assert sys.platform == 'linux' and BWRAP and STRACE, 'Linux, bubblewrap and strace required'
with tempfile.TemporaryDirectory(prefix='.zuo-sandbox-test-', dir=Path.cwd()) as temp:
    root = Path(temp)
    work = root / 'work'
    work.mkdir()
    producer = root / 'producer'
    subprocess.run(['cc', '-static', '-O2', '-o', str(producer),
                    str(TESTS / 'action-sandbox-producer.c')], check=True)
    launcher = root / 'launcher'
    subprocess.run(['cc', '-static', '-O2', '-o', str(launcher),
                    str(RUNNER.with_name('action-sandbox-launcher.c'))], check=True)
    driver = root / 'action-sandbox-driver.zuo'
    shutil.copyfile(TESTS / 'action-sandbox-driver.zuo', driver)
    (work / 'input').write_text('declared input\n')
    (work / 'extra').write_text('unused\n')
    (work / 'hidden').write_text('must not leak\n')
    sdk = root / 'sdk.json'
    sdk.write_text(json.dumps({'version': 1, 'files': [], 'absent': []}))
    reports = root / 'reports'
    cache = root / 'cache'
    env = dict(os.environ, ZUO_ACTION_CACHE=str(cache), ZUO_ACTION_CACHE_LOG='1',
               ZUO_ACTION_SANDBOX_REPORT_DIR=str(reports))
    defaults = ['TOOL=' + str(producer), 'PYTHON=' + sys.executable,
                'RUNNER=' + str(RUNNER), 'BWRAP=' + BWRAP, 'STRACE=' + STRACE,
                'LAUNCHER=' + str(launcher),
                'SDK=' + str(sdk)]

    def report():
        return json.loads(max(reports.glob('*.json'), key=lambda p: p.stat().st_mtime_ns).read_text())

    def run(*variables, ok=True):
        p = subprocess.run([str(EXE), '-X', str(LIB), str(driver), *defaults, *variables],
                           cwd=work, env=env, capture_output=True, text=True)
        message = p.stdout + p.stderr
        assert (p.returncode == 0) == ok, message
        return message

    def fresh():
        for name in ['a', 'b', '_zuo.db', '_zuo_tc.db']:
            (work / name).unlink(missing_ok=True)

    assert 'stored' in run()
    assert (work / 'a').read_text() == 'declared input\n'
    observation = report()
    assert observation['unused'] == observation['violations'] == []
    assert 'hit' in run()  # strict validation runs even with an incremental DB
    env['ZUO_ACTION_SANDBOX_REPORT_DIR'] = str(root / 'other-reports')
    assert 'hit' in run()  # diagnostic settings are not action inputs
    assert not (root / 'other-reports').exists()
    env['ZUO_ACTION_SANDBOX_REPORT_DIR'] = str(reports)
    (work / 'b').unlink()
    assert 'hit' in run() and (work / 'b').exists()
    # Tolerated missing reads and subprocess reads still invalidate the action.
    for mode in ['missing', 'child', 'network', 'write-input', 'symlink', 'incomplete', 'fail']:
        fresh()
        before = set((cache / 'actions').iterdir())
        message = run('MODE=' + mode, ok=False)
        assert set((cache / 'actions').iterdir()) == before, mode
        assert not (work / 'a').exists() and not (work / 'b').exists(), mode
        assert (work / 'input').read_text() == 'declared input\n'
        if mode in ['missing', 'child']:
            assert 'undeclared probe:' in message and '/hidden' in message, message
    fresh()
    assert 'unused declared input:' in run('EXTRA=yes', ok=False)
    assert not (work / 'a').exists()
    assert 'stored' in run('EXTRA=yes', 'UNUSED=report')
    assert report()['unused'] == [str(work / 'extra')]
    # An audit-only cache entry cannot satisfy strict policy.
    fresh()
    assert 'unused declared input:' in run('EXTRA=yes', ok=False)
    # Explicit negative dependencies are revalidated BEFORE an incremental hit.
    sdk.write_text(json.dumps({'version': 1, 'files': [], 'absent': [str(work / 'optional')]}))
    assert 'stored' in run('MODE=optional')
    assert 'hit' in run('MODE=optional')
    (work / 'optional').write_text('new input')
    assert 'declared absent path exists' in run('MODE=optional', ok=False)
    (work / 'optional').unlink()
    # Disabling caching still enforces the boundary.
    env.pop('ZUO_ACTION_CACHE')
    fresh()
    assert 'undeclared probe:' in run('MODE=missing', ok=False)
    run()
    fresh()
    assert 'helper failed' in run('STRACE=/bin/false', ok=False)
print('action-sandbox integration checks passed')
