#!/usr/bin/env python3
"""Linux integration tests; requires cc with static libc, bubblewrap and strace.

Run from a writable directory outside /tmp:
  python3 tests/action-sandbox.py /absolute/path/to/zuo
Missing sandbox capabilities are test failures, never an unsandboxed fallback.
"""
from pathlib import Path
import json
import os
import runpy
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
    # Bubblewrap's private PID-namespace reaper may be killed during exit.
    # Accept a cancelled exit, but never discard an unfinished file syscall.
    parse_trace = runpy.run_path(str(RUNNER))['trace_calls']
    terminal = root / 'terminal.trace'
    terminal.write_text('1 exit_group(0 <unfinished ...>\n1 +++ killed by SIGKILL +++\n')
    assert list(parse_trace(terminal)) == [(1, '__killed__', '', '')]
    terminal.write_text('1 openat(AT_FDCWD, "hidden", O_RDONLY <unfinished ...>\n1 +++ killed by SIGKILL +++\n')
    try:
        list(parse_trace(terminal))
    except ValueError:
        pass
    else:
        raise AssertionError('unfinished file access was accepted')
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
    for mode in ['missing', 'child', 'network', 'write-input', 'symlink', 'incomplete', 'fail', 'abort']:
        fresh()
        before = set((cache / 'actions').iterdir())
        message = run('MODE=' + mode, ok=False)
        assert set((cache / 'actions').iterdir()) == before, mode
        assert not (work / 'a').exists() and not (work / 'b').exists(), mode
        assert (work / 'input').read_text() == 'declared input\n'
        if mode in ['missing', 'child']:
            assert 'undeclared probe:' in message and '/hidden' in message, message
        if mode == 'abort':
            assert report()['exit_status'] != 0
            assert report()['violations'] == []
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
    # Literal compilation programs can be delivered from a hashed file. The
    # environment channel remains separate and the action sees exactly stdin.
    sdk.write_text(json.dumps({'version': 1, 'files': [], 'absent': []}))
    program = work / 'program'
    program.write_text('first stdin\n')
    fresh()
    assert 'stored' in run('MODE=stdin', 'STDIN=' + str(program))
    assert (work / 'a').read_text() == 'declared input\nfirst stdin\n'
    assert 'hit' in run('MODE=stdin', 'STDIN=' + str(program))
    program.write_text('second stdin\n')
    # The strict plan rehashes even when mtimes are indistinguishable.
    assert 'stored' in run('MODE=stdin', 'STDIN=' + str(program))
    assert (work / 'a').read_text().endswith('second stdin\n')
    # Presence is a distinct contract: changing bytes does not invalidate it,
    # but opening those bytes (even tolerantly) rejects cache publication.
    optional = work / 'optional'
    optional.write_text('metadata is not content\n')
    sdk.write_text(json.dumps({'version': 1, 'files': [], 'absent': [],
                              'metadata': [str(optional)]}))
    fresh()
    assert 'stored' in run('MODE=metadata')
    assert str(optional) in report()['metadata_only']
    optional.write_text('changed bytes\n')
    assert 'hit' in run('MODE=metadata')
    fresh()
    assert 'content read of file-presence input:' in run('MODE=metadata-read', ok=False)
    optional.unlink()
    assert 'helper failed' in run('MODE=metadata', ok=False)
    sdk.write_text(json.dumps({'version': 1, 'files': [], 'absent': []}))
    fresh()
    run('MODE=retained')
    assert report()['undeclared_outputs'] == [str(work / 'unlisted-header.h')]
    assert not (work / 'unlisted-header.h').exists()
    fresh()
    before = set((cache / 'actions').iterdir())
    assert 'undeclared retained output:' in run('MODE=retained', 'OUTPUTS=error', ok=False)
    assert before == set((cache / 'actions').iterdir())
    assert not (work / 'a').exists()
    fresh()
    run('MODE=proc')
    fresh()
    assert '/proc/1/environ' in run('MODE=proc-other', ok=False)
    # CLONE_FS shares cwd, not the executable identity or namespace process ID.
    # A child exec must not change how the parent's procfs alias is audited.
    child = producer.with_name(producer.name + '-child')
    shutil.copyfile(producer, child)
    child.chmod(0o755)
    sdk.write_text(json.dumps({'version': 1, 'files': [str(child)], 'absent': []}))
    fresh()
    run('MODE=clone-exec')
    sdk.write_text(json.dumps({'version': 1, 'files': [], 'absent': []}))
    # Entropy is an explicit input. Repeated opens advance one stream, the
    # cache can reuse that stream's result, and seed changes invalidate it.
    entropy = work / 'entropy-seed'
    entropy.write_bytes(b'first explicit build seed')
    fresh()
    assert 'stored' in run('MODE=entropy', 'ENTROPY=' + str(entropy))
    first_entropy_output = (work / 'a').read_bytes()
    assert report()['entropy']['bytes_consumed'] == 64
    assert str(entropy) in report()['used']
    assert 'hit' in run('MODE=entropy', 'ENTROPY=' + str(entropy))
    fresh()
    assert 'hit' in run('MODE=entropy', 'ENTROPY=' + str(entropy))
    assert (work / 'a').read_bytes() == first_entropy_output
    entropy.write_bytes(b'second explicit build seed')
    assert 'stored' in run('MODE=entropy', 'ENTROPY=' + str(entropy))
    assert (work / 'a').read_bytes() != first_entropy_output
    # Reexecution, not cache restoration, must reproduce the same bytes too.
    saved_cache = env.pop('ZUO_ACTION_CACHE')
    fresh()
    run('MODE=entropy', 'ENTROPY=' + str(entropy))
    second_entropy_output = (work / 'a').read_bytes()
    fresh()
    run('MODE=entropy', 'ENTROPY=' + str(entropy))
    assert (work / 'a').read_bytes() == second_entropy_output
    env['ZUO_ACTION_CACHE'] = saved_cache
    # Disabling caching still enforces the boundary.
    env.pop('ZUO_ACTION_CACHE')
    fresh()
    assert 'undeclared probe:' in run('MODE=missing', ok=False)
    run()
    fresh()
    assert 'helper failed' in run('STRACE=/bin/false', ok=False)
print('action-sandbox integration checks passed')
