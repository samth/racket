#!/usr/bin/env python3
"""Real GCC sandbox/cache regression. Linux with static libc, bwrap and strace.

The uncached fixture records a tiny, known compiler invocation to construct its
explicit SDK manifest. This is test setup, not production dependency discovery:
workspace files are declared separately; the strict action never learns inputs.
Run in a writable directory outside /tmp, with the Zuo executable as argv[1].
"""
from pathlib import Path
import ast
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
CODE = runpy.run_path(str(RUNNER))
EXE = Path(sys.argv[1]).resolve()
CC = '/usr/bin/cc'
BWRAP = shutil.which('bwrap')
STRACE = shutil.which('strace')
assert sys.platform == 'linux' and BWRAP and STRACE
assert b'Free Software Foundation' in subprocess.check_output([CC, '--version']), 'GCC fixture'
with tempfile.TemporaryDirectory(prefix='.zuo-sandbox-cc-', dir=Path.cwd()) as temp:
    root = Path(temp)
    work = root / 'work'
    work.mkdir()
    source = work / 'input.c'
    header = work / 'value.h'
    source.write_text('#include "value.h"\nint value(void) { return VALUE; }\n')
    header.write_text('#define VALUE 42\n')
    output = work / 'value.o'
    host_trace = root / 'host.trace'
    action_env = {'LC_ALL': 'C', 'PATH': '/usr/bin:/bin'}
    subprocess.run([STRACE, '-f', '-qq', '-yy', '-s', '65536', '-e',
                    'trace=%file,%process,%network,getdents64,io_uring_setup',
                    '-o', str(host_trace), CC, '-nostdinc', '-c', 'input.c', '-o', str(output)],
                   cwd=work, env=action_env, check=True)
    reference = output.read_bytes()
    files, directories, absent = set(), set(), set()
    for pid, name, text, result in CODE['trace_calls'](host_trace):
        quoted = CODE['QUOTED'].search(text)
        if not quoted:
            continue
        p = ast.literal_eval(quoted[0])
        if not p.startswith(('/usr/', '/lib/', '/lib64/', '/etc/')):
            continue  # Never infer source inputs from the host trace.
        p = os.path.normpath(p)
        if os.path.isfile(p) and name not in ('readlink', 'readlinkat'):
            files.add(p)
        elif os.path.isdir(p):
            directories.add(p)
        elif not os.path.lexists(p):
            absent.add(p)
    for p in list(files):
        if os.stat(p).st_mode & 0o111:
            loader = CODE['interpreter'](p)
            if loader:
                files.add(loader)
    # GCC probes these source paths for optional precompiled headers.
    absent.update([str(source) + '.gch', str(header) + '.gch'])
    sdk = root / 'sdk.json'
    def write_sdk():
        sdk.write_text(json.dumps(dict(version=1, files=sorted(files - {CC}),
                                       directories=sorted(directories), absent=sorted(absent))))
    write_sdk()
    launcher = root / 'launcher'
    subprocess.run([CC, '-static', '-O2', '-o', str(launcher),
                    str(RUNNER.with_name('action-sandbox-launcher.c'))], check=True)
    driver = root / 'action-sandbox-cc-driver.zuo'
    shutil.copyfile(TESTS / driver.name, driver)
    reports = root / 'reports'
    cache = root / 'cache'
    env = dict(os.environ, ZUO_ACTION_CACHE=str(cache), ZUO_ACTION_CACHE_LOG='1',
               ZUO_ACTION_SANDBOX_REPORT_DIR=str(reports))
    defaults = ['CC=' + CC, 'PYTHON=' + sys.executable, 'RUNNER=' + str(RUNNER),
                'BWRAP=' + BWRAP, 'STRACE=' + STRACE, 'LAUNCHER=' + str(launcher),
                'SDK=' + str(sdk)]
    def run(*variables, ok=True):
        p = subprocess.run([str(EXE), '-X', str(LIB), str(driver), *defaults, *variables],
                           cwd=work, env=env, capture_output=True, text=True)
        message = p.stdout + p.stderr
        assert (p.returncode == 0) == ok, message
        return message
    def fresh():
        for name in ['value.o', '_zuo.db', '_zuo_tc.db']:
            (work / name).unlink(missing_ok=True)
    fresh()
    assert 'stored' in run()
    assert output.read_bytes() == reference, 'sandbox and host object bytes differ'
    assert json.loads(next(reports.glob('*.json')).read_text())['unused'] == []
    fresh()
    assert 'hit' in run() and output.read_bytes() == reference
    # A header omitted from the rule is hidden, then diagnosed. No publication.
    fresh()
    before = set((cache / 'actions').iterdir())
    assert '/value.h' in run('HEADER=no', ok=False)
    assert not output.exists() and set((cache / 'actions').iterdir()) == before
    # Missing negative declarations are rejected even if GCC still succeeds.
    absent.remove(str(header) + '.gch')
    write_sdk()
    assert '/value.h.gch' in run(ok=False)
    absent.add(str(header) + '.gch')
    write_sdk()
    assert 'hit' in run()
    # A real source mutation invalidates the entry and matches a fresh compiler.
    header.write_text('#define VALUE 43\n')
    assert 'stored' in run()
    changed = output.read_bytes()
    assert changed != reference
    subprocess.run([CC, '-nostdinc', '-c', 'input.c', '-o', str(output)], cwd=work,
                   env=action_env, check=True)
    assert output.read_bytes() == changed
    # Even an incremental hit must reject a newly present optional PCH.
    Path(str(header) + '.gch').write_text('new optional input')
    assert 'declared absent path exists' in run(ok=False)
print('real GCC sandbox/cache checks passed')
