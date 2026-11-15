#!/usr/bin/env python3
"""Real-compiler invalidation checks for opt-in bundled Chez command actions.

Copies sources and an existing PB compiler into a new isolated directory.
The source checkout and supplied compiler/boots are never modified.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess

p = argparse.ArgumentParser(description=__doc__)
p.add_argument('--zuo', type=Path, required=True)
p.add_argument('--source', type=Path, required=True, help='bundled ChezScheme directory')
p.add_argument('--compiler', type=Path, required=True, help='existing bin/pb/scheme')
p.add_argument('--boots', type=Path, required=True, help='existing boot/pb directory')
p.add_argument('--output', type=Path, required=True, help='new isolated test directory')
a = p.parse_args()
root = a.output.resolve()
root.mkdir(parents=True)
source = root / 'source'
shutil.copytree(a.source, source, ignore=shutil.ignore_patterns('.git', '_zuo*.db'))
compiler = root / 'compiler/bin/pb/scheme'
compiler.parent.mkdir(parents=True)
shutil.copy2(a.compiler, compiler)
boots = root / 'compiler/boot/pb'
boots.mkdir(parents=True)
for name in ['petite.boot', 'scheme.boot']:
    shutil.copy2(a.boots / name, boots / name)
work = root / 'work/s'
work.mkdir(parents=True)
config = root / 'work/Mf-config'
template = 'm=ta6le\nhostm=pb\nhostworkarea=../compiler\nsessionKey=fixture-session\n'
config.write_text(template)
driver = root / 'targets.zuo'
driver.write_text('#lang zuo\n(build/command-line ((dynamic-require '
                  + json.dumps(str(source / 's/build.zuo'))
                  + ' \'targets-at) (make-at-dir "work/s")))\n')
env = dict(os.environ, ZUO_CHEZ_ACTIONS='1', ZUO_CHEZ_TOOLCHAIN_ID='fixture-sdk-v1',
           ZUO_ACTION_CACHE=str(root / 'cache'), ZUO_ACTION_CACHE_LOG='1')
rows = []


def run(label, target='priminfo.so', ok=True):
    result = subprocess.run([str(a.zuo.resolve()), str(driver), '-j', '2', 'work/s/' + target],
                            cwd=root, env=env, capture_output=True, text=True)
    (root / (label + '.stdout')).write_text(result.stdout)
    (root / (label + '.stderr')).write_text(result.stderr)
    log = result.stdout + result.stderr
    row = dict(label=label, status=result.returncode,
               hits=log.count('action cache hit '), misses=log.count('action cache miss '),
               stored=log.count('action cache stored '))
    rows.append(row)
    (root / 'results.json').write_text(json.dumps(rows, indent=2) + '\n')
    assert (result.returncode == 0) == ok, (label, log[-4000:])
    print(row, flush=True)
    return row, log


def fresh_preambles():
    for name in ['cmacros.so', 'priminfo.so', '_zuo.db', '_zuo_tc.db']:
        (work / name).unlink(missing_ok=True)


def hashes(paths):
    return {str(path.relative_to(work)): hashlib.sha256(path.read_bytes()).hexdigest()
            for path in paths}


row, _ = run('prime')
assert row['stored'] == 2
reference = hashes([work / 'cmacros.so', work / 'priminfo.so'])
fresh_preambles()
row, _ = run('fresh-db-restore')
assert row['hits'] == 2 and row['stored'] == 0
assert hashes([work / 'cmacros.so', work / 'priminfo.so']) == reference

# A reader error must be reached even with a valid prior cached output.
path = source / 's/primdata.ss'
original = path.read_bytes()
try:
    path.write_bytes(original + b'\n#<missing-input-regression>\n')
    row, log = run('source-invalidates', ok=False)
    assert row['misses'] >= 1 and 'invalid sharp-sign prefix #<' in log
finally:
    path.write_bytes(original)
fresh_preambles()
row, _ = run('source-restored')
assert row['hits'] == 2

config.write_text(template + 'o=2\n')
row, _ = run('options-invalidate')
assert row['stored'] == 2
config.write_text(template.replace('fixture-session', 'another-session'))
row, _ = run('session-key-invalidates')
assert row['stored'] == 2
config.write_text(template)
env['ZUO_CHEZ_TOOLCHAIN_ID'] = 'fixture-sdk-v2'
row, _ = run('sdk-invalidates')
assert row['stored'] == 2
env['ZUO_CHEZ_TOOLCHAIN_ID'] = 'fixture-sdk-v1'

# A harmless ELF trailer changes the compiler bytes without changing its path.
original = compiler.read_bytes()
try:
    compiler.write_bytes(original + b'\nchez-action-compiler-identity\n')
    row, _ = run('compiler-invalidates')
    assert row['stored'] == 2
finally:
    compiler.write_bytes(original)

# Corrupt an incoming boot so executing the compiler fails. A false cache hit
# would hide this failure. Restore the copied input before the next check.
boot = boots / 'scheme.boot'
original = boot.read_bytes()
try:
    boot.write_bytes(b'invalid boot\n')
    row, _ = run('boot-invalidates', ok=False)
    assert row['misses'] >= 1 and row['stored'] == 0
finally:
    boot.write_bytes(original)
fresh_preambles()
row, _ = run('tool-inputs-restored')
assert row['hits'] == 2

row, _ = run('nanopass-prime', 'nanopass.so')
assert row['stored'] == 1
paths = [work / 'nanopass.so', *sorted((work / 'nanopass').glob('*.so'))]
assert len(paths) == 14, paths
reference = hashes(paths)
paths[-1].unlink()
row, _ = run('nanopass-cooutput-restore', 'nanopass.so')
assert row['hits'] == 1 and row['stored'] == 0
assert hashes(paths) == reference

# Source checkout mtimes are not content dependencies. Verify both a real
# recompile and a cached consumer with a future-dated Nanopass source file.
helper = source / 'nanopass/nanopass/helpers.ss'
st = helper.stat()
os.utime(helper, ns=(st.st_atime_ns, st.st_mtime_ns + 60_000_000_000))
env.pop('ZUO_ACTION_CACHE')
for path in paths:
    path.unlink()
row, _ = run('nanopass-mtime-independent', 'nanopass.so')
assert hashes(paths) == reference
env['ZUO_ACTION_CACHE'] = str(root / 'cache')
run('consumer-prime', 'cprep.patch')
reference_patch = (work / 'cprep.patch').read_bytes()
(work / 'cprep.patch').unlink()
row, _ = run('consumer-cache-restore', 'cprep.patch')
assert row['hits'] == 1 and row['stored'] == 0
assert (work / 'cprep.patch').read_bytes() == reference_patch

config.write_text(template.replace('fixture-session', ''))
row, log = run('missing-key-rejected', ok=False)
assert 'requires POSIX and an explicit sessionKey' in log
config.write_text(template + 'c=t\n')
row, log = run('profiling-rejected', ok=False)
assert 'do not support profiling' in log
config.write_text(template)

# Outer bootstrap dependencies must include the previously omitted sources and
# the selected architecture backend, without another architecture's backend.
for machine, included, excluded in [('ta6le', 'x86_64.ss', 'riscv64.ss'),
                                    ('trv64le', 'riscv64.ss', 'x86_64.ss')]:
    script = root / (machine + '-inputs.zuo')
    script.write_text('#lang zuo\n(define names ((dynamic-require '
                      + json.dumps(str(source / 's/build.zuo'))
                      + ' \'all-src-names-for-machine) ' + json.dumps(machine)
                      + '))\n(for-each displayln names)\n')
    result = subprocess.run([str(a.zuo.resolve()), str(script)], cwd=root, env=env,
                            capture_output=True, text=True)
    assert result.returncode == 0, result.stderr
    names = result.stdout.splitlines()
    assert included in names and excluded not in names
    assert all(name in names for name in ['debug.ss', 'layout.ss', 'primdata.ss',
                                         'patch.ss', 'ubify.ss'])
    assert all(name in names for name in ['build.zuo', 'machine.zuo',
                                         '../makefiles/lib.zuo', '../makefiles/scheme-action.zuo'])
print('Chez action input, configuration and complete-output checks passed', flush=True)
