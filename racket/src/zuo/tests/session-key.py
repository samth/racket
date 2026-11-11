#!/usr/bin/env python3
"""Check session-key forwarding and warm rebuilds with an isolated compiler stub.

Supply the source directory and Makefile from a configured native CS build
with WPO enabled.
The stub records actual compiler arguments and creates the declared outputs.
"""
import argparse
import json
import os
import re
from pathlib import Path
import shutil
import subprocess
import sys

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--zuo', type=Path, required=True)
parser.add_argument('--source', type=Path, required=True, help='racket/src directory')
parser.add_argument('--makefile', type=Path, required=True, help='configured cs/c/Makefile')
parser.add_argument('--output', type=Path, required=True, help='new isolated test directory')
args = parser.parse_args()
root = args.output.resolve()
root.mkdir(parents=True)
source = args.source.resolve()
work = root / 'work'
work.mkdir()
build = work / 'cs/c'
build.mkdir(parents=True)
shutil.copyfile(args.makefile, build / 'Makefile')
config = dict((key.strip(), value.strip())
              for line in args.makefile.read_text().splitlines()
              if '=' in line and not line.lstrip().startswith('#')
              for key, value in [line.split('=', 1)])
machine = config['MACH'].strip()
tool = build / 'ChezScheme' / machine / 'bin' / machine / 'scheme'
tool.parent.mkdir(parents=True)
boots = build / 'ChezScheme' / machine / 'boot' / machine
boots.mkdir(parents=True)
for name in ['petite.boot', 'scheme.boot']:
    (boots / name).write_text('fixture boot input\n')
calls = root / 'compiler-calls.jsonl'
tool.write_text('#!' + sys.executable + '\n' + '''import json,sys
from pathlib import Path
args=sys.argv[sys.argv.index('--script')+1:]
script=Path(args.pop(0)).name
if script=='compile-file.ss':
    key=None
    while args[0].startswith('--'):
        flag=args.pop(0)
        if flag in ['--session-key','--src','--dest','--xpatch']:
            value=args.pop(0)
            if flag=='--session-key':key=value
    dest=Path(args[1])
    row=dict(script=script,key=key,dest=dest.name)
    dest.write_text(json.dumps(row))
    if dest.name!='racket.so':dest.with_suffix('.wpo').write_text(json.dumps(row))
else:
    assert script=='convert-to-boot.ss',script
    dest=Path(args[-2])
    dest.write_bytes(Path(args[-3]).read_bytes())
    row=dict(script=script,dest=dest.name)
with open(CALLS,'a') as out:out.write(json.dumps(row)+'\\n')
'''.replace('CALLS', repr(str(calls))))
tool.chmod(0o755)
driver = root / 'targets.zuo'
driver.write_text('#lang zuo\n(build/command-line* (dynamic-require '
                  + json.dumps(str(source / 'cs/c/build.zuo'))
                  + ' \'targets-at) (make-at-dir "cs/c"))\n')

template = (build / 'Makefile').read_text()

def run(label, key=None, override=None, changed=True):
    before = len(calls.read_text().splitlines()) if calls.exists() else 0
    env = dict(os.environ)
    # Configuration supplies the value; the old environment adapter is ignored.
    env['RACKET_CHEZ_SESSION_KEY'] = 'obsolete adapter value'
    configured = key or ''
    text, count = re.subn(r'(?m)^SESSION_KEY[ \t]*=.*$',
                          lambda m: 'SESSION_KEY = ' + configured, template)
    assert count == 1
    (build / 'Makefile').write_text(text)
    command = [str(args.zuo.resolve()), str(driver), '-j', '4', 'cs/c/racket.boot']
    if override is not None:
        command.append('SESSION_KEY=' + override)
    result = subprocess.run(command, cwd=work, env=env, capture_output=True, text=True)
    (root / (label + '.stdout')).write_text(result.stdout)
    (root / (label + '.stderr')).write_text(result.stderr)
    assert result.returncode == 0, result.stdout + result.stderr
    new = [json.loads(line) for line in calls.read_text().splitlines()[before:]]
    assert len(new) == (11 if changed else 0), (label, new)
    effective = key if override is None else override
    expected = {}
    if effective and new:
        names = [row['dest'] for row in new if row['script'] == 'compile-file.ss']
        helper = root / (label + '-keys.zuo')
        helper.write_text('#lang zuo\n(require '
                          + json.dumps(str(source / 'ChezScheme/makefiles/lib.zuo'))
                          + ')\n(for-each (lambda (name) (displayln (~a (~s name) " " '
                          + '(~s (session-key-for ' + json.dumps(effective)
                          + ' "racket" name))))) \'('
                          + ' '.join(json.dumps(name) for name in names) + '))\n')
        keys = subprocess.run([str(args.zuo.resolve()), str(helper)], cwd=work,
                              capture_output=True, text=True)
        assert keys.returncode == 0, keys.stderr
        for line in keys.stdout.splitlines():
            name, end = json.JSONDecoder().raw_decode(line)
            expected[name] = json.loads(line[end:].strip())
        assert len(set(expected.values())) == len(names)
    for row in new:
        if row['script'] == 'compile-file.ss':
            assert row['key'] == expected.get(row['dest']), (label, row)
    print(label, 'passed', flush=True)

run('unset')
run('unset-warm', changed=False)
run('configured', key="build base with 'quotes' = value")
run('configured-warm', key="build base with 'quotes' = value", changed=False)
run('changed-key', key='second build base')
run('explicit-override', key='configured base', override='explicit base')
run('explicit-empty', key='configured base', override='')
run('empty-configuration-warm', key='', changed=False)
print('Session-key forwarding and dependency checks passed')
