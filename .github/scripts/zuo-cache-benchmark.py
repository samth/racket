#!/usr/bin/env python3
"""Measure native CS caching without reusing a Chez/build tree.

Run `prepare`, then any of current/seed/local/github. Each measured build starts
in the same empty directory. Only the Zuo CAS survives between seed and local.
GitHub transports that same CAS to the github job; it contains no build DBs.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import time

REPO = Path(__file__).resolve().parents[2]
SRC = REPO / 'racket/src'
ROOT = Path(os.environ.get('RUNNER_TEMP', '/tmp')) / 'racket-zuo-cache-benchmark'
EVIDENCE = Path(os.environ.get('RUNNER_TEMP', '/tmp')) / 'zuo-cache-evidence'
CACHE = ROOT / 'cache'
BUILD = ROOT / 'build'
ZUO = ROOT / 'zuo'
NAMES = ['chezpart', 'rumble', 'thread', 'io', 'regexp', 'schemify',
         'linklet', 'expander', 'main']


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def write(path, value):
    path.write_text(json.dumps(value, indent=2) + '\n')


def capture(*command):
    return subprocess.check_output(command, text=True).strip()


def prepare():
    ROOT.mkdir(parents=True, exist_ok=True)
    EVIDENCE.mkdir(parents=True, exist_ok=True)
    # Hosted image identity covers the SDK/shared libraries omitted from the
    # pilot's direct tool inputs. Paths remain part of Zuo's action identity.
    packages = capture('dpkg-query', '-W')
    identity = dict(schema='gha-zuo-native-v1', image=os.environ.get('ImageOS'),
                    image_version=os.environ.get('ImageVersion'),
                    machine=capture('uname', '-m'), compiler=capture('cc', '--version'),
                    linker=capture('ld', '--version'),
                    packages_sha256=hashlib.sha256(packages.encode()).hexdigest(),
                    source_root=str(REPO), build_root=str(BUILD))
    key = hashlib.sha256(json.dumps(identity, sort_keys=True).encode()).hexdigest()
    write(EVIDENCE / 'runner.json', dict(identity, toolchain=key))
    subprocess.run(['cc', '-O2', '-DZUO_LIB_PATH=' + json.dumps(str(SRC / 'zuo/lib')),
                    '-o', str(ZUO), str(SRC / 'zuo/zuo.c')], check=True)
    output = os.environ.get('GITHUB_OUTPUT')
    if output:
        with open(output, 'a') as stream:
            stream.write('toolchain=' + key + '\n')
    print('Toolchain fingerprint:', key, flush=True)


def measure(mode):
    assert mode in ['current', 'seed', 'local', 'github']
    runner = json.loads((EVIDENCE / 'runner.json').read_text())
    env = dict(os.environ)
    for name in ['ZUO_ACTION_CACHE', 'ZUO_CS_ACTIONS', 'ZUO_CS_TOOLCHAIN_ID',
                 'RACKET_CHEZ_SESSION_KEY', 'PLT_CS_COVERAGE_COUNTERS']:
        env.pop(name, None)
    if mode != 'current':
        env.update(ZUO_CS_ACTIONS='1', ZUO_CS_TOOLCHAIN_ID=runner['toolchain'],
                   ZUO_ACTION_CACHE=str(CACHE), ZUO_ACTION_CACHE_LOG='1')
    key = env['RACKET_CI_SESSION_KEY']
    if mode == 'seed':
        assert not CACHE.exists(), 'seed must start with empty storage'
    reference = CACHE / 'benchmark-reference.json'
    previous = json.loads(reference.read_text()) if reference.exists() else None
    if mode in ['local', 'github']:
        assert previous, 'expected a seeded action cache'
        assert previous['toolchain'] == runner['toolchain'], 'runner image changed'
    if BUILD.exists():
        assert not BUILD.is_symlink()
        shutil.rmtree(BUILD)
    BUILD.mkdir()
    result = dict(mode=mode, toolchain=runner['toolchain'],
                  source_commit=capture('git', '-C', str(REPO), 'rev-parse', 'HEAD'),
                  session_key=key, phases=[])
    write(EVIDENCE / (mode + '.json'), result)

    def run(label, command):
        start = time.monotonic()
        with (EVIDENCE / (mode + '-' + label + '.stdout')).open('wb') as out, \
             (EVIDENCE / (mode + '-' + label + '.stderr')).open('wb') as err:
            p = subprocess.run(command, cwd=BUILD, env=env, stdout=out, stderr=err)
        row = dict(phase=label, command=command, seconds=time.monotonic() - start,
                   status=p.returncode)
        result['phases'].append(row)
        write(EVIDENCE / (mode + '.json'), result)
        print(mode, label, round(row['seconds'], 2), 'seconds, status', p.returncode, flush=True)
        if p.returncode:
            raise SystemExit(p.returncode)

    run('configure', [str(SRC / 'configure'), '--enable-csonly', '--disable-docs',
                      '--enable-sessionkey=' + key])
    run('build', [str(ZUO), '.', '-j', '4', 'cs'])
    run('smoke', [str(BUILD / 'cs/c/racketcs'), '-X', str(REPO / 'racket/collects'),
                  '-e', '(begin (unless (= 6 (apply + (list 1 2 3))) '
                  '(error "smoke")) (displayln "Racket smoke passed"))'])
    log = ''.join((EVIDENCE / (mode + '-build.' + s)).read_text() for s in ['stdout', 'stderr'])
    result['cache_events'] = {event: len(re.findall(r'action cache ' + event + r' [0-9a-f]{64}', log))
                              for event in ['hit', 'miss', 'stored', 'rejected']}
    result['compiler_invocations'] = sum('/cs/compile-file.ss ' in line and
                                         'compile-action.zuo' not in line
                                         for line in log.splitlines())
    paths = [BUILD / 'cs/c' / (name + suffix) for name in NAMES for suffix in ['.so', '.wpo']]
    paths.append(BUILD / 'cs/c/racket.so')
    result['native_outputs'] = {p.name: digest(p) for p in paths}
    config = dict((k.strip(), v.strip()) for line in (BUILD / 'cs/c/Makefile').read_text().splitlines()
                  if '=' in line and not line.lstrip().startswith('#') for k, v in [line.split('=', 1)])
    machine = config['MACH']
    chez = BUILD / 'cs/c/ChezScheme' / machine
    tools = [chez / 'bin' / machine / 'scheme']
    tools += [chez / 'boot' / machine / (name + '.boot') for name in ['petite', 'scheme']]
    result['chez_tools'] = {p.name: digest(p) for p in tools}
    result['zuo_sha256'] = digest(ZUO)
    result['cache_bytes'] = sum(p.stat().st_size for p in CACHE.rglob('*') if p.is_file())
    result['cache_actions'] = len(list((CACHE / 'actions').glob('*')))
    if previous:
        result['native_matches_seed'] = sum(value == previous['native_outputs'][name]
                                             for name, value in result['native_outputs'].items())
        result['chez_matches_seed'] = result['chez_tools'] == previous['chez_tools']
    if mode == 'seed':
        assert result['cache_events']['stored'] == 10, result['cache_events']
        write(reference, result)
    write(EVIDENCE / (mode + '.json'), result)
    if os.environ.get('GITHUB_OUTPUT'):
        with open(os.environ['GITHUB_OUTPUT'], 'a') as stream:
            stream.write('stored=' + str(result['cache_events']['stored']) + '\n')
    print(json.dumps({k: result[k] for k in ['mode', 'cache_events', 'compiler_invocations',
                                            'cache_bytes']}), flush=True)


parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('mode', choices=['prepare', 'current', 'seed', 'local', 'github'])
args = parser.parse_args()
if args.mode == 'prepare':
    prepare()
else:
    measure(args.mode)
