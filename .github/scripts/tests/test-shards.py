#!/usr/bin/env python3
"""Check that sharding preserves the existing suite commands and timeouts."""
from pathlib import Path
import collections
import json
import os
import subprocess
import tempfile

SCRIPT = Path(__file__).resolve().parents[1] / 'run-racket-tests.sh'
with tempfile.TemporaryDirectory(prefix='racket-shards-') as d:
    root = Path(d)
    stub = '''#!/usr/bin/env python3
import json,os,sys
with open(os.environ['CALLS'], 'a') as f: f.write(json.dumps([os.path.basename(sys.argv[0]),*sys.argv[1:]])+'\\n')
if sys.argv[1:2] == ['-e']: print(4)
'''
    for name in ['raco', 'racket']:
        (root / name).write_text(stub)
        (root / name).chmod(0o755)
    def run(*args, ok=True):
        calls = root / 'calls'; calls.write_text('')
        env = dict(os.environ, PATH=str(root)+os.pathsep+os.environ['PATH'], CALLS=str(calls))
        p = subprocess.run(['bash', str(SCRIPT), *args], env=env, capture_output=True, text=True)
        assert (p.returncode == 0) == ok, p.stdout+p.stderr
        return [json.loads(line) for line in calls.read_text().splitlines()]
    def tests(calls):
        return [tuple(c) for c in calls if c[1:2] == ['test'] or c[1:] == ['-l','tests/racket/contract/all']]
    def installs(calls): return [c for c in calls if c[1:3] == ['pkg','install']]
    all_calls = run()
    core = run('--skip-install','--shard','core')
    other = run('--skip-install','--shard','other')
    assert len(tests(all_calls)) == 18
    assert len(tests(core)) == 1 and len(tests(other)) == 17
    assert collections.Counter(tests(all_calls)) == collections.Counter(tests(core)+tests(other))
    assert len(installs(all_calls)) == 1 and not installs(core)+installs(other)
    install_only = run('--install-only')
    assert len(installs(install_only)) == 1 and not tests(install_only)
    run('--shard','bogus',ok=False)
    run('--unknown',ok=False)
    print('All 18 suite commands and timeout flags preserved; setup is separate.')
