#!/usr/bin/env python3
"""Exercise build-key scoping without compiling Scheme or touching the checkout."""
from pathlib import Path
import argparse,hashlib,json,re,shlex,subprocess,sys,tempfile
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--zuo',type=Path,required=True)
p.add_argument('--source',type=Path,default=Path(__file__).resolve().parents[1])
a=p.parse_args()
with tempfile.TemporaryDirectory(prefix='chez-build-key-') as d:
    root=Path(d);src=root/'source';work=root/'work';work.mkdir()
    for name in ['s','nanopass','unicode','makefiles']:(src/name).mkdir(parents=True)
    unit=src/'s/unit.ss';unit.write_text('; first source\n')
    cap=root/'capture.py';cap.write_text('import sys\nopen(sys.argv[1],"w").write(sys.stdin.read())\n')
    captured=root/'stdin';driver=root/'main.zuo'
    def key(seed='',what='unit',script='(void)'):
        q=lambda x:json.dumps(str(x))
        driver.write_text('''#lang zuo
(require LIB zuo/thread)
(define runners (make-run-scheme (make-at-dir SRC) (make-at-dir DEST)
  (lambda (k) (if (eq? k 'sessionKey) SEED #f)) HOST "" "ta6le"))
(call-in-main-thread (lambda () ((list-ref runners 2) WHAT '(SCRIPT) '())))
'''.replace('LIB',q(a.source.resolve()/'makefiles/lib.zuo')).replace('SRC',q(src/'s')).replace('DEST',q(work)).replace('SEED',q(seed)).replace('HOST',q(shlex.join([sys.executable,str(cap),str(captured)]))).replace('WHAT',q(what)).replace('SCRIPT',script))
        r=subprocess.run([str(a.zuo.resolve()),str(driver)],capture_output=True,text=True)
        assert not r.returncode,r.stdout+r.stderr
        m=re.search(r'\$session-key\) "([^"]+)"',captured.read_text())
        return m.group(1) if m else None
    assert key() is None, 'Default builds must not override the session key'
    first=key(seed='custom');assert first==key(seed='custom')
    assert first!=key(seed='custom',what='another-unit')
    # The ordinary explicit-key API has no source/script hashing or auto mode.
    assert first==key(seed='custom',script='(void 1)')
    unit.write_text('; changed source\n');assert first==key(seed='custom')
    # Exact legacy preimage, including old directory scope and NUL separators.
    prefix='k'+hashlib.sha256(('custom\0'+str(work)+'\0unit').encode()).hexdigest()[:24]+'-'
    actual=key(seed='custom')
    assert actual==prefix,(actual,prefix)
    literal='k'+hashlib.sha256(('auto\0'+str(work)+'\0unit').encode()).hexdigest()[:24]+'-'
    assert key(seed='auto')==literal
print('Default has no override; explicit key scoping retains the upstream formula.')
