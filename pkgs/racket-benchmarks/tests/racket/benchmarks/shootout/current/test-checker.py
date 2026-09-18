#!/usr/bin/env python3
"""Regression checks for the one-current-candidate-per-family invariant."""
from pathlib import Path
import copy, json, shutil, subprocess, sys, tempfile

P=Path(__file__).resolve().parent
manifest=json.loads((P/'targets.json').read_text())
def verify(label,edit=None,extra=None,expected=False):
    with tempfile.TemporaryDirectory(prefix='racket-submission-inventory-') as directory:
        target=Path(directory)/'current'
        shutil.copytree(P,target,ignore=shutil.ignore_patterns('compiled','__pycache__'))
        data=copy.deepcopy(manifest)
        if edit:edit(data)
        (target/'targets.json').write_text(json.dumps(data))
        if extra:
            path=target/extra;path.parent.mkdir(parents=True,exist_ok=True)
            path.write_text('#lang racket/base\n')
        result=subprocess.run([sys.executable,str(target/'check.py'),'--inventory-only'],capture_output=True,text=True)
        assert (result.returncode==0)==expected,(label,result.stdout,result.stderr)
        print('PASS',label)

verify('exact current inventory',expected=True)
verify('duplicate candidate rejected',lambda d:d['families'][0]['candidates'].append(copy.deepcopy(d['families'][0]['candidates'][0])))
verify('missing candidate rejected',lambda d:d['families'][0]['candidates'].clear())
verify('duplicate family rejected',lambda d:d['families'].__setitem__(1,copy.deepcopy(d['families'][0])))
verify('legacy family rejected',lambda d:d['families'][0].update(family='ackermann',site_family='ackermann'))
verify('extra family rejected',lambda d:d['families'].append(copy.deepcopy(d['families'][0])))
verify('wrong status rejected',lambda d:d['families'][0]['candidates'][0].update(status='analysis-control'))
verify('wrong primary rejected',lambda d:d['families'][0].update(primary='other.rkt'))
verify('path escape rejected',lambda d:d['families'][0]['candidates'][0].update(source='../other.rkt'))
verify('changed source hash rejected',lambda d:d['families'][0]['candidates'][0].update(sha256='0'*64))
verify('unlisted program rejected',extra='extra.rkt')
verify('nested alternate rejected',extra='alternatives/extra.rkt')
