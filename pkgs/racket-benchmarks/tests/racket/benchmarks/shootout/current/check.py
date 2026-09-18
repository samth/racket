#!/usr/bin/env python3
"""Check every current candidate against the downloaded official small output.

Usage: python3 check.py --racket /path/to/racket
Optional --collects and --config support an uninstalled Racket CS executable.
This checker never fetches files, installs libraries, or submits programs.
"""
from pathlib import Path
import argparse, hashlib, json, subprocess, sys

P=Path(__file__).resolve().parent
ap=argparse.ArgumentParser()
ap.add_argument('--racket',default='racket')
ap.add_argument('--collects')
ap.add_argument('--config')
ap.add_argument('--compiled-root')
ap.add_argument('--experimental',action='store_true')
a=ap.parse_args()
prefix=[a.racket]
for flag,value in [('-X',a.collects),('-G',a.config),('-R',a.compiled_root)]:
    if value:prefix.extend([flag,value])
prefix.append('-y')
manifest=json.loads((P/'targets.json').read_text())
rows=[]
for family in manifest['families']:
    for candidate in family['candidates']:
        if candidate['status']=='experimental' and not a.experimental:continue
        source=P/candidate['source']
        source_hash=hashlib.sha256(source.read_bytes()).hexdigest()
        if source_hash!=candidate['sha256']:
            sys.exit('Source changed since manifest generation: '+str(source))
        fixture=family['fixture']
        data=(P/'test-data'/fixture['stdin']).read_bytes() if fixture.get('stdin') else None
        cmd=prefix+[str(source),str(fixture['n'])]
        result=subprocess.run(cmd,input=data,capture_output=True,timeout=180)
        expected=(P/'test-data'/fixture['stdout']).read_bytes()
        if result.returncode or result.stdout!=expected:
            sys.stderr.buffer.write(result.stderr)
            sys.exit('FAIL '+candidate['source']+' (official output mismatch or process error)')
        rows.append(dict(source=candidate['source'],bytes=len(expected),
                         sha256=hashlib.sha256(result.stdout).hexdigest()))
        print('PASS',candidate['source'])
print(json.dumps(dict(checks=len(rows),results=rows),indent=2))
