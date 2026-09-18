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
ap.add_argument('--inventory-only',action='store_true',help='check the exact ten-program inventory without running Racket')
a=ap.parse_args()
prefix=[a.racket]
for flag,value in [('-X',a.collects),('-G',a.config),('-R',a.compiled_root)]:
    if value:prefix.extend([flag,value])
prefix.append('-y')
manifest=json.loads((P/'targets.json').read_text())
CURRENT_FAMILIES={'nbody':'nbody','spectralnorm':'spectralnorm',
    'fannkuch-redux':'fannkuchredux','binarytrees':'binarytrees','pidigits':'pidigits',
    'fasta':'fasta','knucleotide':'knucleotide','mandelbrot':'mandelbrot',
    'regexredux':'regexredux','revcomp':'revcomp'}
def require(condition,message):
    if not condition:sys.exit('Invalid submission inventory: '+message)
families=manifest['families']
require(len(families)==len(CURRENT_FAMILIES),'expected exactly ten benchmark families')
require({f['family']:f['site_family'] for f in families}==CURRENT_FAMILIES,
        'families must match the current Benchmarks Game, with no legacy tests')
declared=[]
for family in families:
    require(len(family['candidates'])==1,family['family']+' must have exactly one candidate')
    candidate=family['candidates'][0]
    source=candidate['source']
    require(candidate['status']=='submission-candidate','analysis controls are not submission candidates')
    require(source==Path(source).name and source.endswith('.rkt') and source!='info.rkt',
            'candidate must be a standalone program in this directory')
    require(family['primary']==source,'primary must be the sole candidate')
    declared.append(source)
    require((P/source).is_file(),'missing source '+source)
    require(hashlib.sha256((P/source).read_bytes()).hexdigest()==candidate['sha256'],
            'source changed since manifest generation: '+source)
require(len(set(declared))==10,'each benchmark must have its own source')
actual={str(p.relative_to(P)) for p in P.rglob('*.rkt') if p.name!='info.rkt'}
require(actual==set(declared),'extra or missing benchmark programs: '+str(sorted(actual^set(declared))))
if a.inventory_only:
    print('PASS exactly ten current families, one submission candidate each; source hashes match')
    sys.exit(0)
rows=[]
for family in families:
    for candidate in family['candidates']:
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
