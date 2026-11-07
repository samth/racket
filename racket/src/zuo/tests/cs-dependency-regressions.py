#!/usr/bin/env python3
"""Mutation regressions for native CS source dependencies.

Run cs-action-cache.py first, then pass its evidence directory as --experiment.
This explicitly mutates and restores two source files; use an isolated checkout.
The existing output/cache groups are backed up and restored around each case.
"""
from pathlib import Path
import argparse,json,os,shutil,subprocess,tempfile,time
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--zuo',type=Path,required=True)
p.add_argument('--source',type=Path,required=True,help='isolated racket/src checkout')
p.add_argument('--experiment',type=Path,required=True)
p.add_argument('--toolchain-id',required=True)
p.add_argument('--driver',type=Path)
p.add_argument('--workarea',type=Path)
p.add_argument('--cache',type=Path)
a=p.parse_args();src=a.source.resolve();exp=a.experiment.resolve();work=a.workarea.resolve() if a.workarea else exp/'work';cache=a.cache.resolve() if a.cache else exp/'cache';driver=a.driver.resolve() if a.driver else exp/'main.zuo'
assert driver.is_file() and (work/'racket.so').is_file() and cache.is_dir()
out=exp/'dependency-regressions';out.mkdir(exist_ok=True);rows=[]
env=dict(os.environ,ZUO_CS_ACTIONS='1',ZUO_CS_TOOLCHAIN_ID=a.toolchain_id,ZUO_ACTION_CACHE=str(cache),ZUO_ACTION_CACHE_LOG='1')
with tempfile.TemporaryDirectory(prefix='cs-dependency-baseline-') as d:
 baseline=Path(d)
 for path in work.iterdir():
  if path.is_file():shutil.copy2(path,baseline/path.name)
 def restore():
  for path in work.iterdir():
   if path.is_file():path.unlink()
  for path in baseline.iterdir():shutil.copy2(path,work/path.name)
  (work/'_zuo_tc.db').unlink(missing_ok=True)
 def run(label,ok):
  start=time.monotonic();r=subprocess.run([str(a.zuo.resolve()),'-X',str(src/'zuo/lib'),str(driver),'-j','4'],cwd=work,env=env,capture_output=True,text=True);text=r.stdout+r.stderr;(out/(label+'.log')).write_text(text)
  row=dict(label=label,status=r.returncode,seconds=time.monotonic()-start,cache_hits=text.count('action cache hit '),cache_stores=text.count('action cache stored '));rows.append(row);(out/'results.json').write_text(json.dumps(rows,indent=2)+'\n')
  assert (r.returncode==0)==ok,text[-2000:]
  if not ok:assert 'invalid sharp-sign prefix #<' in text,text[-2000:]
 for label,rel in [('io-rktl','rktio/rktio.rktl'),('linklet-trace','cs/linklet/trace.ss')]:
  restore();path=src/rel;original=path.read_bytes();st=path.stat()
  try:
   path.write_bytes(original+b'\n#<dependency-regression-invalid>\n')
   run(label+'-warm-reject',False)
   for f in work.iterdir():
    if f.is_file():f.unlink()
   run(label+'-fresh-cache-reject',False)
  finally:path.write_bytes(original);os.utime(path,ns=(st.st_atime_ns,st.st_mtime_ns));restore()
 # The unused hygienic module should no longer force any actions to rebuild.
 unused=src/'zuo/lib/zuo/private/base-hygienic/struct.zuo';original=unused.read_bytes();st=unused.stat()
 try:
  unused.write_bytes(original+b'\n; unused module regression\n');run('unused-module-warm',True)
  assert rows[-1]['cache_hits']==rows[-1]['cache_stores']==0
 finally:unused.write_bytes(original);os.utime(unused,ns=(st.st_atime_ns,st.st_mtime_ns));restore()
print('Warm and empty-directory builds reject both invalid sources; unused module stays up to date.')
