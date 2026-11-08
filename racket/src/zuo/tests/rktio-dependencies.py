#!/usr/bin/env python3
"""Check static rktio command identity without turning linker flags into C inputs."""
from pathlib import Path
import argparse,json,re,shutil,subprocess,tempfile,time
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--zuo',type=Path,required=True)
p.add_argument('--build',type=Path,required=True,help='isolated static rktio build')
p.add_argument('--output',type=Path,required=True)
a=p.parse_args();work=a.build.resolve();out=a.output.resolve();out.mkdir(parents=True);rows=[]
def run(label,target,ok=True):
 r=subprocess.run([str(a.zuo.resolve()),'.','-j','4',target],cwd=work,capture_output=True,text=True)
 (out/(label+'.log')).write_text(r.stdout+r.stderr);rows.append(dict(label=label,status=r.returncode));(out/'results.json').write_text(json.dumps(rows,indent=2)+'\n')
 assert (r.returncode==0)==ok,r.stdout+r.stderr
 if not ok:assert 'dependency-regression' in r.stdout+r.stderr,r.stdout+r.stderr
 print(label,r.returncode,flush=True)
with tempfile.TemporaryDirectory(prefix='rktio-dependencies-') as temp:
 backup=Path(temp)/'work';shutil.copytree(work,backup,symlinks=True)
 try:
  run('control','librktio.a')
  original=(work/'Makefile').read_bytes()
  def mutate(key,option):
   changed,n=re.subn(rb'(?m)^('+key.encode()+rb'\s*=.*)$',lambda m:m[0]+b' '+option.encode(),original)
   assert n==1,key
   (work/'Makefile').write_bytes(changed)
  for key in ['CFLAGS','CPPFLAGS']:
   mutate(key,'-fdependency-regression-'+key)
   run(key.lower()+'-warm-reject','rktio_fd.o',ok=False)
   (work/'Makefile').write_bytes(original)
   run(key.lower()+'-restore','librktio.a')
  before={p.name:p.stat().st_mtime_ns for p in work.glob('*.o')}
  mutate('LDFLAGS','-Wl,--dependency-regression-unused-link-flag')
  run('ldflags-not-a-compile-input','librktio.a')
  assert before=={p.name:p.stat().st_mtime_ns for p in work.glob('*.o')}
  mutate('ARFLAGS','--dependency-regression-ARFLAGS')
  run('arflags-warm-reject','librktio.a',ok=False)
 finally:
  shutil.rmtree(work);shutil.copytree(backup,work,symlinks=True)
print('rktio command dependency regressions passed')
