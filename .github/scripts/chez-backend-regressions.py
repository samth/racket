#!/usr/bin/env python3
"""Verify source/config invalidation with a constant CI namespace.

Requires a disposable warm x86-64 PB build. Sources, config and build state are
restored after each mutation. No source-derived namespace can force a rebuild.
"""
from pathlib import Path
import argparse,json,os,re,shutil,subprocess,tempfile,time
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--root',type=Path,required=True)
p.add_argument('--zuo',type=Path,required=True)
p.add_argument('--output',type=Path,required=True)
a=p.parse_args();root=a.root.resolve();zuo=a.zuo.resolve();out=a.output.resolve();out.mkdir(parents=True,exist_ok=True)
state=root/'replay-state';build=state/'build';cs=build/'cs/c';src=root/'racket/src/ChezScheme/s';native=cs/'ChezScheme/ta6le'
key=(root/'.github/chez-session-key').read_text().strip();env=dict(os.environ,RACKET_CHEZ_SESSION_KEY=key);rows=[]
with tempfile.TemporaryDirectory(prefix='fixed-base-build-backup-',dir=root) as d:
 backup=Path(d)/'build';shutil.copytree(build,backup,symlinks=True)
 def restore():
  shutil.rmtree(build);shutil.copytree(backup,build,symlinks=True)
  for f in build.rglob('_zuo_tc.db'):f.unlink()
 def check(label,cwd,target,ok,diagnostic=None):
  for f in build.rglob('_zuo_tc.db'):f.unlink()
  start=time.monotonic()
  r=subprocess.run([str(zuo),'-X',str(root/'racket/src/zuo/lib'),'.','-j','4',target],cwd=cwd,env=env,capture_output=True,text=True)
  log=r.stdout+r.stderr;(out/(label+'.log')).write_text(log)
  count=sum(line.startswith('compiling ') and ' with output to ' in line for line in log.splitlines())
  rows.append(dict(case=label,status=r.returncode,scheme_compilations=count,c_compilations=sum(' -c ' in line for line in log.splitlines()),seconds=time.monotonic()-start,session_key=key))
  assert (r.returncode==0)==ok,(label,log[-2500:])
  if ok:assert count==0,(label,count)
  if diagnostic:assert diagnostic in log,log[-2500:]
 try:
  for name,ok,poison,diagnostic in [
    ('riscv64.ss',True,b"\n(error 'backend-dependency-regression)\n",None),
    ('loongarch64.ss',True,b"\n(error 'backend-dependency-regression)\n",None),
    ('x86_64.ss',False,b"\n(error 'backend-dependency-regression)\n",'too many expressions in '),
    ('cpnanopass.ss',False,b'\n#<fixed-session-dependency-regression>\n','invalid sharp-sign prefix #<')]:
   restore();path=src/name;original=path.read_bytes();st=path.stat()
   try:
    path.write_bytes(original+poison)
    check(name,cs,'scheme',ok,diagnostic)
   finally:path.write_bytes(original);os.utime(path,ns=(st.st_atime_ns,st.st_mtime_ns))
  restore();path=native/'Mf-config';original=path.read_bytes();blocked=out/'blocked.h';blocked.write_text('#error dependency-regression-CPPFLAGS\n')
  try:
   changed=re.sub(rb'^CPPFLAGS=.*$',lambda m:m[0]+b' -include '+str(blocked).encode(),original,flags=re.M)
   assert changed!=original,'No CPPFLAGS entry in the native config'
   path.write_bytes(changed)
   check('CPPFLAGS',native/'c','stats.o',False,'dependency-regression-CPPFLAGS')
  finally:path.write_bytes(original)
 finally:
  restore();(out/'results.json').write_text(json.dumps(rows,indent=2)+'\n')
print(json.dumps(rows,indent=2))
