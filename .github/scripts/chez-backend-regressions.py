#!/usr/bin/env python3
"""Check backend dependency precision using an isolated, warm x86-64 PB build.

Mutates only the disposable bundled sources and restores build state afterwards.
The active-backend failure verifies invalidation as well as backend selection.
"""
from pathlib import Path
import argparse,json,os,shutil,subprocess,tempfile,time
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--root',type=Path,required=True)
p.add_argument('--zuo',type=Path,required=True)
p.add_argument('--output',type=Path,required=True)
a=p.parse_args();root=a.root.resolve();zuo=a.zuo.resolve();out=a.output.resolve();out.mkdir(parents=True,exist_ok=True)
state=root/'replay-state';build=state/'build';cs=build/'cs/c';src=root/'racket/src/ChezScheme/s'
parent=json.loads((state/'parent.json').read_text());env=dict(os.environ,RACKET_CHEZ_SESSION_KEY=parent['session_key']);rows=[]
with tempfile.TemporaryDirectory(prefix='backend-build-backup-',dir=root) as d:
 backup=Path(d)/'build';shutil.copytree(build,backup,symlinks=True)
 try:
  for name,ok in [('riscv64.ss',True),('loongarch64.ss',True),('x86_64.ss',False)]:
   path=src/name;original=path.read_bytes();st=path.stat()
   try:
    # np-register.ss requires exactly three expressions in a backend file.
    path.write_bytes(original+b'\n(error \'backend-dependency-regression "poisoned backend")\n')
    for f in build.rglob('_zuo_tc.db'):f.unlink()
    start=time.monotonic()
    r=subprocess.run([str(zuo),'-X',str(root/'racket/src/zuo/lib'),'.','-j','4','scheme'],cwd=cs,env=env,capture_output=True,text=True)
    log=r.stdout+r.stderr;(out/(name+'.log')).write_text(log)
    count=sum(line.startswith('compiling ') and ' with output to ' in line for line in log.splitlines())
    rows.append(dict(backend=name,status=r.returncode,scheme_compilations=count,seconds=time.monotonic()-start))
    assert (r.returncode==0)==ok,(name,log[-2500:])
    if ok:assert count==0,(name,count)
    else:assert 'too many expressions in ' in log and '/x86_64.ss' in log,log[-2500:]
   finally:path.write_bytes(original);os.utime(path,ns=(st.st_atime_ns,st.st_mtime_ns))
 finally:
  shutil.rmtree(build);shutil.copytree(backup,build,symlinks=True)
  for f in build.rglob('_zuo_tc.db'):f.unlink()
  (out/'results.json').write_text(json.dumps(rows,indent=2)+'\n')
print(json.dumps(rows,indent=2))
