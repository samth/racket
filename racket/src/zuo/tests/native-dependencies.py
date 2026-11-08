#!/usr/bin/env python3
"""Warm native CS source and co-output checks in an isolated built checkout."""
from pathlib import Path
import argparse,json,os,subprocess,time
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--zuo',type=Path,required=True)
p.add_argument('--source',type=Path,required=True)
p.add_argument('--build',type=Path,required=True)
p.add_argument('--output',type=Path,required=True)
a=p.parse_args();src=a.source.resolve();work=a.build.resolve();out=a.output.resolve();out.mkdir(parents=True)
driver=out/'targets.zuo'
driver.write_text('''#lang zuo
(require SOURCE_LIB)
(define config (config-file->hash "cs/c/Makefile"))
(define base (build-path "cs/c/ChezScheme" (hash-ref config 'MACH)))
(define boots (list (build-path base "boot" (hash-ref config 'MACH) "petite.boot")
                    (build-path base "boot" (hash-ref config 'MACH) "scheme.boot")))
(define ts ((dynamic-require SOURCE_MAIN 'targets-at) (make-at-dir "cs/c")
 (hash-set* config 'SCHEME (build-path base "bin" (hash-ref config 'MACH) "scheme")
                  'scheme-args (list "-B" (car boots) "-B" (cadr boots)) 'schemify "f") boots))
(build/command-line ts)
'''.replace('SOURCE_LIB',json.dumps(str(src/'lib.zuo'))).replace('SOURCE_MAIN',json.dumps(str(src/'cs/main.zuo'))))
rows=[]
def run(label,target,ok=True):
 t=time.monotonic();r=subprocess.run([str(a.zuo.resolve()),str(driver),'-j','4','cs/c/'+target],cwd=work,capture_output=True,text=True)
 log=r.stdout+r.stderr;(out/(label+'.log')).write_text(log);rows.append(dict(label=label,status=r.returncode,seconds=time.monotonic()-t));(out/'results.json').write_text(json.dumps(rows,indent=2)+'\n')
 assert (r.returncode==0)==ok,log
 if not ok:assert 'invalid sharp-sign prefix #<' in log,log
 print(label,r.returncode,flush=True)
for label,relative,name in [('io-include','rktio/rktio.rktl','io'),('linklet-include','cs/linklet/trace.ss','linklet')]:
 run(label+'-control',name+'.so')
 file=src/relative;original=file.read_bytes();st=file.stat()
 artifacts=[work/'cs/c'/(name+suffix) for suffix in ['.so','.wpo','.so.compile-stamp']]
 backup=[(p,p.read_bytes(),p.stat()) for p in artifacts]
 try:
  file.write_bytes(original+b'\n#<dependency-regression-native>\n');os.utime(file,ns=(st.st_atime_ns,st.st_mtime_ns+2_000_000_000))
  run(label+'-warm-reject',name+'.so',ok=False)
 finally:
  file.write_bytes(original);os.utime(file,ns=(st.st_atime_ns,st.st_mtime_ns))
  for p,data,s in backup:p.write_bytes(data);p.chmod(s.st_mode&0o777);os.utime(p,ns=(s.st_atime_ns,s.st_mtime_ns))
run('cooutput-control','io.so')
artifacts=[work/'cs/c'/('io'+suffix) for suffix in ['.so','.wpo','.so.compile-stamp']]
backup=[(p,p.read_bytes(),p.stat()) for p in artifacts]
try:
 (work/'cs/c/io.wpo').unlink();run('cooutput-repair','io.so');assert (work/'cs/c/io.wpo').is_file()
finally:
 for p,data,s in backup:p.write_bytes(data);p.chmod(s.st_mode&0o777);os.utime(p,ns=(s.st_atime_ns,s.st_mtime_ns))
print('Native CS include and co-output regressions passed')
