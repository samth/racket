#!/usr/bin/env python3
"""Regression checks for the audited Chez Zuo dependencies (Linux native build).

Use an isolated source checkout and an existing native workarea produced by
configure/bootquick/build. This explicitly mutates sources/configuration, backs
up the workarea, and restores sources and outputs even on failure.
"""
from pathlib import Path
import argparse,json,os,re,shutil,subprocess,tempfile,time
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--source',type=Path,default=Path(__file__).resolve().parents[1])
p.add_argument('--workarea',type=Path,required=True,help='isolated native workarea, e.g. evidence/work/ta6le')
p.add_argument('--zuo',type=Path,required=True)
p.add_argument('--output',type=Path,required=True,help='new evidence directory, outside the workarea')
p.add_argument('--driver',type=Path,help='optional adapter for embedded workareas')
p.add_argument('--config-file',type=Path,help='configuration used by the adapter')
a=p.parse_args();src=a.source.resolve();work=a.workarea.resolve();out=a.output.resolve();out.mkdir(parents=True)
assert work.is_dir() and (work/'c/main.zuo').is_file() and out!=work and work not in out.parents
rows=[];command=[str(a.zuo.resolve()),str(a.driver.resolve()) if a.driver else '.','-j','1']
config_file=a.config_file.resolve() if a.config_file else work/'Mf-config'
def run(label,target,cwd,ok=True):
 t=time.monotonic();r=subprocess.run([*command,target],cwd=cwd,capture_output=True,text=True);text=r.stdout+r.stderr;(out/(label+'.log')).write_text(text)
 row=dict(label=label,status=r.returncode,seconds=time.monotonic()-t,c_compilations=text.count(' -c '),scheme_compilations=sum(line.startswith('compiling ') and ' with output to ' in line for line in text.splitlines()));rows.append(row);(out/'results.json').write_text(json.dumps(rows,indent=2)+'\n');print(row,flush=True)
 assert (r.returncode==0)==ok,text[-2400:]
 if not ok:
  configure_log=work/'zlib/configure.log'
  config_text=configure_log.read_text() if configure_log.is_file() else ''
  (out/(label+'-configure.log')).write_text(config_text)
  evidence=text+'\n'+config_text
  assert 'dependency-regression' in evidence and ('#error' in evidence or 'unrecognized option' in evidence),evidence[-2400:]
 return text
with tempfile.TemporaryDirectory(prefix='chez-dependency-baseline-') as d:
 backup=Path(d)/'workarea';shutil.copytree(work,backup,symlinks=True)
 def restore():
  shutil.rmtree(work);shutil.copytree(backup,work,symlinks=True)
  for path in work.rglob('_zuo_tc.db'):path.unlink()
 def mutation(path,fn,check):
  restore();original=path.read_bytes();st=path.stat()
  try:path.write_bytes(fn(original));check()
  finally:path.write_bytes(original);os.utime(path,ns=(st.st_atime_ns,st.st_mtime_ns));restore()
 # Every newly identified LZ4 source/header input must invalidate its archive.
 for name in ['lz4hc.h','xxhash.h','lz4frame_static.h','lz4file.h','lz4file.c']:
  mutation(src/'lz4/lib'/name,
           lambda b:b+b'\n#error dependency-regression-lz4\n',
           lambda n=name:run('lz4-'+n,'liblz4.a',work/'c',ok=False))
 blocked=out/'blocked.h';blocked.write_text('#error dependency-regression-CPPFLAGS\n')
 mutation(config_file,
          lambda b:re.sub(rb'^CPPFLAGS\s*=.*$',lambda m:m[0]+b' -include '+str(blocked).encode(),b,flags=re.M),
          lambda:run('cppflags-warm-reject','stats.o',work/'c',ok=False))
 mutation(config_file,
          lambda b:re.sub(rb'^LDFLAGS\s*=.*$',lambda m:m[0]+b' -Wl,--dependency-regression-link-option',b,flags=re.M),
          lambda:run('ldflags-warm-reject','scheme',work/'c',ok=False))
 mutation(config_file,
          lambda b:b+b'\nmdlinkflags=-Wl,--dependency-regression-link-option\n',
          lambda:run('link-only-flags-warm-reject','scheme',work/'c',ok=False))
 def iconv_check():
  run('disableiconv-regenerate','config.h',work/'c')
  assert b'#define DISABLE_ICONV' in (work/'c/config.h').read_bytes()
 mutation(config_file,lambda b:re.sub(rb'(?m)^((?:disableiconv|DISABLE_ICONV)\s*=\s*)no',rb'\1yes',b),iconv_check)
 restore();helper=work/'s/nanopass/helpers.so';helper.unlink()
 try:
  run('missing-nanopass-helper','nanopass.so',work/'s');assert helper.is_file()
  assert len(list((work/'s/nanopass').glob('*.so')))==13
 finally:restore()
 # The machine definition recipe also creates the architecture option file.
 if (src/'s/a6-options.def').is_file():
  restore();options=work/'s/a6-options.def';options.unlink()
  try:
   run('missing-machine-options','cmacros.so',work/'s');assert options.is_file()
  finally:restore()
 # Observe object mtimes after changing only the isolated header.
 header=src/'c/nocurses.h'
 def header_check():
  before={p.name:p.stat().st_mtime_ns for p in (work/'c').glob('*.o')}
  run('nocurses-narrow-rebuild','libkernel.a',work/'c')
  changed=[p.name for p in (work/'c').glob('*.o') if before[p.name]!=p.stat().st_mtime_ns]
  assert changed==['expeditor.o'],changed
  rows.append(dict(label='nocurses-changed-objects',objects=changed));(out/'results.json').write_text(json.dumps(rows,indent=2)+'\n')
 mutation(header,lambda b:b+b'\n/* dependency regression comment */\n',header_check)
print('All audited Chez invalidation, output-group, and header-precision regressions passed.')
