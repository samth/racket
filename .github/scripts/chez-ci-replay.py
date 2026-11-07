#!/usr/bin/env python3
"""Replay actual Chez revisions through Racket's PB/bootstrap + native CS build.

DESTRUCTIVE within the specified disposable checkout: replaces bundled Chez.
Both arms include the audited dependency fixes. Only reuse enables the explicit
CI key, restored state, and the existing native CS action cache. No Chez action
cache is invented: when its key changes, its workarea is discarded.
"""
from pathlib import Path
import argparse,hashlib,json,os,re,shutil,subprocess,time
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--root',type=Path,default=Path.cwd())
p.add_argument('--chez',type=Path,required=True)
p.add_argument('--revision',required=True)
p.add_argument('--mode',choices=['prepare','cold','reuse','rebuild'],required=True)
p.add_argument('--name',required=True)
p.add_argument('--jobs',default='4')
p.add_argument('--zuo',type=Path)
p.add_argument('--allow-replace-bundled',action='store_true',required=True)
a=p.parse_args();root=a.root.resolve();chez=a.chez.resolve();state=root/'replay-state';evidence=root/'replay-evidence'/a.name;evidence.mkdir(parents=True,exist_ok=True)
zuo=a.zuo.resolve() if a.zuo else root/'racket/src/build/bin/zuo'
build=state/'build';cs=build/'cs/c';native=state/'native';cache=state/'action-cache';bundle=root/'racket/src/ChezScheme';selection=json.loads((root/'.github/scripts/chez-replay-revisions.json').read_text());rows=[]
sha=lambda p:hashlib.sha256(Path(p).read_bytes()).hexdigest()
def run(label,args,cwd=root,env=None):
 start=time.monotonic()
 with (evidence/(label+'.log')).open('w') as f:r=subprocess.run(list(map(str,args)),cwd=cwd,env=env,stdout=f,stderr=subprocess.STDOUT)
 log=(evidence/(label+'.log')).read_text(errors='replace')
 row=dict(label=label,seconds=time.monotonic()-start,status=r.returncode,
          c_compilations=sum(' -c ' in line for line in log.splitlines()),
          scheme_compilations=sum(line.startswith('compiling ') and ' with output to ' in line for line in log.splitlines()),
          full_scheme_passes=log.count('to build all at once'),cache_hits=log.count('action cache hit '),cache_stores=log.count('action cache stored '))
 rows.append(row);(evidence/'phases.json').write_text(json.dumps(rows,indent=2)+'\n')
 assert r.returncode==0,(label,log[-3500:]);return row
start_all=time.monotonic()
# Sources and pinned library checkouts are materialized in the actual bundled path.
run('checkout',['git','reset','--hard',a.revision],cwd=chez)
run('submodules',['git','submodule','update','--init','--recursive','nanopass','zlib','lz4'],cwd=chez)
run('dependency-fixes',['git','apply','--3way',root/'.github/scripts/chez-replay-dependency-fixes.patch'],cwd=chez)
run('platform-fixes',['git','apply',root/'.github/scripts/chez-replay-platform-fixes.patch'],cwd=chez)
seed_payload=evidence/'pb-seeds';shutil.copytree(bundle/'boot/pb',seed_payload,ignore=shutil.ignore_patterns('.git'))
shutil.rmtree(bundle);shutil.copytree(chez,bundle,ignore=shutil.ignore_patterns('.git','stex','zuo','__pycache__'))
shutil.rmtree(bundle/'boot/pb',ignore_errors=True);(bundle/'boot').mkdir(exist_ok=True);shutil.copytree(seed_payload,bundle/'boot/pb')
platform=os.environ.get('RUNNER_OS','Linux')+'/'+os.environ.get('RUNNER_ARCH',os.uname().machine)
image=os.environ.get('ImageOS','local')+'/'+os.environ.get('ImageVersion','frozen')
assert not os.environ.get('RUNNER_OS') or os.environ.get('ImageVersion'), 'Hosted cache experiment requires the runner image identity'
flags=['--enable-cs','--disable-curses','--disable-gracket','--disable-docs']
key_args=[str(zuo),str(root/'.github/scripts/chez-session-key.zuo'),str(bundle),platform,image,'--machines=pb,ta6le',' '.join(flags)]
t=time.monotonic();key=subprocess.check_output(key_args,cwd=root,text=True).strip();key_seconds=time.monotonic()-t
parent_meta=json.loads((state/'parent.json').read_text()) if a.mode in ('reuse','rebuild') else None
seed_matches=bool(parent_meta and parent_meta['session_key']==key)
# A changed explicit key is not compatible with an incrementally retained Chez
# workarea. Keep the native Racket cache, but rebuild Chez from scratch.
if a.mode=='cold':shutil.rmtree(state,ignore_errors=True)
state.mkdir(exist_ok=True)
if a.mode=='rebuild' or (a.mode=='reuse' and not seed_matches):shutil.rmtree(cs/'ChezScheme',ignore_errors=True)
if a.mode=='rebuild':(cs/'check-bootstrap-same-stamp').unlink(missing_ok=True)
for f in state.rglob('_zuo_tc.db'):f.unlink()
# Every job starts without native Racket outputs, so reuse has to demonstrate
# real action-cache hits, not just existing-file incremental reuse.
shutil.rmtree(native,ignore_errors=True);native.mkdir()
env=dict(os.environ)
if a.mode!='cold':env['RACKET_CHEZ_SESSION_KEY']=key
else:env.pop('RACKET_CHEZ_SESSION_KEY',None)
# Image identity is explicit in this hosted experiment; compiler and loaded
# boot bytes are additionally hashed by the native command-rule integration.
toolchain=hashlib.sha256(json.dumps([platform,image,sha(zuo)],sort_keys=True).encode()).hexdigest()
if a.mode!='cold':env.update(ZUO_CS_ACTIONS='1',ZUO_CS_TOOLCHAIN_ID=toolchain,ZUO_ACTION_CACHE=str(cache),ZUO_ACTION_CACHE_LOG='1')
else:
 for k in ['ZUO_CS_ACTIONS','ZUO_ACTION_CACHE','ZUO_CS_TOOLCHAIN_ID']:env.pop(k,None)
if not (cs/'Makefile').is_file():
 build.mkdir(exist_ok=True);run('configure',[root/'racket/src/configure',*flags],cwd=build,env=env)
bootstrap=run('chez-bootstrap',[zuo,'-X',root/'racket/src/zuo/lib','.', '-j',a.jobs,'scheme'],cwd=cs,env=env)
assert (cs/'check-bootstrap-same-stamp').is_file()
area=cs/'ChezScheme/ta6le'
racket=run('racket-native',[zuo,'-X',root/'racket/src/zuo/lib',root/'.github/scripts/chez-replay-native.zuo','-j',a.jobs,'NATIVE='+str(area)],cwd=native,env=env)
# Each compiler is executed with its actual loaded boot files for a runtime check.
smoke=evidence/'smoke.ss';smoke.write_text('''(import (chezscheme))
(unless (= (+ 20 22) 42) (error 'smoke "arithmetic"))
(unless (= (flfma 2.0 3.0 4.0) 10.0) (error 'smoke "flfma"))
(display "chez-replay-smoke: ok")(newline)
''')
run('chez-smoke',[area/'bin/ta6le/scheme','-B',area/'boot/ta6le/petite.boot','-B',area/'boot/ta6le/scheme.boot','--script',smoke],env=env)
boots={n:sha(area/'boot/ta6le'/n) for n in ['petite.boot','scheme.boot','scheme.h','equates.h','gc-ocd.inc','gc-oce.inc','gc-par.inc','heapcheck.inc']}
artifacts={p.name:sha(p) for p in native.iterdir() if p.suffix in ['.so','.wpo']};assert len(artifacts)==19
revision=subprocess.check_output(['git','rev-parse','HEAD'],cwd=chez,text=True).strip()
summary=dict(revision=revision,mode=a.mode,name=a.name,session_key=key,seed_matches_parent=seed_matches,
             parent_revision=parent_meta['revision'] if parent_meta else None,platform=platform,image=image,
             key_seconds=key_seconds,chez_seconds=bootstrap['seconds'],racket_native_seconds=racket['seconds'],
             build_seconds=bootstrap['seconds']+racket['seconds'],elapsed_seconds=time.monotonic()-start_all,
             native_cache_hits=racket['cache_hits'],native_cache_stores=racket['cache_stores'],
             bootstrap_compilations=bootstrap['scheme_compilations'],bootstrap_full_passes=bootstrap['full_scheme_passes'],
             native_scheme_compilations=racket['scheme_compilations'],boot_hashes=boots,native_hashes=artifacts,
             compiler_sha256=sha(area/'bin/ta6le/scheme'),zuo_sha256=sha(zuo),jobs=int(a.jobs))
if a.mode in ('reuse','rebuild'):
 assert seed_matches, 'The foreign-backend change must retain the scoped key'
 assert boots==parent_meta['boot_hashes'], 'Boot bytes changed after an irrelevant backend change'
 assert artifacts==parent_meta['native_hashes'], 'Restored native bytes differ from parent'
 assert summary['compiler_sha256']==parent_meta['compiler_sha256']
 assert racket['cache_hits']==10 and racket['scheme_compilations']==0
 if a.mode=='reuse':
  assert bootstrap['scheme_compilations']==0 and bootstrap['full_scheme_passes']==0
 else:
  assert bootstrap['scheme_compilations']>0 and bootstrap['full_scheme_passes']>0
(evidence/'summary.json').write_text(json.dumps(summary,indent=2)+'\n')
if a.mode=='prepare':
 (state/'parent.json').write_text(json.dumps(summary,indent=2)+'\n')
 shutil.rmtree(native)
print(json.dumps(summary,indent=2),flush=True)
