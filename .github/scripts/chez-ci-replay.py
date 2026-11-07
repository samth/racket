#!/usr/bin/env python3
"""Replay actual Chez revisions through Racket's PB/bootstrap + native CS build.

DESTRUCTIVE within the specified disposable checkout: replaces bundled Chez.
Both arms include the audited dependency fixes. Seeded arms use one literal CI
base and the native CS action cache. Source invalidation belongs to Zuo; build
checkpoint compatibility is checked independently of the namespace.
"""
from pathlib import Path
import argparse,hashlib,json,os,re,shlex,shutil,subprocess,time
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--root',type=Path,default=Path.cwd())
p.add_argument('--chez',type=Path,required=True)
p.add_argument('--revision',required=True)
p.add_argument('--mode',choices=['prepare','cold','reuse','rebuild'],required=True)
p.add_argument('--name',required=True)
p.add_argument('--jobs',default='4')
p.add_argument('--zuo',type=Path)
p.add_argument('--reference-dir',type=Path,help='reuse-arm summary to check a fresh seeded target against')
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
platform_patch='chez-replay-platform-modern.patch' if 'x86-64-level' in (chez/'s/machine.zuo').read_text() else 'chez-replay-platform-fixes.patch'
run('platform-fixes',['git','apply',root/'.github/scripts'/platform_patch],cwd=chez)
run('compiler-inputs',['git','apply',root/'.github/scripts/chez-replay-compiler-inputs.patch'],cwd=chez)
seed_payload=evidence/'pb-seeds';shutil.copytree(bundle/'boot/pb',seed_payload,ignore=shutil.ignore_patterns('.git'))
shutil.rmtree(bundle);shutil.copytree(chez,bundle,ignore=shutil.ignore_patterns('.git','stex','zuo','__pycache__'))
shutil.rmtree(bundle/'boot/pb',ignore_errors=True);(bundle/'boot').mkdir(exist_ok=True);shutil.copytree(seed_payload,bundle/'boot/pb')
platform=os.environ.get('RUNNER_OS','Linux')+'/'+os.environ.get('RUNNER_ARCH',os.uname().machine)
image=os.environ.get('ImageOS','local')+'/'+os.environ.get('ImageVersion','frozen')
assert not os.environ.get('RUNNER_OS') or os.environ.get('ImageVersion'), 'Hosted cache experiment requires the runner image identity'
flags=['--enable-cs','--disable-curses','--disable-gracket','--disable-docs']
t=time.monotonic();key=(root/'.github/chez-session-key').read_text().strip();key_seconds=time.monotonic()-t
assert key and '\n' not in key, 'CI namespace must be one nonempty literal'
# Checkpoint compatibility is separate from the gensym namespace. Source changes
# are deliberately absent: their invalidation belongs to the Zuo graph.
cc=os.environ.get('CC','cc');cc_path=shutil.which(shlex.split(cc)[0]);assert cc_path
build_context=hashlib.sha256(json.dumps([platform,image,sha(zuo),cc,sha(cc_path),flags,
 {k:os.environ.get(k,'') for k in ['CPPFLAGS','CFLAGS','LDFLAGS','AR','ARFLAGS','RANLIB','CC_FOR_BUILD','CFLAGS_FOR_BUILD']}],sort_keys=True).encode()).hexdigest()
parent_meta=json.loads((state/'parent.json').read_text()) if a.mode in ('reuse','rebuild') else None
seed_matches=bool(parent_meta and parent_meta['session_key']==key)
context_matches=bool(parent_meta and parent_meta['build_context_id']==build_context)
if a.mode=='cold':shutil.rmtree(state,ignore_errors=True)
state.mkdir(exist_ok=True)
# A policy-version rotation or incompatible toolchain needs a fresh workarea.
# Keep the independently keyed native action cache.
if a.mode=='rebuild' or (a.mode=='reuse' and not (seed_matches and context_matches)):
 shutil.rmtree(build,ignore_errors=True)
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
# Runtime validation uses a shared rktio built outside the timed compiler phases.
rktio=state/'rktio-shared';rktio.mkdir(exist_ok=True)
if not (rktio/'Makefile').exists():
 run('rktio-configure',[root/'racket/src/rktio/configure','--enable-standalone','--prefix='+str(root/'racket')],cwd=rktio,env=env)
run('rktio-shared',[zuo,'-X',root/'racket/src/zuo/lib','.', 'installed-shared-object'],cwd=rktio,env=env)
init_args=['false',str(root/'racket/bin/racket'),str(root/'racket/bin/racket'),str(root/'racket/collects'),str(root/'racket/etc'),'','0','false','false','0','']
smoke_env=dict(env,RACKET_IO_SOURCE_DIR=str(root/'racket/src/cs'))
run('native-racket-smoke',[area/'bin/ta6le/scheme','-B',area/'boot/ta6le/petite.boot','-B',area/'boot/ta6le/scheme.boot','--script',native/'racket.so',*init_args,'-I','racket/base','-q','-e','(begin (unless (= (+ 20 22) 42) (error "arithmetic")) (let ([h (hash \'x (vector 1 2 3))]) (unless (= (vector-ref (hash-ref h \'x) 1) 2) (error "hash/vector"))) (displayln "native-racket-smoke: ok"))'],env=smoke_env)
assert 'native-racket-smoke: ok' in (evidence/'native-racket-smoke.log').read_text()
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
             build_context_id=build_context,build_context_matches_parent=context_matches,
             key_seconds=key_seconds,chez_seconds=bootstrap['seconds'],racket_native_seconds=racket['seconds'],
             build_seconds=bootstrap['seconds']+racket['seconds'],elapsed_seconds=time.monotonic()-start_all,
             native_cache_hits=racket['cache_hits'],native_cache_stores=racket['cache_stores'],
             bootstrap_compilations=bootstrap['scheme_compilations'],bootstrap_full_passes=bootstrap['full_scheme_passes'],
             native_scheme_compilations=racket['scheme_compilations'],boot_hashes=boots,native_hashes=artifacts,
             compiler_sha256=sha(area/'bin/ta6le/scheme'),zuo_sha256=sha(zuo),jobs=int(a.jobs))
expect_unchanged=next((r.get('expect_unchanged',False) for r in selection['revisions'] if r['sha']==revision),False)
if a.mode in ('reuse','rebuild'):
 assert seed_matches, 'The fixed namespace must survive source changes'
 assert context_matches, 'The comparison requires matching build/toolchain context'
 if expect_unchanged and a.mode=='reuse':
  assert boots==parent_meta['boot_hashes']
  assert artifacts==parent_meta['native_hashes']
  assert summary['compiler_sha256']==parent_meta['compiler_sha256']
  assert racket['cache_hits']==10 and racket['scheme_compilations']==0
  if a.mode=='reuse':
   assert bootstrap['scheme_compilations']==0 and bootstrap['full_scheme_passes']==0
 if a.mode=='rebuild':
  assert bootstrap['scheme_compilations']>0 and bootstrap['full_scheme_passes']>0
  assert a.reference_dir, 'Fresh validation requires a measured incremental target'
  candidates=[(p,json.loads(p.read_text())) for p in a.reference_dir.rglob('summary.json')]
  matches=[(p,r) for p,r in candidates if r['revision']==revision and r['mode']=='reuse']
  assert len(matches)==1, 'Expected one incremental target reference'
  reference_path,reference=matches[0]
  pairs=[]
  for name,h in reference['native_hashes'].items():
   other=reference_path.parent/'native-outputs'/name
   assert other.is_file() and sha(other)==h, 'Missing or changed reference output: '+name
   pairs.append((str(other.resolve()),str(native/name)))
  compare=evidence/'compare-native.ss'
  compare.write_text('(import (chezscheme))\n'+''.join('(unless (#%$fasl-file-equal? '+json.dumps(x)+' '+json.dumps(y)+' #t #t) (error \'equivalence "mismatch"))\n' for x,y in pairs))
  run('native-fasl-equivalence',[area/'bin/ta6le/scheme','-B',area/'boot/ta6le/petite.boot','-B',area/'boot/ta6le/scheme.boot','--script',compare],env=env)
  summary['incremental_reference']=reference['name']
  summary['native_byte_identical']=artifacts==reference['native_hashes']
  summary['native_byte_matches']=[n for n in artifacts if artifacts[n]==reference['native_hashes'][n]]
  summary['native_comparison']='Chez fasl-file-equal?; code/data/sharing checked with private gensym renaming'
  summary['equivalence']={
   'compiler':summary['compiler_sha256']==reference['compiler_sha256'],
   'boots':boots==reference['boot_hashes'],
   'native':True}
  assert all(summary['equivalence'].values()), summary['equivalence']
if a.mode=='reuse':
 out=evidence/'native-outputs';out.mkdir()
 for name in artifacts:shutil.copyfile(native/name,out/name)
(evidence/'summary.json').write_text(json.dumps(summary,indent=2)+'\n')
if a.mode=='prepare':
 (state/'parent.json').write_text(json.dumps(summary,indent=2)+'\n')
 shutil.rmtree(native)
print(json.dumps(summary,indent=2),flush=True)
