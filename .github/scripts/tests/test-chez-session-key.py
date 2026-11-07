#!/usr/bin/env python3
"""CI seed follows compiler inputs, but not unrelated Racket or VCS changes."""
from pathlib import Path
import os,shutil,subprocess,sys,tempfile
SCRIPT=Path(__file__).resolve().parents[1]/'chez-session-key.zuo'
ZUO=Path(sys.argv[1]).resolve()
with tempfile.TemporaryDirectory(prefix='ci-chez-key-') as d:
 root=Path(d);chez=root/'racket/src/ChezScheme';chez.mkdir(parents=True)
 for name in ['configure','build.zuo','c/stats.c','s/library.ss','s/x86_64.ss','s/riscv64.ss','s/loongarch64.ss','s/arm64.ss','s/pb.ss','s/new-backend.ss','makefiles/lib.zuo','unicode/data','nanopass/nanopass.ss','nanopass/nanopass/helpers.ss','lz4/lib/lz4.c','zlib/zlib.h']:
  p=chez/name;p.parent.mkdir(parents=True,exist_ok=True);p.write_text(name)
 # Exercise the real selector used by the build graph, not a duplicate mapping.
 source=SCRIPT.parents[2]/'racket/src/ChezScheme'
 for name in ['s/machine.zuo','makefiles/lib.zuo']:
  shutil.copyfile(source/name,chez/name)
 for name in ['petite.boot','scheme.boot','scheme.h','equates.h','gc-ocd.inc','gc-oce.inc','gc-par.inc','heapcheck.inc']:
  p=chez/'boot/pb'/name;p.parent.mkdir(parents=True,exist_ok=True);p.write_text(name)
 def key(context='configure=defaults',env=None,machines='pb,ta6le'):
  return subprocess.check_output([str(ZUO),str(SCRIPT),str(chez),'Linux/X64','fixed-image',*(['--machines='+machines] if machines is not None else []),context],env=env,text=True).strip()
 original=key();assert original==key() and original.startswith('ci-chez-')
 unrelated=root/'racket/collects/example.rkt';unrelated.parent.mkdir();unrelated.write_text('first');assert key()==original
 unrelated.write_text('second');assert key()==original
 git=chez/'zlib/.git';git.mkdir();(git/'HEAD').write_text('different revision');assert key()==original
 # A compiler wrapper's bytes are part of the namespace.
 compiler=root/'compiler';compiler.write_text('#!/bin/sh\nexit 0\n');compiler.chmod(0o755)
 compiler_env=dict(os.environ,CC=str(compiler));compiler_key=key(env=compiler_env)
 compiler.write_text('#!/bin/sh\nexit 1\n');assert key(env=compiler_env)!=compiler_key
 assert key('configure=debug')!=original
 childenv=dict(os.environ,CFLAGS='-O0');assert key(env=childenv)!=original
 for name in ['c/stats.c','s/library.ss','makefiles/lib.zuo','s/machine.zuo','boot/pb/petite.boot']:
  p=chez/name;old=p.read_text();p.write_text(old+'\n; changed\n');assert key()!=original;p.write_text(old)
 # Foreign backends do not perturb x86-64, but do perturb their own targets.
 for backend,machine in [('riscv64','trv64le'),('loongarch64','tla64le'),('arm64','tarm64le')]:
  targetkey=key(machines='pb,'+machine);crosskey=key(machines='pb,ta6le,'+machine);broad=key(machines=None)
  p=chez/'s'/(backend+'.ss');old=p.read_text();p.write_text(old+'\n; changed\n')
  assert key()==original
  assert key(machines='pb,'+machine)!=targetkey
  assert key(machines='pb,ta6le,'+machine)!=crosskey
  assert key(machines=None)!=broad
  p.write_text(old)
 for backend in ['x86_64','pb','new-backend']:
  p=chez/'s'/(backend+'.ss');old=p.read_text();p.write_text(old+'\n; changed\n')
  assert key()!=original
  p.write_text(old)
 assert key(machines='pb,tarm64osx')!=original
 # Checkout relocation does not alter this seed (upstream action scope may).
 moved=root/'relocated';shutil.copytree(chez,moved)
 other=subprocess.check_output([str(ZUO),str(SCRIPT),str(moved),'Linux/X64','fixed-image','--machines=pb,ta6le','configure=defaults'],text=True).strip();assert other==original
print('Machine selection, cross-target invalidation, CI seed scope, config/source/PB invalidation, VCS exclusion, and unrelated-change checks passed.')
