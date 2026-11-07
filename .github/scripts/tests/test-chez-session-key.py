#!/usr/bin/env python3
"""CI seed follows compiler inputs, but not unrelated Racket or VCS changes."""
from pathlib import Path
import os,shutil,subprocess,sys,tempfile
SCRIPT=Path(__file__).resolve().parents[1]/'chez-session-key.zuo'
ZUO=Path(sys.argv[1]).resolve()
with tempfile.TemporaryDirectory(prefix='ci-chez-key-') as d:
 root=Path(d);chez=root/'racket/src/ChezScheme';chez.mkdir(parents=True)
 for name in ['configure','build.zuo','c/stats.c','s/library.ss','makefiles/lib.zuo','unicode/data','nanopass/nanopass.ss','nanopass/nanopass/helpers.ss','lz4/lib/lz4.c','zlib/zlib.h']:
  p=chez/name;p.parent.mkdir(parents=True,exist_ok=True);p.write_text(name)
 for name in ['petite.boot','scheme.boot','scheme.h','equates.h','gc-ocd.inc','gc-oce.inc','gc-par.inc','heapcheck.inc']:
  p=chez/'boot/pb'/name;p.parent.mkdir(parents=True,exist_ok=True);p.write_text(name)
 def key(context='configure=defaults',env=None):
  return subprocess.check_output([str(ZUO),str(SCRIPT),str(chez),'Linux/X64','fixed-image',context],env=env,text=True).strip()
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
 for name in ['c/stats.c','s/library.ss','makefiles/lib.zuo','boot/pb/petite.boot']:
  p=chez/name;old=p.read_text();p.write_text(old+' changed');assert key()!=original;p.write_text(old)
 # Checkout relocation does not alter this seed (upstream action scope may).
 moved=root/'relocated';shutil.copytree(chez,moved)
 other=subprocess.check_output([str(ZUO),str(SCRIPT),str(moved),'Linux/X64','fixed-image','configure=defaults'],text=True).strip();assert other==original
print('CI seed scope, config/source/PB invalidation, VCS exclusion, and unrelated-change checks passed.')
