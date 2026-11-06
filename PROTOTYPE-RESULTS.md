# Zuo command-cache prototype: local evidence

Base: Racket upstream `a66730d537`. Tested on Linux x86-64, 2026-11-06,
with a fixed source/build root and prebuilt native Chez. The implementation and
native pilot are opt in. Correctness fixes for rktio configuration and native
WPO dependencies apply without caching. The original PR workflow is unchanged.

| Native CS compilation probe | Wall time | Cache restores | Compiler actions executed |
|---|---:|---:|---:|
| Cold | 95.49 s | 0 | 10 |
| Warm incremental | 0.61 s | 0 | 0 |
| Empty build directory, no Zuo databases | 5.78 s | 10 | 0 |
| Delete `main.wpo` | 1.33 s | 1 | 0 |
| Corrupt `chezpart.wpo` | 0.86 s | 1 | 0 |

The ten actions produce 19 files: nine `.so`/`.wpo` pairs and final `racket.so`.
All output hashes matched before/after restore. The restored whole-program
Racket passed hash, regexp and thread/channel runtime smoke checks with a
standalone rktio. This measures an actual pure-Zuo cache implementation, not the
previous external bundle-copy experiment. It excludes bootstrap, source
schemification, C compilation, package setup, networking and test execution.
Single local runs overlapped other validation work and are not GitHub runner
speedup estimates. The initial version conservatively fingerprinted every Zuo
library source file; the dependency fixes below replace that with loaded-module
tracking. An earlier narrower pilot measured 75.99 s cold and
3.67 s restore; this is not a controlled comparison of hashing overhead.
Compact timing rows, output hashes and tool fingerprints are committed in
`prototype-evidence/`; complete logs remain in the local investigation workspace.

The standalone rktio check built 26 objects, rebuilt zero on a warm run, and
rebuilt all 26 after changing `CFLAGS` from `-O2` to `-O0` in its generated
Makefile. The prior rules rebuilt zero after that configuration change.

`tests/action-cache.py` passed command, environment, toolchain, source, actual
executable-byte and producer-file invalidation; missing co-outputs; empty build
state; corrupt blobs; malformed metadata; restored executable modes; failed,
incomplete and input-mutating commands; disabled storage; and six simultaneous
cache writers in separate build directories. The existing Zuo test suite passed.

Reproduce the native timing/identity probe from `racket/src/zuo`:

```sh
python3 tests/cs-action-cache.py \
  --zuo /absolute/path/to/prototype/zuo \
  --native /absolute/native/ChezScheme/ta6le \
  --toolchain-id your-fixed-runner-image-sysroot-platform
```

It retains logs, exit statuses, timing rows and all 19 output hashes. It uses
checked-in schemified sources and explicitly loaded native boots. See
`racket/src/zuo/ACTION-CACHE-PROTOTYPE.md` for the API, native opt-in variables,
input-contract requirements and limitations. No remote transport is implemented;
the local directory can be moved by an external trusted cache transport, but
that integration still needs its own correctness/performance evaluation.


## Dependency-audit fixes, 2026-11-06

The audited false hits are fixed: native I/O compilation now declares
`rktio/rktio.rktl`, and linklet compilation declares `cs/linklet/trace.ss`.
Mutation regressions reject invalid edits both with existing outputs and from
an empty build directory with cache storage enabled. The unchanged prefixes
still restore (three and six hits respectively), then the affected action
misses and reports the expected compiler reader error.

Zuo now reports files actually read by its module loader in this runtime.
Command actions track that set instead of scanning all library files. A changed
loaded helper invalidates the action; editing an unused hygienic-language module
keeps the native build up to date. Explicit child-tool inputs are still required;
this does not add arbitrary file-access discovery or sandboxing.

A normal empty-directory restore still gets ten hits, zero executions, and all
19 original artifact hashes (5.47 seconds locally during concurrent validation).
The restored Racket passed hash, regexp, and thread/channel smoke checks. The
cache integration tests, including loaded-helper/unused-module checks, and the
existing Zuo test suite passed. Historical cold timings above are not a paired
performance comparison for this update.

The bundled Chez changes declare LZ4's complete source/build-script tree,
compile/archive/library/link configuration, and all 14 Nanopass compilation
outputs. They also track disableiconv in config.h and restrict nocurses.h to
expeditor.o. Nested library rebuilds start with clean trees and omit VCS metadata,
preventing retained old-flag objects and read-only Git-pack recopy failures.
Both the bundled and standalone Chez builds and mutation regressions passed.
The header edit now rebuilds **one object**, versus 31 in the audit.

Compact evidence is in `prototype-evidence/dependency-fixes-*.json`; full logs
remain in `/workspace/prototypes/checks/dependency-fixes/`. Reproduce the native
mutation checks after `tests/cs-action-cache.py` with
`tests/cs-dependency-regressions.py`; reproduce the bundled checks with
`racket/src/ChezScheme/tools/dependency-regressions.py`. These mutation tests
require isolated sources and native workareas and restore edits in finally
blocks. Validation remains Linux-native; no hosted CI performance claim is made.
