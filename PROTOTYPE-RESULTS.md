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
speedup estimates. This final version conservatively fingerprints every Zuo
library source file. An earlier narrower pilot measured 75.99 s cold and
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
