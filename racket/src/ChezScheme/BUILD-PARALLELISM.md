# Chez Scheme build parallelism analysis

Date: 2026-02-09

## Summary
The Chez Scheme `make build` pipeline has substantial concurrency in the C compilation phase, but the overall wall time has historically been dominated by serialized Scheme bootstrapping. I reworked `bootall` to use the existing per‑file compilation targets (already modeled in `s/build.zuo`) so the bootstrap phase can run in parallel while preserving correctness and the fixpoint loop.

## Instrumentation (reusable)
I added reusable dependency-graph output to Zuo’s build system.

### New options
The following options are now supported by `zuo` builds:

- `--graph <path>` or env `ZUO_BUILD_GRAPH=<path>`: write a dependency graph.
- `--graph-format dot|edges` or env `ZUO_BUILD_GRAPH_FORMAT=dot|edges`.
- `--graph-append` or env `ZUO_BUILD_GRAPH_APPEND=1`: append to a graph file (edges format only). This is important because nested build invocations (e.g., `c/` and `s/` builds) run in separate build states. Append mode preserves all sub-graphs in one file.
- `ZUO_JOBSERVER_TRACE=<path>`: append debug logs for jobserver detection (e.g., parsed `MAKEFLAGS`, auth/fds mode, and whether a client was created). This is useful to verify GNU make jobserver propagation.

### Example command
```sh
cd racket/src/ChezScheme
make clean
ZUO_BUILD_GRAPH=/tmp/chez-build.edges \
ZUO_BUILD_GRAPH_FORMAT=edges \
ZUO_BUILD_GRAPH_APPEND=1 \
make -j 8 build
```

The resulting `/tmp/chez-build.edges` is an edge list with lines of the form:

```
<target>\t<dependency>
```

Lines without a tab represent isolated nodes.

## Experiments run
- `make clean`
- `ZUO_BUILD_GRAPH=/tmp/chez-build.edges ZUO_BUILD_GRAPH_FORMAT=edges ZUO_BUILD_GRAPH_APPEND=1 make -j 8 build`

Observed in the build output:
- The “all at once” Scheme compilation runs multiple times due to fixpoint checks:
  - `Exception in $fasl-file-equal?` appears, then `bootall` runs again.
  - The build stabilizes after several iterations.

## Graph summary
From `/tmp/chez-build.edges`:

- Nodes: 500
- Edges: 1207
- Longest dependency chain (depth 12) starts at `build` and goes through `ta6le/bin/ta6le/scheme` and the Scheme macro/boot pipeline.

Largest fan‑in (many dependents):
- `ta6le/boot/ta6le/scheme.h`
- `ta6le/boot/ta6le/equates.h`
- `ta6le/zlib/libz.a`
- `ta6le/lz4/lib/liblz4.a`

Largest fan‑out (many dependencies):
- `bootall` (96 deps, mostly `s/*.ss`)
- `ta6le/s/bootstrap` (92 deps)
- `ta6le/boot/ta6le/libkernel.a` (33 deps)
- `ta6le/s/nanopass.so` (21 deps)

## Dependency structure (s/build.zuo)
The Scheme build already encodes a dependency structure that we can exploit for parallelism:

- **Compiler preamble**:
  - `machine.def` → `cmacros.so` → `priminfo.so`.
  - `priminfo.so` → `primvars.so`, `env.so`, `setup.so`.
  - `mkheader.so` and `mkgc.so` depend on `cmacros.so`, `priminfo.so`, `primvars.so`, `env.so`.
- **Nanopass library**:
  - `nanopass.so` depends on the nanopass source tree.
- **Per‑file compilation**:
  - Each `base` and `compiler` object (`*.{m}`) depends on:
    - Its own `*.ss` source,
    - `nanopass.so`,
    - `macro‑objs` (`cmacros.so`, `priminfo.so`, `primvars.so`, `env.so`, `setup.so`),
    - `type-srcs` and explicit include dependencies (`:depend` for `cptypes`, `cpnanopass`, `cpprim`, etc.).
- **Boot files**:
  - `petite.boot` depends on `base-objs` (plus `xpatch` in cross builds).
  - `scheme.boot` depends on `compiler-objs` (plus `xpatch` in cross builds).
  - The boot file **load order** is determined by `base-src-names` and `compiler-names`. That ordering can remain intact even if compilation is parallelized, because the boot files are generated from the ordered object lists.

## Why parallelism is still limited
### 1. Kernel first, bootstrap second
In `racket/src/ChezScheme/build.zuo`, the `build` target depends on `kernel`, and then its rule explicitly runs `bootstrap` in the Scheme build tree:

- `build` depends on `kernel`.
- Only after `kernel` finishes, the rule calls `build` for `s/bootstrap`.

This creates a hard serialization point: Scheme bootstrapping never overlaps with kernel compilation.

### 2. Macro/preamble chain is still serial
The `cmacros.so → priminfo.so → primvars.so/env.so/setup.so → mkheader.so/mkgc.so` chain must remain serialized. It is smaller than the full bootstrap compile, but it is a hard dependency for everything else.

### 3. Fixpoint loop still repeats the work
The `bootstrap` target runs `look-for-fixpoint`, which can execute `bootall` multiple times. In practice, the build output shows multiple `Exception in $fasl-file-equal?` messages followed by a full recompile. This is repeated serialized work that further reduces effective parallelism.

### 4. Several Scheme `.so` steps are serialized
Targets like:
- `ta6le/s/cmacros.so`
- `ta6le/s/priminfo.so`
- `ta6le/s/primvars.so`
- `ta6le/s/env.so`
- `ta6le/s/setup.so`
- `ta6le/s/nanopass.so`

are built as separate `run-scheme` invocations with dependencies that form a mostly linear chain. They are comparatively smaller than `bootall` but still serialize the Scheme pipeline.

### 5. C compilation is reasonably parallelizable already
The C build defines each `.o` target independently and uses Zuo’s jobserver integration. With `make -j`, this phase can use multiple cores. The graph shows `ta6le/boot/ta6le/libkernel.a` depending on 30+ object files, plus `zlib` and `lz4` libraries. This stage is not the dominant serialization bottleneck.

## Bootall parallelization change
I reworked `bootall` to use the dependency structure above instead of a single “compile‑at‑once” Scheme process:

- `bootall` now calls a helper (`bootall-rebuild`) that:
  1. Runs `pretty-clean` to force a full rebuild for fixpoint correctness.
  2. Builds **all base + compiler objects** via the existing per‑file targets (parallelizable).
  3. Builds `petite.boot` and `scheme.boot` from those objects (still ordered inside the boot file).
- `bootstrap`’s fixpoint loop now calls `bootall-rebuild` directly to ensure each iteration actually rebuilds with the current compiler.

This preserves correctness because:
- The boot file ordering is unchanged (still based on `base-src-names` and `compiler-names`).
- Per‑file compilation already encodes include dependencies and macro preamble dependencies.
- Fixpoint still compares boot files after a full rebuild with the current compiler.

## Measured parallelism at `-j 4`
I added reusable scheme‑process tracing in `makefiles/lib.zuo` (env `SCHEME_BUILD_TRACE`).
Each scheme invocation logs `start/end` timestamps, which allows computing concurrency.

### Runs
All runs were clean builds in `racket/src/ChezScheme` with `make -j 4` and `ZUO_JOBS=4` to ensure Zuo uses multiple workers.

#### Baseline (all‑at‑once)
```
SCHEME_BUILD_TRACE=/tmp/chez-trace-at-once-jobs4b.log \
ZUO_JOBS=4 BOOTALL=at-once make -j 4 build
```
Results:
- Scheme invocations: **20**
- Max concurrent scheme processes: **4**
- Average concurrency: **~1.03**
- Scheme wall time (first start → last end): **~17.31s**

#### Parallel bootall (current default)
```
SCHEME_BUILD_TRACE=/tmp/chez-trace-par-jobs4.log \
ZUO_JOBS=4 make -j 4 build
```
Results:
- Scheme invocations: **203**
- Max concurrent scheme processes: **4**
- Average concurrency: **~2.40**
- Scheme wall time: **~13.91s**

### Interpretation
Compared to the all‑at‑once baseline, the parallelized bootall increases average scheme‑process concurrency by **~2.3×** (1.03 → 2.40) and reduces scheme‑phase wall time by **~20%** (17.31s → 13.91s). The max concurrency is capped at 4, as expected for `-j 4`.

### Important caveat
GNU make 4.4 uses `--jobserver-auth=fifo:/path` instead of numeric `--jobserver-fds=R,W`. Zuo’s jobserver client originally only handled numeric fd pairs, so it failed to detect the jobserver and fell back to a single job unless `ZUO_JOBS` was set.

I updated Zuo’s jobserver parsing and manager to handle `--jobserver-auth=fifo:` (and `pipe:`) by passing the auth string to the manager, which then opens the FIFO directly. After that change, Zuo picks up jobserver parallelism without `ZUO_JOBS`.

### Updated measurement (no `ZUO_JOBS`)
After the fix, a clean build with only `make -j 4`:

```
SCHEME_BUILD_TRACE=/tmp/chez-scheme-trace.log \
ZUO_JOBSERVER_TRACE=/tmp/zuo-jobserver-trace.log \
make -j 4 build
```

Results:
- Scheme invocations: **406**
- Max concurrent scheme processes: **4**
- Average concurrency: **~2.40**
- Scheme wall time: **~13.79s**

## Potential follow‑ups
These are design options; each has nontrivial tradeoffs.

1. Reduce fixpoint churn by improving determinism.
2. Increase parallelism among early `.so` steps (`primvars`, `env`, `setup` could be scheduled together once `priminfo` is ready).
3. Ensure jobserver propagation in sub‑makes (`zlib`, `lz4`).

## Key takeaway
The Scheme bootstrap pipeline is still constrained by a serial preamble chain and the fixpoint loop, but `bootall` no longer forces a single‑process compilation. Per‑file compilation can now run in parallel, which should substantially reduce bootstrap wall time on multi‑core machines while keeping the fixpoint logic intact.
