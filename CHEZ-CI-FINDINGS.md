# Findings from the recent-Chez CI replay

**With this implementation, only the documentation-only commit got a clear
speedup. The four code-changing commits did not avoid compilation.**

These are actual [Linux GitHub Actions measurements](https://github.com/samth/racket/actions/runs/42378505446),
not projections from the local probes. All five parent jobs and all ten target
comparisons succeeded. The latest five upstream commits at selection time were
replayed against their respective immediate parents.

| Commit | Change | Fresh bootstrap + native Racket | Parent-restored setup | Native cache hits |
|---|---|---:|---:|---:|
| `8403ef4c` | RISC-V/LoongArch foreign-call repair | 220.5 s | 284.0 s | 0/10 |
| `a8f47df0` | Release-note consolidation | 272.7 s | **6.4 s** | **10/10** |
| `68a0bc5e` | x86-64 ISA-level configuration | 251.8 s | 239.3 s | 0/10 |
| `ed9bbe77` | Empty continuation-stack marking repair | 209.7 s | 279.9 s | 0/10 |
| `d533f2e1` | Native fused multiply-add | 222.7 s | 288.8 s | 0/10 |

For the documentation commit, the entire target job fell from **329 s to 50 s**,
including artifact download/restoration: **279 seconds saved, or 84.8% less job
time**. Chez bootstrap took 1.6 s and restored native Racket compilation took
4.8 s. It ran zero bootstrap Scheme compilations, zero whole-bootstrap passes,
and zero native Racket compiler actions. All 19 restored native artifact hashes
matched the parent checkpoint. The compiler and all eight active boot/header
hashes also remained unchanged.

The four production-code commits changed the broad CI session key. The harness
therefore discarded their Chez workareas, as required when changing an explicit
key, and their new boot bytes invalidated native Racket action keys. Each repeated
177 bootstrap Scheme compilations, two full bootstrap passes, and all nine
native Racket library compilations plus whole-program compilation.

There was no corresponding cache benefit for those commits. Their observed
whole-job time differences ranged from 2.4% less time to 42.2% more time; do not
interpret those single-run differences as reliable improvements or causal
slowdowns. Builds with matching source/key and resulting compiler/boot hashes
also varied substantially between hosted jobs. More controlled repetitions would
be needed to attribute time differences when the amount of compilation is the
same. Cache-hit counts and executed work give the stronger conclusion here.

## What this says about the current seed scope

The RISC-V/LoongArch-only commit is an especially concrete example. Its x86-64
Chez executable was byte-identical to its parent's executable. Nevertheless,
the whole-source seed changed, both boot images changed, and all ten native
Racket actions missed. That is conservative namespace invalidation, not an
inherent requirement that every foreign-backend change rebuild x86-64.

The setup succeeds at reusing identical compiler inputs across unrelated changes.
It has not yet delivered code-changing Chez-commit wins. The measured next target
is finer per-platform/per-action Chez dependency and namespace scope; merely
making the current global seed deterministic does not provide that granularity.
No new cache policy or implementation was changed to manufacture a better result
in this benchmark.

## Scope and reproducibility

- Standard Ubuntu 22.04 hosted runners, image `ubuntu22/20261101.347.1`, four build
  jobs, immutable source SHAs. One cold/restored target pair per commit.
- Both arms use identical audited dependency fixes. The actual selected Chez
  checkout and pinned library submodules are materialized in Racket's bundled
  Chez path. The selected Racket PB payload is held constant.
- The fresh arm uses ordinary unseeded builds and no action cache. The restored
  arm uses the CI-only explicit key, parent build state, and native CS action
  cache; timestamp hash databases are discarded. Each native arm starts without
  existing .so/.wpo outputs.
- Every successful arm passes Racket's bootstrap consistency check and
  arithmetic/flfma smoke checks with its built Chez executable and loaded boots.
  All 19 generated native artifacts are inventoried. Full desktop tests were not
  run by this benchmark.
- A preceding parent's cache must already exist. Its preparation time and upload
  cost are recorded separately; it is not free target-PR work. A first cold run
  has no such benefit.
- Package setup, collection compilation, full desktop tests, macOS and iOS are
  outside these measurements. **The 84.8% result is not an end-to-end PR speedup.**

The [CI-generated JSON](benchmark-results/chez-ci-replay-42378505446.json) contains
all parent and target phase times, key decisions, cache/compiler counts, image
identity, output hashes, and job/step timings. The
[CI-generated table](benchmark-results/chez-ci-replay-42378505446.md) includes
whole-job savings for each pair. The workflow retains full evidence artifacts.
The separate collector saved these summaries as ordinary repository files because
this managed runtime's attempts to download Azure-hosted logs still received a
proxy CONNECT 403; no environment settings or proxy configuration were changed.

The benchmark workflow and runner are retained on `prototype/chez-ci-replay`;
the generated data and interpretation are on `prototype/chez-ci-replay-results`.
