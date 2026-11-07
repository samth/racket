# Recent Chez commits: CI replay benchmark

This branch runs the selected RISC-V/LoongArch upstream commit (listed by immutable SHA in
`.github/scripts/chez-replay-revisions.json`) through the Linux PR build's
Chez/PB bootstrap and native Racket CS compilation boundaries. It does not time
package setup, full desktop test execution, or macOS/iOS jobs.

For each target commit, a parent job builds its immediate parent with the
explicit CI session key and the audited Zuo native action cache. It archives the
complete build state and action-cache directory. Two comparison jobs then build
the target: a fresh unseeded build without the action cache, and a seeded build
restoring its parent state/cache. Each native Racket arm starts without .so/.wpo
outputs. Only real command-rule hits can avoid native Racket compilation.

The retained setup does not cache Chez compiler actions. If its source
session key changes, the Chez workarea is discarded, as upstream key changes are
not compatible with retained incremental state. Changes confined to an unused backend retain the scoped key. Restored timestamp hash databases are discarded in either case.
The ordinary Zuo bootstrap recipe is still executed, including any phony steps.

Both arms include identical dependency fixes, applied as a three-way patch to
the selected upstream Chez tree. The actual selected Chez sources and pinned
nanopass/zlib/lz4 are copied into Racket's bundled source path. Racket's selected
PB seed payload is held constant across comparisons. Each native compiler runs
with its actual loaded boots, passes Racket's bootstrap-consistency check, and
runs arithmetic/flfma smoke checks. Native Racket actions hash that executable
and loaded boots as well as declared sources/configuration.

The hosted workflow uses separate standard Ubuntu 22.04 jobs at four build jobs,
records runner image identity, and uploads full logs and compact JSON results.
Source/seed retrieval and artifact transfers are separate workflow steps. Report
build-phase time and target-job elapsed time separately; the latter includes
cache download/restoration. Parent preparation is the preceding cache-warming
cost, not free work done during the target PR. A first run has no parent cache.

Each comparison is one sample per commit on hosted runners, so small differences
are subject to scheduling and hardware noise. No end-to-end PR speedup should be
inferred directly from these build-boundary timings. Code changes may receive no
benefit with this namespace; the benchmark must report that
rather than assume reproducibility alone skips compilation.

The runner is `.github/scripts/chez-ci-replay.py`; it explicitly requires
`--allow-replace-bundled` and must only be used in disposable checkouts. It
replaces bundled sources, maintains a fixed build path, and records every phase
status, elapsed time, compiler work, cache counts and output hashes. The workflow
is restricted to this benchmark branch and supports manual retries.

## Target-specific dependency fix

This branch replays only `8403ef4c` against `048879e3`, with the same fixed PB
payload and native action-cache setup. The seed explicitly selects `pb,ta6le`.
Both arms include the new target-specific dependency rules in addition to the
previous audit fixes. The new patch is applied separately to avoid changing
unrelated upstream ISA-level configuration behavior.

A separate verification job forces a fresh seeded target bootstrap at the same
paths, then requires the compiler, all eight boot/header files, and all 19 native
outputs to match the parent byte for byte. It also requires all ten native cache
hits. This checks that excluding the unused backend preserves freshly compiled
output, rather than merely trusting previously restored files. The warm reuse
arm additionally requires zero bootstrap and native compiler actions.

Mutation regressions poison RISC-V and LoongArch backend files: x86-64 must still
build with zero compiler actions. Poisoning x86_64.ss must invalidate the build
and fail while opening that backend. The tests discard timestamp databases and
restore sources/build state. Key-scope regressions require the changed backend
to invalidate its own target key, and check common/PB/toolchain inputs. A graph
selector test covers eleven machine variants, common and unknown source files,
and compatibility of the complete aggregate source list.
