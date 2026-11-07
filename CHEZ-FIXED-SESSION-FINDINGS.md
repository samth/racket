# Fixed CI session base and correct Zuo dependencies

CI now passes the literal **`racket-chez-ci-v1`** from `.github/chez-session-key`. The source-hashing helper and its source/backend exclusion model are removed. Chez’s existing per-action/workarea derivation is unchanged; normal builds retain upstream defaults.

The implementation is on [samth/racket](https://github.com/samth/racket/tree/prototype/ci-boot-session-key) (component commit `987d4fbd65`) and [samth/ChezScheme](https://github.com/samth/ChezScheme/tree/prototype/ci-boot-session-key) (`9bc7156c`). `prototype/platform-chez-dependencies` and the earlier boot prototype names are updated aliases.

## Dependency corrections exposed by a constant base

The first shared-Scheme replay failed during incremental bootstrap with a sharing mismatch followed by a duplicate interned gensym. A global source-derived namespace had previously discarded the entire Chez workarea, hiding stale compiler-preparation artifacts. The [failed run and diagnostics](benchmark-results/chez-ci-replay-42389568087.json) are retained.

The fix keeps the same constant base and corrects the build graph:

- The bootstrap stamp, compiler preambles and Nanopass library group track the native compiler executable and incoming boot files. The active boot files replaced during fixpoint iteration are distinct from these seed inputs, avoiding a dependency cycle.
- Nanopass removes and regenerates its complete 14-file output group. Its internal imported-library timestamp checks cannot retain child libraries compiled by an older compiler.
- Selected-backend dependencies remain in Zuo. They do not participate in choosing the CI namespace.
- Compiler/platform/configuration compatibility and native action-cache identity remain separate from the namespace. Source changes invalidate through the graph.

## Final hosted verification

All **13 jobs passed** in [run 42393887465](https://github.com/samth/racket/actions/runs/42393887465), measured at immutable replay revision `eec26666f6`. The selected upstream Chez sources and pinned libraries were explicitly copied into Racket’s bundled source path.

For each of three real commits—unused foreign backend, shared C runtime, shared Scheme compiler—an incremental target was compared with an independent fresh target using the same constant base. The compiler executable and all eight boot/header files matched **byte for byte** in every pair.

All 19 native Racket outputs passed Chez’s own FASL code/data/sharing comparison. This comparison allows private gensym unique-name changes; it is not a claim that every independently compiled native file has identical bytes. The C/S fresh runs each matched 5/19 raw native hashes, while the cached RISC-V case matched 19/19. Raw hashes, exact-byte counts and comparison method are recorded separately. No new native gensym-key generator was added.

Every fresh and incremental arm executed its actual Racket core, loaded `racket/base`, and passed arithmetic and hash/vector checks. Chez arithmetic/flfma and bootstrap consistency checks also passed.

All five fixed-base mutation checks passed: poisoning riscv64.ss/loongarch64.ss caused zero compiler actions; poisoning x86_64.ss, cpnanopass.ss or the real Racket CPPFLAGS configuration was detected and rejected. Every mutation used the same literal base. Eleven machine selector variants and upstream namespace/default probes passed locally.

## Measured work and timings

| Commit / change | Cold compiler phases | Reuse compiler phases | Native cache hits | Cold / reuse job |
|---|---:|---:|---:|---:|
| `8403ef4c` riscv64 and loongarch64: repairs for foreign call and callable arguments | 282.4s | 6.4s | 10/10 | 353s / 77s |
| `ed9bbe77` repair in-place marking of an empty continuation stack (#1096) | 273.4s | 57.5s | 0/10 | 334s / 103s |
| `d533f2e1` use native fused multiply-add instructions for flfma (#1095) | 213.8s | 197.7s | 0/10 | 276s / 259s |

The RISC-V change avoided all compiler actions. The C change skipped the slow PB-to-native boot generation but correctly rebuilt native compiler preparation (77 Scheme compilations / one full pass) and all native Racket actions. The Scheme change required 177 Scheme compilations / both bootstrap passes and all native actions; its single timing pair does not establish a reliable speedup.

One pair per commit on Ubuntu 22.04, four build jobs, with runner noise. Compiler-phase time sums Chez bootstrap plus native compilation/restoration. Whole job time additionally includes checkout, artifact transfers, native-reference uploads and runtime verification. Shared rktio building and minimal racket/base loading occur as verification outside those timed compiler phases. These are not end-to-end PR feedback timings; package setup, full desktop tests, macOS and iOS are excluded. Default PR CI is unchanged.

The final run imported already-tested parent checkpoints from run 42392234841, built with the same fixed-base/dependency implementation and identical pinned parent sources. Their original preparation times are retained under `parent_summaries` with `checkpoint_origin_run`; preparation is prior-run cost, not free target work. Consumers checked the compiler/platform/image/configuration compatibility identity independently of the namespace.

The ordinary cold controls unset the CI key and disable the native cache. Their JSON `session_key` field records the candidate configured base, not an enabled override; `mode: cold` and the runner code identify the control. All seeded arms use exactly the same literal value.

## Reproducible evidence

- [CI-generated JSON](benchmark-results/chez-ci-replay-42393887465.json): source revisions, phases, compiler counts, cache hits, hashes, equivalence results, five mutations and all job steps.
- [CI-generated timing table](benchmark-results/chez-ci-replay-42393887465.md).
- [Combined replay source](https://github.com/samth/racket/tree/prototype/chez-fixed-session-ci) and `CHEZ-CI-REPLAY.md`: pinned inputs, source patches, commands and validation protocol.

Full reference outputs and logs are retained as workflow artifacts for seven days. After imported checkpoints expire, regenerate their pinned parents with the replay’s `--mode prepare` command (or the original parent-building workflow at `9b70beb64f`) and archive `replay-state`; update the explicit checkpoint run before a hosted replay. No cached environment snapshot is required.

The original `/workspace/racket` and `/workspace/ChezScheme` checkouts were preserved. No managed environment or proxy/auth configuration was changed or published. Direct Azure log requests still returned Forbidden; the CI collector saved its own evidence using normal Actions artifact access and repository files.
