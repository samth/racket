# Recent Chez commits: measured Linux CI replay

[GitHub Actions run](https://github.com/samth/racket/actions/runs/42393887465)

Each target is compared with its immediate parent. Both arms include the audited dependency fixes. The restored arm uses one fixed CI namespace and the native Racket action cache. Sources invalidate through Zuo dependencies; compiler/platform/configuration compatibility is checked independently of the namespace.

| Commit | Change | Fresh build | Restored build | Native cache hits | Target job saving |
|---|---|---:|---:|---:|---:|
| `8403ef4c` | riscv64 and loongarch64: repairs for foreign call and callable arguments | 282.4s | 6.4s | 10/10 | 276s (78.2%) |
| `ed9bbe77` | repair in-place marking of an empty continuation stack (#1096) | 273.4s | 57.5s | 0/10 | 231s (69.2%) |
| `d533f2e1` | use native fused multiply-add instructions for flfma (#1095) | 213.8s | 197.7s | 0/10 | 17s (6.2%) |

Independent verification jobs force fresh target bootstraps with the same fixed namespace and compare compiler and all eight boot/header files byte for byte, and all 19 native outputs with Chez's code/data/sharing FASL comparison (allowing private gensym renaming). Both fresh and incremental Racket cores execute runtime smoke checks. Raw native byte matches are recorded separately. Constant-base active/inactive/shared-source and flag mutation tests are recorded under dependency_mutations; fresh equivalence results are under validation_summaries.

Fresh/restored build time sums the Chez/PB bootstrap and native Racket compilation phases. Target job saving includes checkout, configuration, artifact download/restoration and cleanup; queue time is excluded. Parent preparation is prior-run cache-warming cost and is recorded in the JSON, not counted as free target-PR work.

One pair per commit, on standard Ubuntu 22.04 hosted runners with four build jobs. Small differences are subject to runner noise. This does not measure package setup, the full desktop test step, macOS or iOS; it is not an end-to-end PR speedup claim.

Every successful arm checks Racket bootstrap consistency, runs arithmetic/flfma smoke checks, and records compiler/boot hashes and all 19 native artifact hashes. Detailed phases, source materialization and cache invalidation decisions are retained in the JSON and workflow artifacts.
