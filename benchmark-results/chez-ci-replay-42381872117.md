# Recent Chez commits: measured Linux CI replay

[GitHub Actions run](https://github.com/samth/racket/actions/runs/42381872117)

Each target is compared with its immediate parent. Both arms include the audited dependency fixes. The restored arm uses the explicit CI key and native Racket action cache; Chez state is discarded when that key changes.

| Commit | Change | Fresh build | Restored build | Native cache hits | Target job saving |
|---|---|---:|---:|---:|---:|
| `8403ef4c` | riscv64 and loongarch64: repairs for foreign call and callable arguments | 278.2s | 3.9s | 10/10 | 295s (87.0%) |

The independent verification job also forces a fresh seeded target bootstrap and checks compiler, all eight boot/header files, and all 19 native artifacts against the parent. Active/inactive backend mutation regressions run in that job. Its timings and output hashes are recorded separately under validation_summaries.

Fresh/restored build time sums the Chez/PB bootstrap and native Racket compilation phases. Target job saving includes checkout, configuration, artifact download/restoration and cleanup; queue time is excluded. Parent preparation is prior-run cache-warming cost and is recorded in the JSON, not counted as free target-PR work.

One pair per commit, on standard Ubuntu 22.04 hosted runners with four build jobs. Small differences are subject to runner noise. This does not measure package setup, the full desktop test step, macOS or iOS; it is not an end-to-end PR speedup claim.

Every successful arm checks Racket bootstrap consistency, runs arithmetic/flfma smoke checks, and records compiler/boot hashes and all 19 native artifact hashes. Detailed phases, source materialization and cache invalidation decisions are retained in the JSON and workflow artifacts.
