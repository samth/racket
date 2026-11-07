# Recent Chez commits: measured Linux CI replay

[GitHub Actions run](https://github.com/samth/racket/actions/runs/42378505446)

Each target is compared with its immediate parent. Both arms include the audited dependency fixes. The restored arm uses the explicit CI key and native Racket action cache; Chez state is discarded when that key changes.

| Commit | Change | Fresh build | Restored build | Native cache hits | Target job saving |
|---|---|---:|---:|---:|---:|
| `8403ef4c` | riscv64 and loongarch64: repairs for foreign call and callable arguments | 220.5s | 284.0s | 0/10 | -113s (-42.2%) |
| `a8f47df0` | Consdense release notes for foreign struct-argument repairs | 272.7s | 6.4s | 10/10 | 279s (84.8%) |
| `68a0bc5e` | configure: add `--x86-64-v<level>` to enable direct ROUNDSD, etc. (#1092) | 251.8s | 239.3s | 0/10 | 7s (2.4%) |
| `ed9bbe77` | repair in-place marking of an empty continuation stack (#1096) | 209.7s | 279.9s | 0/10 | -75s (-29.5%) |
| `d533f2e1` | use native fused multiply-add instructions for flfma (#1095) | 222.7s | 288.8s | 0/10 | -65s (-24.2%) |

Fresh/restored build time sums the Chez/PB bootstrap and native Racket compilation phases. Target job saving includes checkout, configuration, artifact download/restoration and cleanup; queue time is excluded. Parent preparation is prior-run cache-warming cost and is recorded in the JSON, not counted as free target-PR work.

One pair per commit, on standard Ubuntu 22.04 hosted runners with four build jobs. Small differences are subject to runner noise. This does not measure package setup, the full desktop test step, macOS or iOS; it is not an end-to-end PR speedup claim.

Every successful arm checks Racket bootstrap consistency, runs arithmetic/flfma smoke checks, and records compiler/boot hashes and all 19 native artifact hashes. Detailed phases, source materialization and cache invalidation decisions are retained in the JSON and workflow artifacts.
