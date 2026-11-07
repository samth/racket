# Fixed: unused RISC-V backend invalidates x86-64 CI builds

The actual upstream RISC-V/LoongArch commit `8403ef4c`, compared with its immediate
parent `048879e3`, now reuses the x86-64 build correctly. All five jobs in the
[hosted replay](https://github.com/samth/racket/actions/runs/42381872117) passed.

| Measurement | Ordinary cold build | Parent state/cache reuse |
|---|---:|---:|
| Entire target job, including setup and restoration | 339 s | **44 s** |
| Chez PB/native bootstrap | 220.63 s | **0.98 s** |
| Native Racket compilation/restoration | 57.61 s | **2.89 s** |
| Combined build phases | 278.24 s | **3.87 s** |
| Bootstrap Scheme compilations | 177 | **0** |
| Full bootstrap passes | 2 | **0** |
| Native Racket cache hits | 0/10 | **10/10** |

The target job saved **295 seconds (87.0% less time)**. This is one hosted pair,
so absolute cold timings remain subject to runner variation. The stronger result
is that all compiler work was avoided with matching output hashes. These are
build-boundary/job measurements, not full PR feedback timings: package setup,
collection compilation, full desktop tests, macOS and iOS are excluded. A parent
checkpoint must already exist; its preparation cost is separately recorded.

## The dependency fix

`np-register.ss` opens exactly one `<architecture>.ss` backend. Previously both
the explicit CI namespace and the Zuo graph depended on every backend. The fix:

- Adds a shared machine-to-source selector in `s/machine.zuo`. It filters known
  unused backends and retains common and unknown/new source names.
- Uses target-specific dependencies for Scheme compilation, bootstrap stamps,
  and PB/bootquick records. Cross-build records use the requested target machine.
- Lets the CI helper explicitly select its source-built machines with
  `--machines=pb,ta6le`. Omitting the option keeps conservative whole-tree scope.
- Preserves the aggregate source-list export and adds the compiler entry files
  that bootquick records previously omitted.

Normal session-key defaults are unchanged. This narrows only known Scheme
backend files; C inputs and machine-definition files remain conservative.

The implementation is pushed to both forks:
[Racket](https://github.com/samth/racket/tree/prototype/platform-chez-dependencies)
(component commit `c55fef59bd`) and
[ChezScheme](https://github.com/samth/ChezScheme/tree/prototype/platform-chez-dependencies)
(component commit `25b401bc`). The combined cache/replay setup is on
[`prototype/chez-platform-ci`](https://github.com/samth/racket/tree/prototype/chez-platform-ci),
measured at immutable revision `23f67577cb`.

## Correctness checks

The warm arm matched the parent's executable, all eight boot/header files and
all 19 restored native outputs. It ran zero bootstrap passes and zero native
compiler actions. Every native output starts absent, and timestamp hash databases
are discarded, so the ten hits are actual action-cache restorations.

An independent verification job discarded the target's Chez workarea and forced
a fresh seeded rebuild at the same paths. It performed 177 Scheme compilations
and both bootstrap passes, passed the bootstrap-consistency check, and reproduced
the parent's executable and all eight boot/header hashes. Native Racket then
hit all ten cache entries and restored the same 19 output hashes. Its Chez build
took 120.28 seconds; this validation timing is separate from warm target time.

Hosted mutation tests poisoned RISC-V and LoongArch source files: x86-64 still
succeeded with zero Scheme compilations. Poisoning x86_64.ss triggered rebuilding
and failed while reading the active backend. Sources and build state were
restored afterwards. Selector tests cover eleven machine variants; key tests
check that changed backends still invalidate their own cross-target keys, and
that common/PB/toolchain changes remain tracked. Arithmetic/flfma smoke checks
also passed. These tests do not claim to execute RISC-V code.

The local reproduction passed the same tests, including a fresh seeded bootstrap
that reproduced every compiler/boot hash. Original `/workspace/racket` and
`/workspace/ChezScheme` checkouts were preserved. No managed environment or proxy
configuration was changed or published.

[Raw CI-generated JSON](benchmark-results/chez-ci-replay-42381872117.json) records
all timings, counters, compiler/boot/native hashes and job steps. The
[CI-generated table](benchmark-results/chez-ci-replay-42381872117.md) summarizes
the comparison. Full logs and mutation evidence remain workflow artifacts. The
older exploratory run was canceled after correcting a filename-only assertion
in the regression test; the dependency/key implementation did not change for
that correction.
