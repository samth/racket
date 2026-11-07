# Target-specific Scheme backend dependencies

The `architecture` macro in `s/np-register.ss` opens exactly one backend source,
selected by the target architecture. Previously the Zuo graph depended on every
backend, so a RISC-V source edit invalidated native x86-64 compilation and PB
bootquick records.

The shared `machine-source-names` selector retains the target backend, all common
inputs and unknown/new filenames. Per-machine Scheme compilation, bootstrap
stamps, and PB-to-target boot records use it. Cross-build records select their
requested target, not the host/PB machine. The public aggregate `all-src-names`
list still contains every backend, and now also includes the compiler entry
points so bootquick records track those edits.

This changes dependency precision only; it does not change session-key defaults.
Racket's CI session-key base is a fixed literal. It does not inspect these source
files or duplicate dependency selection; Zuo handles invalidation independently.

Run the selector regression with:

```
zuo tools/backend-dependencies.zuo
```

The companion Racket replay checks actual PB bootstraps, warm reuse, fresh seeded
output equivalence, active/inactive backend mutations and native action-cache
hits. No RISC-V execution speed or cross-toolchain correctness claim is made by
these x86-64 tests.

Bootstrap preparation also tracks the native compiler executable and the incoming
seed boot files. These are distinct from the active boot files replaced during
fixpoint iteration, so the dependencies are acyclic. Preamble objects and the
complete Nanopass library group rebuild when their compiler inputs change.
Nanopass compilation removes its prior output group first: Chez's internal
imported-library timestamp checks do not track a changed compiler.
