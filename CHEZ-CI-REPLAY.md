# Fixed CI session base: dependency and cache replay

This prototype separates the literal gensym base in `.github/chez-session-key`
from rebuild and cache decisions. The base is `racket-chez-ci-v1` across source,
compiler, configuration and platform changes. Chez's existing per-action/workarea
namespace derivation is unchanged. There is no source hash, backend exclusion,
or automatic default in the CI namespace.

Zuo's audited dependencies, including selected-backend precision, determine
source invalidation. Native Racket command-rule keys separately track executable,
loaded boot files, sources, command/configuration, loaded rule modules and an
explicit runner/toolchain identity. Whole build-state checkpoints are accepted
only for the same compiler, platform/image, Zuo executable and build configuration;
that compatibility identity never changes the gensym base. It contains no source
hash. A deliberate base-version rotation also requires a fresh workarea.

The replay covers three real upstream changes and their immediate parents:
unused RISC-V/LoongArch backend code, shared C runtime code, and shared Scheme
compiler code. Both arms materialize the selected upstream Chez and pinned
libraries in Racket's bundled path and apply identical audited dependency fixes.
The Racket-selected PB payload is held fixed. The older/newer source graphs use
corresponding narrow patches, preserving upstream ISA-configuration behavior.

For each target, an earlier seeded parent checkpoint warms the native cache.
The cold arm uses ordinary unseeded builds and no native action cache. The reuse
arm retains the fixed base and valid parent state, allowing Zuo to decide which
inputs changed. Native outputs start absent in every target arm; timestamp hash
databases are discarded. No source-derived namespace can force source rebuilds.

The RISC-V arm must run zero compiler actions and hit all ten native cache entries,
with unchanged executable/boot/native hashes. For shared-source changes there is
no assertion of a speedup or unchanged compiler bytes: affected work must rebuild.
An independent verification job discards all Chez/configure build state and forces
a fresh seeded target at the same paths. It compares the resulting executable,
all eight boot/header files and the code/data/sharing of all 19 native outputs with that target's measured
incremental arm, using the exact same fixed base. It must perform both bootstrap
passes; it cannot pass by retaining old Chez outputs.

Mutation tests independently poison unused backends, the active x86-64 backend,
a shared compiler entry file, and CPPFLAGS. All use the same fixed base. Unused
backends must perform zero compiler actions; active/shared source and flag changes
must be noticed and fail at the poisoned input. Build state and source/config
contents are restored between cases. Selector tests cover eleven machine types.

Reports distinguish bootstrap/native build time from whole target-job time,
including source preparation and artifact transfer. Parent cache preparation is
prior-run cost, recorded separately. One pair per commit is subject to runner
noise; executed compiler actions and fresh/incremental equivalence are stronger
evidence than small timing differences. Package setup, full desktop tests, macOS and iOS are not measured here. The
minimal racket/base runtime smoke may compile collections as verification work,
separate from the timed compiler phases. No whole-PR speedup is
inferred from these build-only experiments. Default CI and normal Chez session
behavior remain unchanged.

The replay is destructive only inside its explicitly authorized disposable
checkout (`--allow-replace-bundled`). It does not publish or alter the managed
environment configuration or authentication.

## Compiler-input dependency regression

The first constant-base replay exposed stale compiler-preparation artifacts on
`d533f2e1`: bootstrap failed with a duplicate interned gensym. Global source-key
rotation had previously forced those files to be discarded. The corrective Zuo
patch declares the native compiler executable and incoming boot files as inputs
to bootstrap preparation and the bootstrap stamp. Preamble and Nanopass artifacts
therefore rebuild when their compiler changes. Nanopass removes its complete
14-file output group before compilation; imported-library timestamp checks cannot
retain old child libraries. The active boot files modified during fixpoint
iteration are deliberately not cyclic inputs. No namespace derivation changes.

The real shared-compiler commit remains a required warm-versus-fresh regression,
including independent native compilation when the parent's native cache misses.
The initial failed run and its diagnostics are retained as evidence.

The final verification imports the already-built parent checkpoints from run
42392234841 (identical compiler/namespace/dependency code), rather than warming
them again. Their original preparation times and source revisions remain in the
JSON with checkpoint_origin_run; consumers independently verify the complete
compiler/platform/configuration compatibility identity before reusing build state.

Compiler and eight boot/header comparisons remain byte for byte. Independently
compiled native Racket files can differ in private gensym unique names. The native
comparison uses Chez's own fasl-file-equal? checker, retaining code, data and sharing
checks while allowing private gensym renaming. Raw hashes and exact-byte match
counts are recorded separately. The warm arm uploads native reference outputs
for that check. Each arm also builds a shared rktio outside the timed compiler
phases and executes its actual Racket core against racket/base with arithmetic
and hash/vector checks. Added verification/artifact work is included in job times.
