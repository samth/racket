# Explicit Chez session keys in CI

Normal builds keep upstream Chez's default: no deterministic session-key override.
The automatic-source-key behavior and `sessionKey=auto` default are removed.
The dependency/output fixes from the audit remain on this branch.

The manual **CI Chez session key prototype** workflow opts in through
`RACKET_CHEZ_SESSION_KEY`. Its `seeded` boolean provides a fresh-build control
without changing PR CI defaults. It uses the ordinary PB bootstrap, native build
and existing desktop test script on Linux and macOS; no cache is enabled by this
workflow. Hosted execution has not been measured yet.

Racket calls its bundled Chez build graph directly, so changing arguments to
Chez's configure alone would miss that route. The small adapter in
`racket/src/cs/c/build.zuo` forwards the explicitly supplied environment value to
Chez's existing `sessionKey` option for PB/bootquick/native phases. Without that
variable the original configuration is passed through. Chez's existing per-action
key formula and ordinary application gensym behavior are unchanged.

## Seed scope

After fetching the selected PB payload, CI runs:

```sh
racket/src/build/bin/zuo .github/scripts/chez-session-key.zuo \
  racket/src/ChezScheme "$RUNNER_OS/$RUNNER_ARCH" \
  "${ImageOS:-unknown}/${ImageVersion:-unknown}" "--machines=pb,ta6le" "PKGS=;configure=defaults"
```

The pure-Zuo helper hashes the production Chez source trees, library sources,
Unicode data, build recipes, eight PB seed files, compiler executable bytes,
selected compiler/build environment values, and explicit platform/image/build
context. It also conservatively includes Racket's Chez adapter/configure recipe
files. It excludes VCS metadata, unrelated Racket sources, whole-repository commit
IDs and workflow run IDs. Identical inputs retain the seed across unrelated
Racket PR changes. Editing adapter recipe files can conservatively change it.

With `--machines=pb,ta6le`, the helper excludes the known Scheme compiler
backends that neither selected machine reads. It uses the same selector as the
Chez build graph. Specify every source-built host and target machine: for a
RISC-V cross build include `trv64le`; for Apple Silicon use `tarm64osx` rather
than `ta6le`. The workflow explicitly maps its supported native targets and
rejects unknown combinations. Omit the option for conservative whole-tree scope.
Common files and unknown/new source filenames remain included. This narrows only
known backend files, not conditional C inputs or machine-definition files.

Chez's per-machine compilation, bootstrap stamps, and PB-to-target bootquick
records now use only the selected backend. The aggregate `all-src-names` export
continues to enumerate all backends. Compiler entry-point files are also included
in that list so bootquick records track edits to `cpnanopass.ss` and companions.
Default session-key behavior remains unchanged.

Pass any additional build options in the context arguments; keep those arguments
in agreement with the actual build. The image argument should identify the
runner/toolchain environment, not a unique workflow execution. The helper does
not install or change that environment. Its output is a **gensym namespace**, not
a complete artifact-cache key, a sysroot attestation, or automatic file-read
discovery. Artifact reuse still requires the separately declared action inputs
and toolchain identity.

## Using it locally for a CI reproduction

```sh
key=$(racket/src/build/bin/zuo .github/scripts/chez-session-key.zuo \
  racket/src/ChezScheme Linux/X64 your-runner-image --machines=pb,ta6le 'your exact build options')
RACKET_CHEZ_SESSION_KEY="$key" make CPUS=4 PKGS=""
```

Use a **fresh output directory** when changing or enabling a key. The upstream
explicit-key API does not fully invalidate existing incremental state when its
key changes. Neither this prototype nor the seed helper claims relocation-safe
boot bytes; upstream action scope includes build paths.

For standalone Chez, pass the same generated string through its existing
`configure --sessionkey=...` option. There is no new automatic mode or default.
The literal `--sessionkey=auto` is an ordinary explicit string again.

## Validation

The seed scope tests cover stability, unrelated Racket edits, VCS changes,
relocation of the source tree, source/PB changes, compiler bytes and build flags:

```sh
python3 .github/scripts/tests/test-chez-session-key.py /absolute/path/to/zuo
```

The restored upstream option formula and absent default override are checked by
`racket/src/ChezScheme/tools/build-key-probe.py`. The paired boot probe now
requires an explicit `--sessionkey`; its default-control builds pass no key at
all. Evidence in `prototype-evidence/ci-session-key/` records local native-host
comparisons and the normal Racket PB-to-native bootstrap, including Racket's
`check-bootstrap-same-stamp` consistency check. These are correctness and byte
reproducibility probes, not hosted PR speedup measurements.

Measured locally: two clean normal Racket PB-to-native bootstraps passed in
177.0 and 183.6 seconds, including both bootstrap-consistency checks. All eight
completed native boot/header hashes matched across the two runs at identical
paths. A separate native-host paired comparison matched 8/8 with the explicit
seed; the unchanged default matched 6/8, with petite.boot and scheme.boot
differing. Runtime arithmetic/hash smoke checks passed and independent processes
still generated distinct ordinary gensyms. The helper tests passed with the
ordinary pre-existing Zuo executable as well as the development executable.
The workflow uploads only checksum manifests for comparing hosted runs.
