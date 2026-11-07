# Fixed Chez session-key base in CI

Normal builds retain upstream Chez's default. The manual **CI Chez session key
prototype** workflow opts in through `RACKET_CHEZ_SESSION_KEY`; its `seeded`
boolean also provides an ordinary unseeded control. PR CI defaults are unchanged.

The versioned literal in `.github/chez-session-key` is `racket-chez-ci-v1`.
It is a gensym namespace base, independent of sources, platform, compiler,
flags, runner image, and workflow execution. There is no source-hashing helper
or backend exclusion list in the key. Change the version only to deliberately
rotate the namespace policy, and use a fresh Chez workarea when doing so.

Chez's existing formula derives a distinct namespace from the base, workarea,
and action name. The existing formula and ordinary application gensym behavior
are unchanged. This does not promise relocation-independent boot bytes.

The adapter in `racket/src/cs/c/build.zuo` forwards the explicitly supplied value
to Chez's existing `sessionKey` option for PB/bootquick/native phases. With no
variable, the original configuration is passed through.

## Dependencies and cache identity

Zuo's actual input dependencies decide what must rebuild. Compiler commands,
configuration, toolchain identity, executable bytes and loaded boot bytes belong
in build/action dependencies and cache keys, independently of the session base.
A fixed namespace does not authorize reusing outputs after those inputs change.
The optional native CS action-cache prototype separately enforces these inputs.

The audited dependency fixes remain. Per-machine Chez compilation, bootstrap
stamps and PB-to-target bootquick records depend on the selected backend;
cross-build records use their target, not the host. The aggregate source list
retains all backends and includes compiler entry points. This selection is part
of the build graph only; the CI namespace does not inspect source files.

## Reproduction

Use a fresh build directory when first enabling the key or rotating its version:

```sh
RACKET_CHEZ_SESSION_KEY="$(cat .github/chez-session-key)" make CPUS=4 PKGS=""
```

Standalone Chez can use its existing `configure --sessionkey=racket-chez-ci-v1`
option. There is no automatic default or special interpretation of `auto`.

The manual workflow uses the normal PB bootstrap, native build and desktop test
script on Linux/macOS. It uploads checksum manifests; it enables no artifact
cache by itself. The separate Linux replay combines this fixed base with the
native CS action-cache prototype and checks warm target builds against fresh
seeded builds of the same target revision, including changed shared sources.

Existing namespace/default and backend selector regressions can be run with:

```sh
python3 racket/src/ChezScheme/tools/build-key-probe.py --zuo /absolute/path/to/zuo
/absolute/path/to/zuo racket/src/ChezScheme/tools/backend-dependencies.zuo
```

The namespace probe checks that defaults do not override the key, actions get
separate namespaces, and source/script changes do not change a fixed base's
per-action derivation. Functional dependency tests must independently verify
that source and configuration changes still invalidate their consumers.

Earlier source-derived-seed experiments are historical evidence, not the current
key design. New fixed-base results are retained on the CI replay results branch.
