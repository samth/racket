# Reproducible bundled Chez boots: prototype

This branch changes **Racket's bundled Chez**, independently of the matching
`samth/ChezScheme` branch `prototype/reproducible-boot-default`.

Configure defaults to `--sessionkey=auto`. The build driver scopes identities by
source-tree contents, action script, compiler executable, startup inputs, machine
and existing action/output paths. Ordinary application gensyms retain their
random session identities. `--sessionkey=` opts out; named custom keys preserve
the previous exact key formula. No global runtime gensym policy is changed.

Two fresh native-host bootquick builds at fixed source/output roots produced
8/8 byte-identical boot/header files. Final runs took 19.83 and 17.56 seconds.
A separate opt-out pair reproduced the two changing boot images while the other
six outputs matched. Local timings overlapped other validation work; they are
not CI speedup estimates. Compact command/status/timing rows and SHA-256
manifests are committed in `prototype-evidence/`.

The build-key probe passed stable-key, source/script/action-change, opt-out and
exact legacy custom-key checks. Run it with:

```sh
python3 racket/src/ChezScheme/tools/build-key-probe.py --zuo /absolute/zuo
```

The complete boot comparison is reproducible with:

```sh
python3 racket/src/ChezScheme/tools/reproducible-boot-probe.py \
  --zuo /absolute/zuo \
  --host-scheme '/absolute/native/scheme -B /absolute/native/petite.boot -B /absolute/native/scheme.boot'
```

The standalone Chez prototype additionally passed a normal PB-to-native build
and native runtime smoke checks; independent runtime processes generated
distinct ordinary gensyms. The bundled probe here uses the existing native host,
not a full Racket build. Neither probe establishes reproducibility across
platforms, relocated paths, profiled builds or incremental-versus-clean builds.
Source scope is conservative. Broader evaluation is needed before enabling this
as an upstream default. See bundled Chez's BUILDING for policy details.
