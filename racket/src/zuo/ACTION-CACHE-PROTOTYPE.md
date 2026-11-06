# Explicit command actions and local artifact caching (prototype)

This branch adds `command-rule` to Zuo and opts native Racket CS compilation
into it with `ZUO_CS_ACTIONS=1`. Existing procedural rules keep their behavior.
The cache implementation is Zuo; Python is used only by its integration tests.

Build the prototype executable; an older Zuo cannot supply its runtime metadata:

```sh
cc -O2 -o /absolute/build/zuo zuo.c
/absolute/build/zuo -X /absolute/source/lib build.zuo
```

Use `command-rule` as the rule returned by a file target's rule callback:

```scheme
(command-rule deps
  (hash 'argv (list "/absolute/path/to/compiler" "-o" "out" "in")
        'environment (list '("LANG" . "C") '("PATH" . "/usr/bin:/bin"))
        'directory "."
        'toolchain "immutable-image+sysroot+platform-identity"
        'tools (list "/absolute/path/to/extra/tool")
        'rule-files (list (at-source "build.zuo"))))
```

The description is also the command executed. No independent build closure can
drift from it. `environment` is the **complete child environment**; stdin is EOF.
The executable, Zuo, loaded module source files, declared tools, and rule files become
inputs automatically. Declare every other file read, including generated
inputs and boot images, in `deps`. Data targets represent configuration inputs.
The `:rule` option in `make-targets` lets a file target's callback accept
`(path token resolved-deps)` and return this rule instead of executing a recipe.
It rejects secondary dependencies and phony targets.

Set `ZUO_ACTION_CACHE=/absolute/cache` to enable storage. Unset it to execute the
same command rules without caching. `ZUO_ACTION_CACHE_LOG=1` reports hit/miss,
rejection, and publication with only an action digest. Metadata stores output
digests and modes, not command arguments or environment values.

Keys include the command, working directory, exact child environment, explicit
toolchain identity, declared dependency hashes, direct input bytes/modes and immediate symlink targets, and the
complete output paths. The cache bypasses Zuo's timestamp hash cache when taking
these direct snapshots. Input groups include their declared co-outputs. All
outputs are removed before a miss executes, and all must be produced afresh.
Inputs are rechecked before publication. Failed/incomplete/input-mutating actions
do not publish results.

Blobs are SHA-256 addressed. A small manifest is committed last by rename.
Restore validates the whole group before staging files, then promotes each file
by rename and records the normal Zuo build result. Corrupt blobs and malformed
manifests cause execution. Separate build directories can share storage; do not
run concurrent builds in one output directory. POSIX regular-file outputs only.
Cache permission and other I/O errors can still fail the build.

This is not a sandbox: the caller is responsible for complete inputs and an
accurate toolchain identity, including dynamic libraries, sysroots, locale data,
and platform. Undeclared reads, clocks, random inputs and network reads cannot be
made cacheable simply by giving them a tag. Paths are deliberately part of the
identity; this first prototype requires fixed checkout and build roots across
runs. Cache storage must be trusted. There is no remote transport, eviction,
portable path rewriting, or automatic toolchain discovery in this prototype.
When restoring a whole incremental build directory separately, discard its
`_zuo_tc.db` timestamp caches before trusting dependency hashes.

## Racket CS pilot

```sh
export ZUO_CS_ACTIONS=1
export ZUO_CS_TOOLCHAIN_ID='your-immutable-runner-image+sysroot+platform'
export ZUO_ACTION_CACHE=/absolute/cache
# Invoke Racket's Zuo build with the prototype executable and libraries.
```

This pilot covers the nine native library compilations and final whole-program
compilation. It hashes declared loaded boot images in addition to the Chez
executable. The child receives PATH, C locale, UTC, and explicitly configured
RACKET_SO compilation variables. Cross compilation keeps its existing rules.
Changing the child environment is intentional and opt in. The caller must
supply the remaining runner/toolchain identity. No C, package, test, or Chez
bootstrap action is cached by this integration yet.

Independent correctness fixes declare each native `.so`/`.wpo` output group,
include all library groups as whole-program inputs, and give standalone rktio
rules their complete configuration and producer files as dependencies. These
fixes work without enabling the cache.

## Validation

```sh
python3 tests/action-cache.py /absolute/build/zuo
/absolute/build/zuo -X /absolute/source/lib tests/main.zuo
```

The executable's default library location must also resolve for child processes
started by the existing test suite (for example, put the executable alongside
`lib`). The cache checks cover command/env/toolchain/source/executable changes,
missing co-outputs, fresh build state, malformed/corrupt entries, executable mode,
failed/incomplete/input-mutating commands, and simultaneous CAS writers.
Local timing evidence is recorded in `PROTOTYPE-RESULTS.md` at the repository
root. These measurements are not GitHub runner speedup claims.

Symlink file inputs are dereferenced for bytes/modes and their immediate link
targets are included. Producers that inspect additional filesystem metadata,
resolved ancestor paths, directory listings, or absent files must declare that
information separately as data; the prototype does not discover those reads.

The module loader now reports actual filesystem module inputs through
`runtime-env`'s `module-files` entry. Embedded modules are covered by the Zuo
executable. External boot images must still be declared explicitly. This is a
conservative set of modules loaded so far in the parent process, including the
active driver, rather than a scan of all library files. Arbitrary file reads and
modules loaded only by child tools still require explicit declarations. The
native pilot now declares `rktio/rktio.rktl` and `cs/linklet/trace.ss`; mutation
regressions verify rejection of stale incremental outputs and false cache hits.
