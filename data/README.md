# Scope-set measurements: master vs scope-sets-r4

Measurements on huor (Intel Core Ultra 7 265, Linux) comparing two Racket CS trees from
samth/racket:

- **master**: 45ccd1c43a (version 9.4.0.4)
- **sets**: branch scope-sets-r4, 34f4651b1e (version 9.4.0.5)

Each tree is minimal Racket CS built with `make base`, with main-distribution and
main-distribution-test (257 packages) installed with `--no-setup` from one local snapshot
of pkgs.racket-lang.org, so both trees compile identical sources. Every command ran with
its own `PLTADDONDIR`. `scripts/` holds every script used.

## Full build (`buildplot/`)

The step build-plot-cs.racket-lang.org measures, run once per tree, each pinned to one
P-core, the two running at the same time:

    perf stat -e cpu_core/instructions/u,cpu_core/cycles/u env PLT_EXPANDER_TIMES=1 PLT_LINKLET_TIMES=1 \
      racket -W "debug@GC error" -l- raco setup -j 1

| | master | sets | change |
|---|---|---|---|
| wall | 1801 s | 1717 s | −4.6% |
| user+sys | 1791 s | 1706 s | −4.8% |
| instructions | 17.436e12 | 16.541e12 | −5.1% |
| cycles | 8.471e12 | 8.012e12 | −5.4% |
| compiling collections | 1323 s | 1235 s | −6.7% |
| building documentation | 472 s | 476 s | +0.8% |
| expander `expand` | 350.9 s | 291.5 s | −16.9% |
| expander `eval` | 476.8 s | 471.7 s | −1.1% |
| expander `compile` | 257.2 s | 259.7 s | +1.0% |
| expander `read` | 41.6 s | 42.3 s | +1.8% |
| linklet layer total | 821.7 s | 828.0 s | +0.8% |
| peak RSS | 1049 MB | 1036 MB | −1.2% |
| GC-log peak memory (before GC) | 820.9 MB | 836.4 MB | +1.9% |
| peak memory after a major GC | 666.7 MB | 665.3 MB | −0.2% |
| GCs / GC time | 214,829 / 544 s | 200,743 / 521 s | −6.6% / −4.3% |

`buildplot/<tree>/setup-times.txt` has the full PLT_EXPANDER_TIMES and PLT_LINKLET_TIMES
reports.

`buildplot/gc-memory.csv` holds the memory-over-CPU-time data that build-plot plots:
`variant,cpu_s,mem_after_mb,mem_before_mb,major`. `mem_before` is the `@ N K` figure of
each debug@GC line, and `mem_after` subtracts the `free` figure. The file keeps every
major GC and, of the minor GCs, the one with the highest pre-GC memory in each of 650
CPU-time buckets (about 2000 points per tree). Documentation starts at about CPU 1318 s
(master) and 1229 s (sets); the setup.log timestamps give the wall time, scaled by each
run's CPU/wall ratio.

## Startup and load (`startup/`)

Ten runs per command, alternating trees on one P-core, after a warm-up run; no display.
Load times do not regress; instructions fall slightly as more code loads.

| command | instructions (median) | wall (median) | peak RSS (median) |
|---|---|---|---|
| `racket -n -e ''` | 0.00% | 0.050 s both | +0.05% |
| `racket -l racket/base -e ''` | −0.14% | 0.060 s both | +0.18% |
| `racket -l racket -e ''` | −0.17% | 0.140 s both | −0.08% |
| `racket -l typed/racket -e ''` | −1.42% | 0.260 → 0.250 s | +0.02% |
| `racket -l racket/base -l drracket/private/link -e ''` | −0.64% | 1.250 → 1.230 s | −0.39% |

`drracket/private/link` defines DrRacket's compound unit, so requiring it loads all of
DrRacket's code without starting the GUI. Memory in use after `(dynamic-require m #f)` and
three collections (`startup/mem.txt`): racket/base 54.79 → 54.81 MB, racket 82.05 → 82.13
MB, typed/racket 109.11 → 108.95 MB.

## Compiled code (`zo/`)

**Size.** The 7,657 .zo files of the full install total 361,689,957 bytes on master and
361,863,974 bytes on sets (+0.05%).

**Determinism** (`zo/determinism.txt`). Each tree compiled the closure of racket,
typed/racket, scribble/manual, pict, json, net/url and rackunit (756 modules) from scratch
into fresh `PLTCOMPILEDROOTS`, once with `raco make` in one process and once with
`raco make -j 6` and the targets reversed. In both trees the two runs agree on 755 of 756
files; the exception is syntax-color's racket-lexer in both. Against the .zo the full
`raco setup` produced, 283 files differ in both trees, and they are the same 283 paths, so
the change adds no nondeterminism.

**What differs between master and sets.** .zo files embed absolute paths, so both trees
compiled the same 756 modules with each tree moved in turn to the same path
(`scripts/samepath.sh`). After masking the version string and the bundle hash in each
header, 141 files are identical and 615 differ (`zo/same-path-content-differs.txt`). For
40 of those 615, sampled at random, `scripts/mi-dump.rkt` compiled the module to
machine-independent form in both trees and compared every linklet bundle entry: the only
entries that differ are the `stx-data` linklets, which hold the serialized syntax
literals (`zo/machine-independent-keys.txt`). In those linklets the scope sets that master
serializes with the `#:seteq` tag use the `#:scope-set` tag, and the empty scope set
becomes one shared entry, which shifts later indices by one
(`zo/cm-accomplice-{master,sets}.rktd`). Phase-level code, declarations and data
linklets are identical.

## Rhombus (`rhombus/`)

Both trees also have the `rhombus` package and its dependencies (28 more packages, from
one snapshot of pkgs.racket-lang.org; `rhombus/pkgshow.txt`). The workload compiles every
module of the Rhombus `-lib` packages (820 files, tests and docs excluded;
`rhombus/rh-lib-files.txt`) from scratch with `raco make` in one process, writing to a
fresh `PLTCOMPILEDROOTS` root so the installed trees stay unchanged. Modules that raco
itself has not loaded are compiled again too, so each run writes 1,549 .zo files. Three runs
per tree alternated on one P-core:

| | master (median) | sets (median) | change |
|---|---|---|---|
| wall | 339.8 s | 307.4 s | −9.6% |
| instructions | 4.023e12 | 3.772e12 | −6.2% |
| cycles | 1.593e12 | 1.434e12 | −10.0% |
| peak RSS | 1333 MB | 1290 MB | −3.2% |
| expander `expand` | 87.5 s | 68.4 s | −21.9% |
| expand allocation | 114.3 GB | 81.8 GB | −28.4% |
| expander `eval` | 99.9 s | 96.8 s | −3.1% |
| expander `compile` | 63.4 s | 62.2 s | −2.0% |

`rhombus/summary.md` has min and median for every metric.

## Interned-scope prefix lengths (`interned-prefix/`)

How many interned scopes a scope set holds, and how many an operation on a normal scope walks past, with interned scopes sorted first. See `interned-prefix/README.md`.
