# Interned-first prototype: results

Expansion time (minimum of 5, after a warm-up expansion) and allocation of one expansion,
from `../../ryan/bench.rkt` on P-core 3 of huor with nothing else running (`run.sh`,
raw output in `results.txt`). The programs come from `../../ryan/gen2.py`. master is the
build-plot master tree; `as` and `int` are `../racket-as` and `../racket-int` run as
bin/racket of a copy of the build-plot scope-sets-r4 tree.

| program | master | as (7a7c482433) | int (cd126ece77) |
|---|---|---|---|
| 20,000 interned add/remove pairs, depth 10 | 9.1 ms, 23 MB | 9.5 ms, 30 MB | 8.3 ms, 16 MB |
| same, depth 100 | 10.7 ms, 29 MB | 30.2 ms, 141 MB | 9.0 ms, 17 MB |
| same, depth 1000 | 87.2 ms, 183 MB | 385.4 ms, 1332 MB | 86.7 ms, 46 MB |
| Rhombus `block` nested 100 deep | 177 ms, 365 MB | 154 ms, 335 MB | 152 ms, 332 MB |
| Rhombus `block` nested 300 deep | 640 ms, 939 MB | 718 ms, 790 MB | 572 ms, 764 MB |
| Rhombus `block` nested 1000 deep | 5686 ms, 4389 MB | 6222 ms, 3631 MB | 5742 ms, 3332 MB |
| Rhombus `fun` nested 100 deep | 199 ms, 370 MB | 171 ms, 332 MB | 171 ms, 327 MB |
| Rhombus `fun` nested 300 deep | 909 ms, 996 MB | 858 ms, 832 MB | 825 ms, 794 MB |
| Rhombus `fun` nested 1000 deep | 8159 ms, 5428 MB | 9682 ms, 4349 MB | 9228 ms, 3916 MB |

## Compile workloads: as vs int (`run2.sh`, `compile/`)

typed/racket with `raco make` into a fresh `PLTCOMPILEDROOTS` root, and the Rhombus -lib packages with `../../scripts/rh-clean.sh` and one `raco make` of the aggregator module, both in a copy of the build-plot scope-sets-r4 tree with the binary swapped in. Three rounds on P-core 3, alternating which binary goes first.

### typed (median of 3)

| binary | instructions (G) | cycles (G) | wall (s) | expand (s) | expand alloc (GB) | peak RSS (MB) | read counts |
|---|---|---|---|---|---|---|---|
| as | 579.9 | 244.8 | 49.2 | 13.58 | 14.84 | 440 | [15869, 15877] |
| int | 579.8 (-0.0%) | 247.9 (+1.3%) | 49.8 (+1.3%) | 13.63 (+0.4%) | 14.84 (+0.0%) | 437 (-0.7%) | [15869] |

as typed walls: 49.1, 49.3, 49.2

int typed walls: 49.1, 50.0, 49.8

### rhombus (median of 3)

| binary | instructions (G) | cycles (G) | wall (s) | expand (s) | expand alloc (GB) | peak RSS (MB) | read counts |
|---|---|---|---|---|---|---|---|
| as | 2406.3 | 1015.9 | 205.9 | 56.35 | 74.32 | 1292 | [32889] |
| int | 2410.2 (+0.2%) | 1020.3 (+0.4%) | 206.8 (+0.4%) | 56.30 (-0.1%) | 74.21 (-0.1%) | 1249 (-3.3%) | [32889] |

as rhombus walls: 205.9, 204.7, 206.1

int rhombus walls: 205.0, 206.8, 207.5
