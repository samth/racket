# Add-scopes batching: results

Each program in `../progs` compiled with `compiler/cm`'s `managed-compile-zo` after deleting
its `compiled/` directory (`run.sh`), on P-core 3 of huor with nothing else running. Expand
time and allocation come from the `expand` row of `PLT_EXPANDER_TIMES`; wall is the whole
process. One warm-up process, then 5 measured processes per program and binary, binaries
alternating; the table shows the minimum of the 5 (`raw.txt` has every run).

- master: build-plot master tree (45ccd1c43a)
- fix: 853ff8ccea (scope sets, before batching add-scopes) — `hashvars/bins/racket-fix`
- as: 7a7c482433 (+ batched add-scopes) — `interned-first/racket-as`
- int: cd126ece77 (+ interned scopes first) — `interned-first/racket-int`

The last three run as bin/racket of a copy of the build-plot scope-sets-r4 tree.

| program | binary | expand (ms) | expand alloc (MB) | wall (ms) |
|---|---|---|---|---|
| x-fixed-4000 | master | 171 | 276 | 476 |
| x-fixed-4000 | fix | 161 | 370 | 459 |
| x-fixed-4000 | as | 136 | 240 | 431 |
| x-fixed-4000 | int | 137 | 241 | 428 |
| x-fixed-8000 | master | 345 | 554 | 791 |
| x-fixed-8000 | fix | 374 | 1036 | 813 |
| x-fixed-8000 | as | 274 | 483 | 702 |
| x-fixed-8000 | int | 274 | 483 | 705 |
| interned-flat-10000 | master | 447 | 712 | 982 |
| interned-flat-10000 | fix | 517 | 1507 | 1022 |
| interned-flat-10000 | as | 357 | 619 | 860 |
| interned-flat-10000 | int | 359 | 619 | 860 |
