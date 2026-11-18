# Interned-scope prefix lengths

With interned scopes sorted first (afcaf895da), the interned scopes of a scope set form a
prefix of its list, and an operation on a normal scope walks past that prefix before it
reaches its position. This measures how long that prefix is in practice, to judge whether
keeping interned and normal scopes in separate lists would pay off.

## Method

`instrument.py` adds counters to `racket/src/expander/common/scope-set.rkt` of the
interned-first expander (`instrument.patch` is the resulting diff; the expander code is the
same in afcaf895da and in cd126ece77, the commit it was taken from). For each operation it
records how many interned scopes (id at least 2^59) the set holds and, for an operation on
a normal scope, how many interned scopes it walks past:

- `update`: `scope-set-add`, `-remove` and `-flip`, including single-operation
  `scope-set-apply-ops`
- `member`: `scope-set-member?`
- `apply`: multi-operation `scope-set-apply-ops`, measured to the newest scope it changes
- `subset`: `scope-set-subset?` when the sets are not `eq?` (both sets' interned scopes)
- `equal`: `scope-set=?` when hash codes and counts match, so it walks the lists (both
  sets' interned scopes)

`run.sh` runs the instrumented expander from source with `racket/src/expander/run.rkt` on
a 9.4.0.5 build of the branch, compiling each workload and everything it requires from
source in one process:

- `rhombus`: the `rhombus` language
- `shrubbery`: the `shrubbery` reader and its libraries
- `rhm-fun-300`, `rhm-block-300`: the deep Rhombus programs from `../ryan/gen2.py`
  (functions or blocks nested 300 deep). These runs also compile `rhombus`, so
  `summarize.rkt` subtracts the `rhombus` run to isolate the program.
- `typed-racket`: `typed/racket`, for comparison

Rhombus's amalgam step runs the demodularizer at compile time, which needs the compilation
manager and fails under `run.rkt`, so the Rhombus runs use a copy of the tree with
`#:no-demod` enabled in `rhombus-lib/rhombus/private/amalgam.rkt`. That skips only the
demodularization of the already-expanded modules. The paths in `run.sh` are those of the
measurement machine.

`results/*.stats` hold the raw histograms, and `results/summary.md` is the output of
`summarize.rkt`.

## Results

"Held" is the number of interned scopes in the set (or in both sets, for `subset` and
`equal`); "walked" is the number of interned scopes an operation on a normal scope passes.

| workload | operation | ops | with interned | held mean / p50 / p99 / max | walked: mean / p50 / p99 / max |
|---|---|---|---|---|---|
| rhombus | update | 7,052,012 | 2.0% | 0.02 / 0 / 1 / 2 | 0.01 / 0 / 1 / 2 |
| rhombus | member | 7,322 | 100% | 1 / 1 / 1 / 1 | — |
| rhombus | apply | 3,403,831 | 0.2% | 0.00 / 0 / 0 / 1 | 0.00 / 0 / 0 / 1 |
| rhombus | subset | 517,490 | 28.1% | 0.44 / 0 / 2 / 2 | — |
| rhombus | equal | 4,886,090 | 0.7% | 0.01 / 0 / 0 / 4 | — |
| rhm-fun-300 | update | 143,187 | 4.9% | 0.05 / 0 / 1 / 1 | 0.04 / 0 / 1 / 1 |
| rhm-fun-300 | apply | 69,609 | 1.3% | 0.01 / 0 / 1 / 1 | 0.01 / 0 / 1 / 1 |
| rhm-fun-300 | subset | 9,149 | 54.1% | 0.58 / 1 / 2 / 2 | — |
| rhm-fun-300 | equal | 40,366 | 4.8% | 0.10 / 0 / 2 / 2 | — |
| rhm-block-300 | update | 142,570 | 5.0% | 0.05 / 0 / 1 / 1 | 0.04 / 0 / 1 / 1 |
| rhm-block-300 | apply | 80,087 | 1.1% | 0.01 / 0 / 1 / 1 | 0.01 / 0 / 1 / 1 |
| rhm-block-300 | subset | 6,882 | 20.8% | 0.21 / 0 / 1 / 1 | — |
| rhm-block-300 | equal | 55,966 | 2.0% | 0.04 / 0 / 2 / 2 | — |
| shrubbery | all | 4,768,900 | 0% | 0 | 0 |
| typed-racket | all | 12,748,434 | 0% | 0 | 0 |

No set ever holds more than two interned scopes, and nearly all operations see none:
98% of updates while compiling `rhombus`, and 95% in the deep programs, involve a set with
no interned scope at all. An operation on a normal scope walks past 0.01 to 0.04 interned
scopes on average and never more than 2. Keeping interned scopes in a separate list would
save at most two comparisons on a few percent of operations, so the single list with
interned scopes first is enough for current code. Outside Rhombus (`shrubbery`,
`typed/racket`) no scope set contains an interned scope.
