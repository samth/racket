# SBCL code for four Benchmarks Game programs

SBCL 2.6.0.debian on huor (Intel Core Ultra 7 265, P-core 6), 2026-11-01.
Each program was compiled with its own declarations (`build.lisp`) and saved
as an executable. Times are the minimum of 3 runs of `perf stat`; cycle and
instruction counts are medians of user-mode `cpu_core` events. Outputs match
the Game's expected output.

Files in `asm/`:
- `PROGRAM.image.asm`: disassembly of the hot functions, printed by the same
  executable image that was timed, so the addresses match the profile.
- `PROGRAM.annotated.asm`: the same listing, with the share of all
  `cpu_core/cycles/pp` samples on each instruction (`hot.py`).
- `fannkuchredux-sbcl-5-runtime.asm`: the permutation function that this
  program builds and compiles at run time (shown for n=7).

| program | size | wall (s) | cycles (G) | instructions (G) | Racket on the same core |
|---|---|---|---|---|---|
| nbody-sbcl-2 (safety 0) | 50M | 1.81 | 9.20 | 42.0 | candidate nbody.rkt (flvector, unsafe): 1.93 s, 45.0G |
| nbody-sbcl-1 (default safety) | 50M | 1.89 | 9.59 | 63.9 | flvector, safe ops: 3.38 s, 139.5G |
| spectralnorm-sbcl-6 (default safety, 1 core) | 5500 | 2.38 | 12.12 | 35.1 | master spectralnorm.rkt, safe ops: 1.77 s, 82.0G; unsafe 1.19 s |
| fannkuchredux-sbcl-5 (1 core) | 11 | 1.05 | 5.31 | 8.7 | master fannkuch-redux.rkt, safe ops: 3.66 s, 127G |
| binarytrees-sbcl-1 (1 core) | 21 | 5.82 | 26.95 | 76.1 | bt-variants C (pairs): 2.0 s |

## n-body

Both SBCL programs represent a body as a `(vector double-float)` through
`defstruct (:type (vector double-float))`, the same layout as the Racket
candidate's flvector. Fields are raw doubles; reads and writes are
`MOVSD [reg+disp]` with no boxing. `applyforces` is declared inline, and its
arguments are declared `(vector double-float 7)`.

- **safety 0 (#2):** 107 instructions for `advance`. The hot loop is pure
  SSE arithmetic: `SUBSD`/`MULSD`/`ADDSD`, `SQRTSD`, `DIVSD`, and
  `MOVSD` loads and stores at fixed offsets. There are no type or bounds
  checks. Samples spread evenly, about 5% per arithmetic instruction. This
  matches the Racket candidate (1.81 vs 1.93 s).
- **default safety (#1):** 229 instructions, 52% more instructions executed
  than #2, but only 4% slower. Each body taken from the list is checked
  once: a lowtag test (`TEST AL, 15`) and a header test (`CMP BYTE PTR
  [reg-15], -27`, "is a simple-array double-float"). Because the
  declaration gives the length as 7, the field reads inside `applyforces`
  have **no bounds checks**. Only the position update in `advance`
  compares the length (`CMP QWORD PTR [RSI-7], 6/8/10`) for indices 0–2.
  The checks are well-predicted branches that overlap with the float
  arithmetic.

What Racket lacks: in the safe Racket flvector program, every `flvector-ref`
re-checks the vector's type and the index against the length. cptypes learns
that `b` is an flvector after the first access, but not its length. SBCL
gets the length from a declaration. A cptypes fact recording "`b` has length
greater than k", learned from one successful access, would let later accesses
at constant indices up to k skip the bounds check. That alone would remove most
of the 1.75x gap between the safe and unsafe Racket programs.

## spectral-norm

`eval-A` is inlined into the A·u loops. With `n` declared as a 31-bit integer,
SBCL keeps `i`, `j` and `n` as tagged fixnums and proves the denominator
arithmetic cannot overflow, so there are **no overflow checks**. The inner loop
is about 24 instructions:
- integer arithmetic on tagged values (`LEA`, `SAR`, `IMUL`), then
  `CVTSI2SD` and `DIVSD`;
- a vector check per element (`CMP BYTE PTR [RDI-14], 0`) and a bounds check
  (`CMP RSI, [RDI-7]; JAE`);
- a load, `MULSD`, and a load-add-store of `retval[i]`. The accumulator stays
  in memory: there is no scalar replacement.

Most samples fall on the store and add of the accumulator (10% each) and on
the divide chain. Racket already does better here: master's safe program runs
in 1.77 s against SBCL's 2.38 s, with the accumulator in a register.

## fannkuch-redux

`fannkuchredux-sbcl-5` builds Lisp source specialized to `n` at run time
(`mkperm`) and compiles it with `compile`. The permutation is held in `n`
separate fixnum variables instead of an array. The flip and rotate steps are
straight-line moves between registers, selected by a `case` on the flip
length. There are no arrays and no bounds checks at all, and only 8.7G
instructions run, against Racket's 127G (safe) or roughly 40G (the unsafe
candidate). This is run-time code generation, not a better general compiler.

## binary-trees

`binarytrees-sbcl-1` builds trees from conses: `(list item)` for a leaf and
`(cons item (cons left right))` for an interior node. That makes 2 or 3 conses
per node, with an integer item in every node, against Racket's 1 pair.
Allocation is inline in a thread-local allocation buffer (`thread.cons-tlab`,
with a pseudo-atomic bracket). `check-node` has no fixnum declaration on its
sum, so each node calls `SB-VM::GENERIC-+`. 46% of samples fall outside the
two tree functions, mostly in the GC. SBCL's single-core time (5.8 s) is
nearly 3x Racket's pair version.
