# Interned scopes and long modules

Sam Tobin-Hochstadt · branch [`scope-sets-r4`](https://github.com/samth/racket/tree/scope-sets-r4) · [racket/racket#5669](https://github.com/racket/racket/pull/5669)

The scope-set representation in [the main write-up](README.md) keeps each set as a list sorted newest first, so an operation costs time and space in proportion to the number of scopes in the set that are newer than the one it changes. Two patterns made the expander touch old scopes in large sets far more often than ordinary expansion does, and each had its own fix:

- **Interned scopes.** `make-interned-syntax-introducer` scopes had negative ids, so they were the oldest scopes in every set. Rhombus adds and removes them on every binding and reference, so an identifier deep inside nested binding forms paid for copying its whole set each time. Interned scopes now sort first, and the commit is [afcaf895da](https://github.com/samth/racket/commit/afcaf895da).
- **Long modules.** A module records its use-site scopes for serialization by adding them all to one syntax object, newest first. That puts each scope at the end of the list, so a module with thousands of macro uses in definition positions spent quadratic time and space on it. Adding a list of scopes is now one merge, and the commit is [ae557e7462](https://github.com/samth/racket/commit/ae557e7462).

Both came out of review comments on the PR. One asked about large and deeply nested programs in the teaching languages, which `local-expand` to control error order, and the other asked about code that relies on `make-interned-syntax-introducer`. The teaching languages turned out to be fine (see [Teaching languages](#teaching-languages)). Measuring the interned introducer exposed both problems. The long-module problem appeared in a flat module with many interned-introducer uses, and the same thing happens with any macro.

## Interned scopes first

Each scope's id comes from one counter, so newer scopes have larger ids, and a set's list is in decreasing id order. Interned scopes were the exception: they took a negative id, so that `scope-set-max`, which picks the scope that holds a set's bindings, would never pick one. With sorted lists, that put them at the end of every list.

Interned scopes now take ids above 2^59:

```racket
(define interned-scope-id-base (expt 2 59))

(define (make-interned-scope sym)
  ... (define id (+ interned-scope-id-base (new-scope-id!))) ...)
```

They sort as newer than every ordinary scope, so adding or removing one touches only the front of a set. `scope-set-max` keeps master's choice by skipping them, unless a set holds nothing else:

```racket
(define (scope-set-max s)
  (define l (scope-set-elems s))
  (let loop ([p l])
    (cond
      [(null? p) (car l)]
      [(< (scope-base-id (car p)) interned-scope-id-base) (car p)]
      [else (loop (cdr p))])))
```

Ids must be unique, because the sorted list for a set must be unique for `scope-set=?` to be correct. Interned ids come from the same counter as all other ids, offset by 2^59, so they cannot collide with each other. Ordinary ids cannot reach 2^59: loading `racket` from source creates about 540,000, and a full `raco setup` of main-distribution creates an estimated 27 million (about 2^25), judging from the rate measured while compiling typed/racket.

A base low enough to keep interned ids fixnums on a 32-bit platform, such as 2^28, would not be safe. The counter is shared by the whole process and never resets, so a long-running DrRacket session, sandbox or REPL server could reach it. Ordinary and interned ids would then collide, and `scope-set-max` would skip ordinary scopes. Both failures would silently produce wrong bindings. With 2^59, interned ids are bignums on 32-bit platforms, so comparisons that involve them take Chez's generic path there. That is a small constant per operation, and on 64-bit platforms the ids are fixnums.

Adding and removing an interned scope 20,000 times on an identifier inside D nested `let`s:

| D | master | interned scopes last | interned scopes first |
|---|---|---|---|
| 10 | 9.1 ms, 23 MB | 9.5 ms, 30 MB | 8.3 ms, 16 MB |
| 100 | 10.7 ms, 29 MB | 30.2 ms, 141 MB | 9.0 ms, 17 MB |
| 1000 | 87.2 ms, 183 MB | 385.4 ms, 1332 MB | 86.7 ms, 46 MB |

"Interned scopes last" is commit ae557e7462, the representation with negative interned ids, and "interned scopes first" is afcaf895da. With interned scopes first, time matches master at every depth and allocation is a quarter of master's or less.

Deeply nested Rhombus programs combine interned scopes with nested `local-expand`:

| Program | master | interned scopes last | interned scopes first |
|---|---|---|---|
| `block` nested 100 deep | 177 ms, 365 MB | 154 ms, 335 MB | 152 ms, 332 MB |
| `block` nested 300 deep | 640 ms, 939 MB | 718 ms, 790 MB | 572 ms, 764 MB |
| `block` nested 1000 deep | 5686 ms, 4389 MB | 6222 ms, 3631 MB | 5742 ms, 3332 MB |
| `fun` nested 100 deep | 199 ms, 370 MB | 171 ms, 332 MB | 171 ms, 327 MB |
| `fun` nested 300 deep | 909 ms, 996 MB | 858 ms, 832 MB | 825 ms, 794 MB |
| `fun` nested 1000 deep | 8159 ms, 5428 MB | 9682 ms, 4349 MB | 9228 ms, 3916 MB |

With interned scopes first, these are 9 to 14% faster than master at depths 100 and 300, within 1% of master for `block` at 1000, and 13% slower for `fun` at 1000. They allocate less than master in every case, down to 72% for `fun` at 1000. The remaining cost at depth 1000 comes from nested `local-expand` flipping old ordinary scopes, the cost-model case in the main write-up, not from interned scopes.

Interned scopes first costs nothing on ordinary compiles. Compiling typed/racket and the Rhombus `-lib` packages from source, medians of 3 alternating runs:

| Workload | interned scopes last | interned scopes first |
|---|---|---|
| typed/racket, instructions | 579.9 × 10⁹ | 579.8 × 10⁹ |
| typed/racket, expand allocation | 14.84 GB | 14.84 GB |
| typed/racket, wall time | 49.2 s | 49.8 s |
| Rhombus `-lib`, instructions | 2406 × 10⁹ | 2410 × 10⁹ |
| Rhombus `-lib`, expand allocation | 74.32 GB | 74.21 GB |
| Rhombus `-lib`, wall time | 205.9 s | 206.8 s |
| Rhombus `-lib`, peak RSS | 1292 MB | 1249 MB |

The wall-time differences are within run-to-run spread. The three typed/racket runs of each binary span 49.1 to 50.0 s.

## Long modules

The module expander removes all of a module's use-site scopes from the left-hand side of each definition, and when it finishes, it records those scopes for serialization by adding them to an empty syntax object:

```racket
(add-scopes empty-syntax (unbox (root-expand-context-use-site-scopes ctx)))
```

A module gets a use-site scope for each macro use in a definition context, so a module with N definitions whose right-hand sides use a macro has about N of them. The box holds them newest first. `add-scopes` added them one at a time, and each newer scope was already in the set, so each addition went to the end of the list and copied every scope before it. Recording N use-site scopes therefore copied about N²/2 pairs. On master, sets were `hasheq` tables, so the same loop cost N log N.

`remove-scopes` already turned a list of scopes into one sorted list of operations and applied it with a single merge, because removing a module's use-site scopes from each body form had the same problem. `add-scopes` now does the same:

```racket
(define (add-scopes s scs)
  (cond
    [(null? scs) s]
    [(null? (cdr scs)) (add-scope s (car scs))]
    [(for/and ([sc (in-list scs)])
       (and (scope? sc) (not (representative-scope? sc))))
     ;; Like `remove-scopes`, apply all at once; adding one at a time
     ;; would copy the set for each scope older than the newest
     (apply-scope-ops s (list->scope-ops scs 'add))]
    [else
     (for/fold ([s s]) ([sc (in-list scs)])
       (add-scope s sc))]))
```

`list->scope-ops` sorts the scopes newest first and drops duplicates, and `apply-scope-ops` merges the operations into the syntax object's set and its pending propagation in one pass each.

The test programs are `racket/base` modules of N definitions `(define gi (+ 1 (m x)))`, where `m` is an identity macro. `interned-flat-10000` is a chain of 10,000 definitions such as `(define g2 (+ 1 (sp-ref g1)))`, where the macro `sp-ref` adds an interned scope to its argument and looks up its binding:

| Program | master | adding one at a time | adding in one merge |
|---|---|---|---|
| 4,000 definitions | 171 ms, 276 MB | 161 ms, 370 MB | 136 ms, 240 MB |
| 8,000 definitions | 345 ms, 554 MB | 374 ms, 1036 MB | 274 ms, 483 MB |
| `interned-flat-10000` | 447 ms, 712 MB | 517 ms, 1507 MB | 357 ms, 619 MB |

Each cell is expand time and expand allocation from `PLT_EXPANDER_TIMES`. "Adding one at a time" is commit df459b17b3, and "adding in one merge" is ae557e7462. One at a time, allocation grows faster than the module and reaches twice master's. In one merge, both the time and the allocation are about 20% below master's. Sorting interned scopes first makes no difference to these flat programs: afcaf895da matches ae557e7462 to within 2 ms and 1 MB.

## Teaching languages

The teaching languages expand each definition with `local-expand` to report errors in source order, so deeply nested and large programs are where old-scope operations would show. They do not regress. These were measured on the representation commit alone (df459b17b3), since neither later change touches this pattern:

| Program | master | this change |
|---|---|---|
| ISL, `local` nested 1000 deep | 591 ms, 908 MB | 528 ms, 248 MB |
| ASL, `let` nested 1000 deep | 796 ms, 1489 MB | 747 ms, 459 MB |
| BSL, `cond` nested 1000 deep | 262 ms, 408 MB | 220 ms, 229 MB |
| ISL+, `lambda` nested 1000 deep | 230 ms, 366 MB | 195 ms, 122 MB |
| BSL, 2,000 functions | 3916 ms, 8295 MB | 2623 ms, 5911 MB |
| ISL, 2,000 functions | 1686 ms | 1432 ms |

## How the numbers were measured

All numbers come from Racket CS builds of each commit, measured before the branch was rebased from master 45ccd1c43a onto 51c0ade4b1; the rebase changed no expander code. Each binary replaces `bin/racket` in the same 9.4.0.5 tree. Runs are pinned to one P-core of an Intel Core Ultra 7 265 with nothing else running. Each small-program number is the minimum of 5 runs after a warm-up, and each compile number is the median of 3 rounds that alternate which binary runs first.

- Interned scopes and deep Rhombus programs: [`data/interned-first/results/`](data/interned-first/results/), with the raw runs and the script.
- Long modules: [`data/add-scopes/`](data/add-scopes/). `genx.py` generates the definition modules and `gen.rkt` the interned-introducer ones. `run1.sh` deletes `compiled/` and compiles one file with `managed-compile-zo`. The results and raw runs are in `results/`.
- Teaching languages: [`data/ryan/`](data/ryan/), with the generators `gen.py` and `gen2.py`, the benchmark driver `bench.rkt`, and `results.txt`.

The randomized test in `common/scope-set.rkt` covers both changes. It includes ids above 2^59, which count as interned ids, and also stand in for bignum ids on a 32-bit platform. It checks that `scope-set-max` skips them. Racket's core tests pass with all three commits.
