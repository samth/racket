# Racket CS runtime concurrency measurements (Oct 31 – Nov 3, 2026)

Benchmarks, drivers, and raw results from measuring Racket CS parallel
threads, pool-worker spinning, and hash-table lock designs on huor (Intel
Core Ultra 7 265; P-cores 0-7, E-cores 8-19). Each build is `make base
PKGS=""` from the samth/racket branch named below. Unless noted, runs are
interleaved across builds; single-threaded runs use CPU 5, 4-CPU runs use
CPUs 4-7 (0-3 in the earliest runs), and 5-CPU runs use 3-7.

`results.tar.xz` unpacks to `rkt-conc/results/` and `sched-stagger/`.

## bench/

| file | what it measures |
|---|---|
| `spectral-pt.rkt` | spectral-norm (N from env `N`, default 5500) with 4 parallel threads; prints compute time |
| `stagger.rkt` | 40 fork-join passes of 4 pool threads with a boxing (GC-heavy) loop; per-pass span, start stagger, chunk times |
| `bt-threads.rkt`, `bt-par.rkt` | binary-trees with parallel threads and with places (from branch nbody-flvector-loop-entry) |
| `hash-par.rkt` | 20M `hash-ref` and `hash-set!` on a small mutable `hasheq`; `PAR=1` first creates a parallel thread |
| `stress.rkt` | `W` parallel threads mixing set/ref/remove on shared `hasheq` and `hasheqv`; checks counts and own writes |
| `shared-read.rkt` | `R` readers doing `hash-ref` on one shared table; `WRITER=1` adds an occasional writer |
| `shared-write.rkt` | `W` threads doing 2M `hash-set!` each on one shared table |
| `private-tables.rkt` | `W` threads each counting into a private `hasheqv` |
| `resize-stress.rkt` | correctness: a writer grows and shrinks a table (resizes) while 3 lock-free readers check stable keys |
| `pub-test.rkt` | correctness: a writer stores fresh pairs and 4-slot vectors while 3 readers check they are fully initialized |
| `compile-big.rkt` | expands and compiles four large collection modules from source |
| `idle.rkt` | one round of 4 pool threads, then a 2 s sleep; CPU used while the pool is idle |
| `park-stats.rkt` | runs another benchmark, then prints the park/timeout/late counters of the seqpark2b/3/3b variants |

## scripts/

| file | purpose |
|---|---|
| `run.sh` | master vs hash-lock-fastpath vs hash-lock-spin (`hash-*`, `bt-*`, `stress-*`, `spectral-0*`) |
| `run2.sh`, `run3.sh` | fastpath vs pool-worker-spin (`ws-*` on CPUs 0-3, contaminated; `ws47-*` on CPUs 3-7/4-7) |
| `run4.sh` | hash-lock prototypes (`h-*`) |
| `run5.sh` | seqpark (`s-*`) |
| `run6.sh` | seqpark2/2b (`t-*`) |
| `run7.sh`, `run7-wrap.sh` | seqpark3/3b (`u-*`, with load log) |
| `run8.sh`, `run8-wrap.sh` | seqpark4 variants (`v-*`, with load log) |
| `run9.sh` | pool-worker spin variants (`w-*`) |
| `prep-bench.sh` | copies `bench/` into `bench-<build>` and compiles it with that build's `compiler/cm` |
| `build4.sh` | parallel builds pinned to CPUs 8-19, selecting a variant with `RACKET_HASH_LOCK` |
| `summarize.py` | median [min-max] tables from a results directory (`BUILDS`, `PREFIX` env vars) |
| `racket-wrap.sh` | stand-in `racket/bin/racket` for `zuo . schemified` with a base build that lacks `raco make` |
| `edit_*.py` | the scripted edits that added each lock variant to `rumble/lock.ss` |
| `brackets.py` | bracket-balance checker for Scheme files |

## sched-stagger/

The first investigation: why a 4-worker program ran faster on 5 CPUs.
`threadpoll.py` reports per-OS-thread run and run-queue wait from /proc,
`timeline.py` turns `perf script` samples into a per-CPU timeline, and
`pinworkers.py` pins each pool worker to its own CPU. Measured with the
installed Racket 9.3.0.10 [cs].

## results (in results.tar.xz)

| prefix | builds (samth/racket branch @ commit) | summary |
|---|---|---|
| `bt-*`, `hash-*`, `stress-*`, `spectral-0*`, `gc/`, `bt-par-rerun/`, `compile-big/` | master @ 2943cc5016, hash-lock-fastpath @ d3df21e7fc, hash-lock-spin @ 8b6d7eb7c7 | `summary.md` |
| `ws-*`, `ws47-*`, `threadpoll/` | hash-lock-fastpath @ d3df21e7fc, pool-worker-spin @ 12a06cdd23 | `summary-workerspin*.md` (`ws-*` contaminated by a job on CPU 2) |
| `h-*` | master, fastpath, spin, and hash-lock-protos @ df6b2000ae built as `yield`, `park`, `bias`, plus hash-lock-protos-seq @ d5786eade5 built as `seq` | `summary-hashlock.md` |
| `s-*` | master, fastpath, seq, park, hash-lock-seqpark @ 4e71db6099 | `summary-seqpark.md` |
| `t-*` | master, seqpark, hash-lock-seqpark2 @ 7569f4abe8 built as `seqpark2` and `seqpark2b` | `summary-seqpark2.md` (contaminated by other jobs) |
| `u-*` | master, seqpark, seqpark2/2b, hash-lock-seqpark3 @ 0487a446c4 built as `seqpark3` and `seqpark3b` | `summary-seqpark3.md`, `u-load.txt` |
| `v-*` | master, seqpark, seqpark3, and hash-lock-seqpark4 @ 9716df3d15's predecessor built as `seqpark4t`, `seqpark4r` (the final `seqpark4`), and both combined (labeled `p-seqpark4` in this run) | `summary-seqpark4.md`, `v-load.txt` |
| `w-*` | fastpath, pool-worker-spin @ 12a06cdd23, and a prototype with env-selected spin length and mode (`pool-worker-spin2-prototype.diff`); the chosen 1 ms spin is pool-worker-spin2 @ faaa4cb81b | `summary-workerspin2.md` |
| `correctness/` | stress, resize-stress, shared-write and pub-test runs for each lock variant | |
| `build-*.log` | build logs | |

The Chez Scheme change that lets eq-hashtable lookups run without a lock
is samth/ChezScheme branch eq-hashtable-concurrent-read @ 298bab20.
