# Parallel-thread implementations

Six standalone implementations using Racket's parallel threads, on the same
workloads as the selected current Benchmarks Game programs. These are analysis
controls, not six additional submission candidates. `../../current/targets.json`
remains the sole submission inventory, with exactly ten programs.

Requires **64-bit Racket CS 8.18.0.2 or newer**, with parallelism enabled.
The Game's published Racket measurements name 8.15, which predates this API;
these versions need a newer runtime before they could replace those submissions.
BC runs the threads concurrently but not in parallel.

[Scaling follow-up notes](SCALING.md) distinguish the open spectral-norm
scheduling question, deferred GC limits, source-level bottlenecks and startup
measurement effects.

| Program | Work distribution |
| --- | --- |
| `fannkuch-redux.rkt` | At most four persistent workers; n*(n-1) complete permutation blocks; reusable private fxvectors. |
| `spectralnorm.rkt` | One bounded pool, disjoint row intervals, joins between every matrix pass; unchanged denominator recurrence and summation order. |
| `mandelbrot.rkt` | At most four workers, cyclic rows, disjoint bitmap writes, separate row kernel; unchanged coordinates and escape recurrence. |
| `binarytrees.rkt` | Parent plus at most three workers, grouped depths, fresh pairs for every node and leaf. |
| `knucleotide.rkt` | Parent plus at most three workers dynamically claim 46 complete reading-frame histograms; initialized DNA bytes are shared read-only. |
| `regexredux.rkt` | Up to three pattern-count workers share read-only input; parent performs the five ordered substitutions using reusable output buffers. |

All joins report worker failure, output is ordered, and pools are closed after
their last worker is started. Closing a pool does not cancel its workers.
No matrix entries, permutations, histogram entries, tree nodes, regex passes
or pixel iterations required by the current algorithms are omitted.

```
racket fannkuch-redux.rkt 12
racket spectralnorm.rkt 5500
racket mandelbrot.rkt 16000 > bitmap.pbm
racket binarytrees.rkt 21
racket knucleotide.rkt < input25000000.txt
racket regexredux.rkt < input5000000.txt
racket tests/strings.rkt
```

The string-benchmark test checks every entry of every reading-frame histogram
against a byte-substring oracle, and compares complete regex intermediate
strings with `regexp-replace*`, including growing replacements and buffer reuse.
Nucleotide tables remain built-in `hasheq` tables, grown from their default
size. Regex matching remains in the standard engine; no passes are fused or
omitted. The selected place-based candidates also retain the portable buffer
and encoding improvements, without requiring the new thread API.

The [parallel-thread study](https://droplet.tail5921ac.ts.net/parallel-thread-compiler-report/)
records matched one/four-core timings, negative results, complete output/state
checks, compiler dumps, disassembly and runtime traces. The source/runtime
hashes in that evidence identify the measured versions. Faster parallel wall
time is not a single-core compiler improvement, and parallel threads are not
uniformly faster than futures or places.

The other four current families are not rewritten in this study: five-body
time steps and the pidigits spigot have serial dependencies; FASTA's exact RNG
and reverse-complement's input/output pipeline need separate partitioning work.
This is not a claim that FASTA or reverse-complement cannot be parallelized.
