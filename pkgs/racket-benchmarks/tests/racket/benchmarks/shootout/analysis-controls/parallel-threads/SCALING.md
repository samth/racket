# Parallel-scaling follow-up notes

Local measurements: Intel Core Ultra 7 265, x86-64 Linux, Racket CS 9.3.0.8
with the regenerated schemify loop-entry fix; CPUs 0–3 are distinct performance
cores. Frequency was not locked. Times exclude process startup, compilation
and dependency initialization, but include input processing, workers, joins
and output construction. These are not the Game's historical measurements.
Most medians use three shuffled repetitions; spectral-norm's parent A/B uses
five. No collector or backend change is part of these controls.

## Open scheduling question: spectral norm

Four pool workers plus a coordinating parent perform forty dependent matrix
passes. At N=5500, traces show a median worker-start stagger of 2.97 ms even
though an uninterrupted chunk takes about 7.2 ms. Some equal-work chunks take
approximately twice as long as others. Gaps between passes total only 11.05 ms;
GC overlaps only 2.65 ms of the traced 425.64 ms worker region.

- Keeping the source and four computation workers unchanged, allowing CPUs
  0–4 instead of 0–3 changes a batch's median from 436.91 to 300.24 ms. This
  intentionally allows an extra CPU; it is not a four-core speedup result.
- On CPUs 0–3, using three pool workers and having the parent compute the
  remaining quarter changes 430.87 to 297.24 ms. One-core times are 1132.07
  and 1134.42 ms; aggregate four-core CPU is 1160 and 1153 ms. The latter
  scales 3.82x with essentially unchanged work. Every bit of all three result
  vectors matches at N=1, 2, 7, 100, 257 and 5500. Worker recurrence assembly
  is unchanged apart from relocated constants.
- A persistent-worker/fsemaphore control did not help: 420.60 to 462.58 ms
  in its paired batch. Thread creation alone is not an established cause.

This establishes an avoidable scheduling interaction on this host, not yet a
specific Racket or Linux scheduler defect. The queue/condition-variable path
is in `racket/src/thread/future.rkt`, especially `schedule-future!` and
`start-worker`. Next: reduce to equal compute chunks and barriers, then trace
per-pthread wake-ups, CPU placement and context switches. Do not assume that
keeping workers alive fixes the problem. These diagnostic spectral controls
are not source changes in this commit.

## Deferred structural limit: binary-trees

At N=21, elapsed time improves only 2011 to 1640 ms (1.23x). Aggregate CPU
rises 2010 to 2791 ms; reported GC CPU rises 866 to 1371 ms, on about 9.83 GB
of cumulative allocation. Serial stretch/long-lived construction and unequal
costs among coarse depth-task groups also matter.

The evidence supports substantial shared-heap GC cost, but does not isolate
allocation locks, collection synchronization, bandwidth and task imbalance.
The compared OCaml entry forks independent process heaps; it is not a
shared-heap-thread comparison. More parallel Racket/Chez GC is a larger
project and is explicitly deferred. No GC tuning or shared-leaf shortcut
was introduced.

## Source-level bottlenecks addressed here

K-nucleotide formerly left five histograms on the parent and had only two
background workers. Forty-six complete reading-frame tasks, claimed by the
parent and three workers, reduce four-core time at FASTA N=25000000 from
9.516 to 5.169 s. Scaling improves 1.53x to 2.90x; one-core time rises 2.7%.
Parsing/encoding remains serial, and the length-1 frame is a relatively large
indivisible task. Four-core GC CPU rises 351 to 402 ms. Private built-in
tables still pay generic wrapper/locking costs; no table is shared for
mutation, so these results do not establish contention on a common table.

Regex-redux's replacement chain formerly dominated the parent. Buffer reuse
and three counting workers reduce four-core time at FASTA N=5000000 from
5.172 to 1.740 s, improving scaling 1.61x to 3.07x and reducing allocation
about 80%. The five replacements remain dependent and ordered; that residual
serial chain is not a scheduler defect. A general literal-replacement fast
path in the regex library is a separate opportunity, independent of GC redesign.

## Measurement artifact, not an established runtime problem

Mandelbrot and fannkuch-redux already scale 3.81x and 3.79x when startup and
loading are excluded. Earlier small full-CLI runs substantially understated
their computational scaling. Startup remains a real end-to-end cost, but is
not evidence that arithmetic workers fail to execute concurrently.

Raw measurements, traces, diagnostic controls and compiler/disassembly evidence
remain in the analysis workspace's `docs/parallel-scaling-diagnosis/` and
`docs/regex-knucleotide-tuning/` directories, not in this source commit.
The checked-in correctness tests are `tests/strings.rkt`; the sole submission
inventory remains `../../current/targets.json`.
