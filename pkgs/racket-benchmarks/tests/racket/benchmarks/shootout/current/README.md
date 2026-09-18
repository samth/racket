# Current Benchmarks Game optimization targets

These standalone programs target the ten families currently included on the
[Benchmarks Game site](https://benchmarksgame-team.pages.debian.net/benchmarksgame/).
The optimized fixnum domains assume 64-bit Racket CS, matching the measured
platform; 32-bit operation is not a target of this study.
They are the optimization targets for those families. The parent directory
preserves historical shootout programs, including workloads no longer in the
Game. Historical results are not automatically results for these programs.

`targets.json` lists each primary target, all candidates, SHA256 hashes, rule URLs, exact small
fixtures, dependencies and parallel/experimental status. Every program runs
directly with Racket; no report harness or compiler dump library is required:

```
racket nbody.rkt 50000000
racket fasta.rkt 25000000 > input25000000.txt
racket knucleotide.rkt < input25000000.txt
racket fasta.rkt 5000000 > input5000000.txt
racket regexredux-par.rkt < input5000000.txt
```

Use FASTA N=5000000 for the regex-redux performance workload. The programs
and their original contributors retain the accompanying Benchmarks Game BSD
license. Do not remove `LICENSE` when redistributing the set.

## Submission-oriented choices

- `fasta.rkt` generates every random value and searches cumulative
  probabilities. The historical cached-period implementation is excluded.
- `knucleotide.rkt` packs DNA keys into fixnums and uses Racket's built-in
  `hasheq`. `knucleotide-bytes.rkt` retains the measured byte-key alternative.
  The large-input primary, `knucleotide-par.rkt`, distributes complete
  histograms over three execution contexts, trading more memory for lower wall time.
  All three count every required histogram, extract only THREE, and sort frequency
  ties by key. No benchmark-specific hash-table implementation is introduced.
- `binarytrees.rkt` uses the current allocation/checking workload rather than
  the old signed payload checks. Every node, including leaves, is allocated.
- `regexredux.rkt` and its parallel alternate use the current five ordered
  substitutions, not the historical eleven IUB replacements.
- `pidigits.rkt` uses the permitted GMP library and performs both candidate
  extractions on each transition. `pidigits-pure.rkt` uses Racket integers.
- `spectralnorm.rkt` and `spectralnorm-par.rkt` genuinely use all four
  required procedures, including the matrix-element function. Files named
  `spectralnorm-recurrence*` are **experimental**, pending clarification of
  the four-procedure rule; they are not submission candidates.
- Parallel variants retain the complete workload and ordered output. Their
  multicore wall times must not be presented as single-core compiler gains.

The target is the fastest implementation consistent with the rules, not a
promise that optimization is exhausted. Keep measured losing controls in the
analysis evidence instead of silently replacing a faster working candidate.

## Conformance checks

```
python3 check.py --racket /path/to/racket
```

This offline smoke check verifies source hashes and compares each candidate
against the site's downloaded official small output. Its byte-exact nbody
comparison is stricter than the site's absolute-error tolerance; a differing
cross-platform result needs that numerical tolerance check before rejection.
GMP must be available
for `pidigits.rkt`. Use `--experimental` to check the recurrence controls too.
For an uninstalled Racket CS executable, `--collects`, `--config` and
`--compiled-root` select its collection/configuration/cache paths.

The detailed audit additionally checks full internal states/histograms,
larger inputs, boundary cases, compiler passes and generated assembly. Small
output equality alone does not establish compliance with the required work.

These are candidates for eventual submission. Nothing has been submitted;
final acceptance is the Game maintainers' decision. Website sources are
comparison references, not exemptions from the current written rules.
