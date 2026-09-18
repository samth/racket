# Current Benchmarks Game submission candidates

Exactly one standalone candidate is selected for each of the ten benchmarks
currently in the [Benchmarks Game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/).
`targets.json` is the authoritative submission and optimization inventory.
There are no legacy benchmarks or alternative candidates in this directory.

| Benchmark | Sole candidate |
| --- | --- |
| binary-trees | `binarytrees-par.rkt` |
| fannkuch-redux | `fannkuch-redux-par.rkt` |
| fasta | `fasta.rkt` |
| k-nucleotide | `knucleotide-par.rkt` |
| mandelbrot | `mandelbrot-par.rkt` |
| n-body | `nbody.rkt` |
| pidigits | `pidigits.rkt` |
| regex-redux | `regexredux-par.rkt` |
| reverse-complement | `revcomp.rkt` |
| spectral-norm | `spectralnorm-recurrence-par.rkt` |

Unselected versions are preserved in `../analysis-controls/` for experiments
and reproducibility only. Historical programs in the parent directory are
not submission candidates either. Improving or replacing a candidate must
preserve the one-program-per-benchmark invariant.

The measured platform is 64-bit Racket CS; 32-bit operation is not a target.
All programs use the standard Racket distribution, except `pidigits.rkt`,
which also requires the permitted GMP shared library. No compiler-dump or
report harness package is needed to run them:

```
racket nbody.rkt 50000000
racket fasta.rkt 25000000 > input25000000.txt
racket knucleotide-par.rkt < input25000000.txt
racket fasta.rkt 5000000 > input5000000.txt
racket regexredux-par.rkt < input5000000.txt
```

FASTA generates every random value; k-nucleotide builds all seven histograms
using a built-in hash table; binary-trees allocates every node; regex-redux
performs the current five ordered substitutions; pidigits retains both
extractions. The spectral-norm denominator recurrence has direct precedent in
the currently listed [C++ #6](https://benchmarksgame-team.pages.debian.net/benchmarksgame/program/spectralnorm-gpp-6.html)
and [#5](https://benchmarksgame-team.pages.debian.net/benchmarksgame/program/spectralnorm-gpp-5.html).
Its former experimental rules label has been withdrawn. Parallel execution
does not omit work and must not be reported as a single-core compiler gain.

## Checks

```
python3 check.py --inventory-only
python3 test-checker.py
python3 check.py --racket /path/to/racket
```

The checker rejects duplicate candidates, missing or extra programs, analysis
controls, legacy families, and changed source hashes. It then compares the ten
programs with the downloaded official small fixtures. For an uninstalled
runtime, `--collects`, `--config` and `--compiled-root` specify its paths.
The byte-exact n-body check is stricter than the site's absolute-error
tolerance; differing cross-platform output needs that tolerance check before
rejection. Larger-input, complete-state and measurement evidence is in the
associated report; small output equality alone does not prove compliance.

The programs retain their contributors' accompanying BSD `LICENSE`. These
are candidates for eventual submission, not a claim of maintainer acceptance
or globally optimal performance. Nothing has been submitted to the Game.
