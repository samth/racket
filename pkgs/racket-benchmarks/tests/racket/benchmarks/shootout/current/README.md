# Current Benchmarks Game programs

One program for each of the ten benchmarks currently in the
[Computer Language Benchmarks Game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/).
The programs in the parent directory are older versions, kept so that
their timings stay comparable over time.

| Benchmark | Program | Parallelism |
| --- | --- | --- |
| binary-trees | `binarytrees.rkt` | parallel threads |
| fannkuch-redux | `fannkuch-redux.rkt` | parallel threads |
| fasta | `fasta.rkt` | none |
| k-nucleotide | `knucleotide.rkt` | parallel threads |
| mandelbrot | `mandelbrot.rkt` | parallel threads |
| n-body | `nbody.rkt` | none |
| pidigits | `pidigits.rkt` | none (uses GMP through the FFI) |
| regex-redux | `regexredux.rkt` | parallel threads |
| reverse-complement | `revcomp.rkt` | none |
| spectral-norm | `spectralnorm.rkt` | parallel threads |

The parallel programs use `thread` with `#:pool`, so they need Racket CS
8.18.0.2 or newer; on BC, the threads run concurrently but not in parallel.

Each program takes the Game's command-line argument, and k-nucleotide,
regex-redux and reverse-complement read FASTA input on standard input,
which `fasta.rkt` produces:

```
racket nbody.rkt 50000000
racket spectralnorm.rkt 5500
racket fannkuch-redux.rkt 12
racket binarytrees.rkt 21
racket pidigits.rkt 10000
racket fasta.rkt 25000000 > input25000000.txt
racket knucleotide.rkt < input25000000.txt
racket mandelbrot.rkt 16000 > output.pbm
racket regexredux.rkt < input5000000.txt
racket revcomp.rkt < input25000000.txt
```

`racket check.rkt` runs every program on the Game's small inputs and
compares the output with the Game's expected output in "test-data"
(see "test-data/provenance.json"). `racket tests/strings.rkt` checks the
k-nucleotide histograms and regex-redux substitutions against simple
reference implementations.

The programs are under the Benchmarks Game's revised BSD license; see
"LICENSE".
