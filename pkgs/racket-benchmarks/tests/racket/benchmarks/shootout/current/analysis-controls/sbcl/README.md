# SBCL reference programs

Lisp SBCL programs from the Computer Language Benchmarks Game, fetched
2026-11-01 from
https://benchmarksgame-team.pages.debian.net/benchmarksgame/program/NAME-sbcl-N.html
(the source shown on each program page). They are kept for comparing
compiler output with the Racket shootout programs; they are not Racket
benchmarks. The programs keep their contributors' credits and are under
the Game's 3-Clause BSD license,
https://benchmarksgame-team.pages.debian.net/benchmarksgame/license.html

Elapsed seconds, total CPU seconds and approximate CPU load, from each
benchmark's performance page on the same date:

| program | elapsed s | CPU s | note |
|---|---|---|---|
| nbody-sbcl-2 | 7.83 | 7.81 | fastest SBCL |
| nbody-sbcl-1 | 9.28 | 9.27 | plainer |
| spectralnorm-sbcl-3 | 1.47 | 5.72 | fastest SBCL, multicore |
| spectralnorm-sbcl-7 | 1.47 | 5.72 | multicore |
| spectralnorm-sbcl-2 | 1.48 | 5.72 | multicore |
| spectralnorm-sbcl-6 | 5.36 | 5.35 | single core |
| spectralnorm-sbcl-1 | 5.37 | 5.35 | single core, plainer |
| spectralnorm-sbcl-9 | | | listed, no timing row found |
| fannkuchredux-sbcl-4 | 9.41 | 36.94 | fastest SBCL, multicore |
| fannkuchredux-sbcl-5 | 30.88 | 30.86 | single core |
| fannkuchredux-sbcl-2 | 48.05 | 48.02 | single core, plainer |
| fannkuchredux-sbcl-3 | | | listed, no timing row found |
| binarytrees-sbcl-3 | 2.83 | 7.18 | fastest SBCL, multicore |
| binarytrees-sbcl-1 | 9.60 | 9.58 | single core, plainer |
