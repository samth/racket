# Analysis controls — not submission candidates

The ten programs directly in this directory preserve the unselected versions measured during the
current Benchmarks Game study. They are comparison controls, not additional
submission candidates. The sole authoritative submission set is
[`../current/targets.json`](../current/targets.json): exactly one program for
each of the ten current Game families.

The controls include sequential alternatives, byte-key k-nucleotide, pure-Racket
pidigits, and direct-formula/sequential spectral norm. Their source bytes are
unchanged from commit `f45bf48b71`. They are retained for reproducibility, not
because they are necessarily incorrect or prohibited by the rules.

[`parallel-threads/`](parallel-threads/README.md) adds six optimized thread-pool
implementations for analysis on Racket CS 8.18.0.2 or newer. They are not extra
submission candidates; the site's published 8.15 runtime predates that API.

Historical benchmarks in the parent directory are also outside the submission
set. Keep the accompanying BSD `LICENSE` when redistributing these sources.
