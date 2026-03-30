# Design Note: Coverage-Guided Extension for rackcheck

## Summary

This extension adds a coverage-guided feedback loop to rackcheck. It is
implemented as `rackcheck/guided`, a sibling module to `rackcheck/prop`
and `rackcheck/rackunit`.

## Why inside rackcheck rather than a separate package

The guidance loop is structurally identical to rackcheck's `check` function:
it calls the property's generator to produce shrink-trees, extracts values,
runs the test function, and descends shrink trees on failure. The only
difference is that it also collects coverage feedback between steps.

Making this part of rackcheck:
- Eliminates dependency on rackcheck's `(module+ private)` submodules
- Uses three new public accessors added to `prop.rkt`: `property-gen`,
  `property-proc`, and `property-arg-ids`
- Shares the `shrink-tree` struct (already public)
- Allows `(require rackcheck)` to provide the full API

## What was added to rackcheck's public API

In `prop.rkt`, three accessors were added:

```racket
(property-gen p)       ; -> gen? — the generator (produces shrink-trees)
(property-proc p)      ; -> procedure? — the test function
(property-arg-ids p)   ; -> list? — argument names
```

These enable custom check loops without reaching into `(module+ private)`.

## Module structure

```
rackcheck-lib/
  prop.rkt              ; existing + 3 new public accessors
  guided.rkt            ; public API for coverage-guided testing
  guided/
    config.rkt          ; guided-config struct
    coverage.rkt        ; errortrace instrumentation, snapshot, diff
    corpus.rkt          ; corpus management, interestingness criteria
    mutation.rkt        ; type-aware value mutation
    guidance.rkt        ; main loop (peer of check in prop.rkt)
    shrinking.rkt       ; type-aware shrinking for mutated inputs
    tests/              ; test suite
```

## errortrace dependency

The only new external dependency is `errortrace-lib`, which is part
of the standard Racket distribution. It is used exclusively by
`guided/coverage.rkt` for expression-level execution counting.

## How coverage feedback works

errortrace's `execute-counts-enabled` parameter and `get-execute-counts`
function provide per-expression execution counts. These counts are
cumulative and cannot be reset, so we use a diff-based approach:

1. Snapshot counts before running a test input
2. Run the property
3. Snapshot counts after
4. Diff to get this input's coverage contribution

An input is "interesting" if it:
- Triggers coverage points not previously seen
- Has a novel coverage signature (different combination of points)
- Pushes execution counts past power-of-2 thresholds

Interesting inputs are saved to a corpus and used to generate future
inputs via type-aware mutation.
