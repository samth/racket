# Design Note: Coverage-Guided Property-Based Testing for Racket

## Overview

This library combines rackcheck's property-based testing framework with
errortrace's runtime coverage instrumentation to create a feedback-directed
testing loop. Instead of generating test inputs blindly, the system tracks
which inputs trigger new code coverage and uses that information to guide
future input generation.

## Architecture

```
main.rkt                    Public API (re-exports rackcheck + guided testing)
private/
  config.rkt                Configuration struct
  coverage.rkt              errortrace instrumentation, snapshotting, diffing
  corpus.rkt                Corpus management, interestingness criteria
  mutation.rkt              Type-aware value mutation
  guidance.rkt              Main guidance loop
  shrinking.rkt             Type-aware shrinking for mutated inputs
```

## Key Design Decisions

### 1. Building on rackcheck (not quickcheck)

rackcheck provides:
- Generators with integrated shrink trees (`gen:natural`, `gen:list`, etc.)
- A `property` macro with named bindings
- A `check` function with configurable seed, test count, size, and deadline
- rackunit integration

We extend rackcheck by replacing its `check` loop with a guided version
that intercepts each test execution to collect coverage feedback.

### 2. errortrace for inner-loop feedback

errortrace provides two relevant APIs:

- **Execute counts** (`execute-counts-enabled`, `get-execute-counts`):
  Expression-level execution counts. Each entry maps a syntax object
  (with source, line, col, position, span) to an execution count.
  Counts are cumulative and cannot be reset.

- **Test coverage** (`coverage-counts-enabled`, `get-coverage`):
  Boolean coverage (covered vs not covered). Less granular than execute counts.

We use execute counts because they provide richer feedback:
- Expression-level granularity (not just line-level)
- Execution frequency information (not just covered/uncovered)
- Can detect "deeper" execution of loops and recursive paths

**Coverage diffing strategy**: Since execute counts are cumulative and
cannot be reset, we snapshot before each test and diff after. This is
efficient and correct — each diff represents exactly one test's coverage
contribution.

### 3. Coverage representation

Coverage points are normalized to `(list source-path position span)`:
- `source-path`: String representation of the syntax source
- `position`: 1-based character position in the source file
- `span`: Number of characters in the expression

This is stable across runs (unlike syntax object identity) and precise
enough to distinguish individual expressions.

### 4. Interestingness criteria

An input is "interesting" if its coverage diff satisfies any of:

1. **New coverage**: The diff contains coverage points not in the
   corpus's global coverage set.
2. **Novel signature**: The hash of the coverage signature (set of
   hit points) has not been seen before.
3. **Count threshold crossing**: Some execution count crossed a
   power-of-2 boundary (1→2, 3→4, 7→8, etc.), indicating deeper
   execution of a loop or recursive path.

### 5. Two-level mutation strategy

Since rackcheck generators produce typed Racket values (not bytes),
mutations operate at the value level:

- **Integers**: +/-1, perturbation, halving, doubling, negation, powers of 2
- **Strings**: insert/delete/replace char, truncate, duplicate
- **Lists**: insert/delete/replace element, shuffle, splice from corpus
- **Booleans**: flip
- **Pairs/Vectors**: recursive mutation
- **Bytes**: insert/delete/replace byte

For cross-corpus mutation, we also support **splicing**: taking a prefix
of one corpus entry and a suffix of another.

The mutation rate (configurable, default 50%) controls how often we
mutate a corpus entry vs generate a fresh input from the rackcheck
generator.

### 6. Shrinking

Two modes:
1. **Fresh generation failures**: The shrink tree from rackcheck's generator
   is available, so we use rackcheck's own shrink descent (greedy search
   through the tree of smaller alternatives).
2. **Mutation-based failures**: No shrink tree is available, so we use
   custom type-aware shrinking that tries to minimize the input while
   preserving the failure. This is simpler than rackcheck's integrated
   shrinking but effective for common types.

Shrinking is a separate phase after failure discovery. It does not
use coverage feedback — failure preservation is the primary objective.

### 7. In-process execution

errortrace instrumentation works within a single process and namespace.
Target modules are loaded once via `dynamic-require` with the errortrace
compile handler active. All subsequent function calls are automatically
tracked. No subprocess isolation is needed for the basic case.

**Caveat**: errortrace state (execute counts) is global and cumulative.
This means running multiple guided checks in the same process will see
accumulated coverage from prior runs. The diff-based approach handles
this correctly for individual test inputs, but the corpus's interestingness
criteria may be affected (later runs may find fewer "novel" points).

## Technical Questions Answered

### Can errortrace coverage be reset between tests?
No. Execute counts are stored in a module-level `hasheq` in errortrace-lib
and cannot be cleared from outside. We use diffing instead.

### Does per-input coverage require recompilation?
No. The target module is compiled once with errortrace's compile handler.
All subsequent calls to its functions are tracked via the inserted
instrumentation. No recompilation is needed per test input.

### Is syntax-object identity stable enough for signatures?
No. We normalize to `(list source-path position span)` which is stable
across calls within a single process run.

### How does macro expansion affect coverage?
errortrace instruments expanded syntax. The `position` and `span` in
the execute counts refer to the original source locations, not the
expanded code. This means coverage of macro-generated code maps back
to the macro call site, which is the desired behavior.

### How to handle serialization for corpus replay?
For fresh-generation inputs, we can regenerate from the RNG seed and
iteration number. For mutated inputs, we store the input value directly
(it's already a Racket value in memory). For persistence across processes,
inputs would need to be serialized — this is a known limitation of the
current prototype.

## Performance Characteristics

The main overhead comes from:
1. **Coverage snapshotting** (`get-execute-counts`): O(n) where n is the
   number of instrumented expressions. This is called twice per test.
2. **Coverage diffing**: O(n) hash operations.
3. **Interestingness checking**: O(n) set operations.

For small target modules (< 100 expressions), the overhead is negligible.
For larger modules, the per-test overhead may become significant and
could benefit from filtering to only track coverage in the target file.

## Known Limitations

1. **Global errortrace state**: Coverage accumulates across all tests in
   a process. This doesn't affect correctness but can reduce the
   effectiveness of interestingness criteria in long-running sessions.

2. **No cross-process persistence**: The corpus lives in memory. Saving
   and loading corpus entries for long-running fuzzing campaigns is not
   yet implemented.

3. **No struct mutation**: Arbitrary struct values cannot be mutated
   generically. Only built-in types (integers, strings, lists, etc.)
   have mutation strategies.

4. **Single-file targeting**: The instrumentation is most effective
   when focused on a single target module. Multi-module coverage
   tracking works but increases overhead.

5. **Mutation may produce out-of-range values**: Mutating an integer
   from `gen:integer-in` may produce values outside the original range.
   The property function should handle this gracefully (or the mutation
   should be constrained — future work).
