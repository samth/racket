# Linklet Planning API

## Context and Motivation

The existing `compile` pipeline immediately feeds generated `(linklet …)` S-expressions (or correlated linklets) into `compile-linklet`, returning opaque `compiled-in-memory` structures. Tools such as the `extract/` utilities need these raw linklet forms and metadata, but today they rely on bootstrap-mode heuristics like `bootstrap:linklet-as-s-expr?`, which fail when a different backend is active. A first-class planning API lets tooling inspect or replace the `compile-linklet` stage without disturbing the core compilation flow.

## Goals

- Expose a stable API that accepts unexpanded syntax or data, performs expansion, and returns:
  - the sequence of linklet S-expressions (or correlated linklets) that `compile-linklet` would receive;
  - the associated metadata (phase, role, import keys, callbacks, options) necessary to compile or analyze those linklets;
  - a finalizer capable of rebuilding the exact `compiled-in-memory` result after clients have compiled (or transformed) the linklets.
- Guarantee zero behavioural regression: invoking the new API and immediately finalizing with `compile-linklet` must match the current `compile` output.
- Surface extractor-friendly metadata so the `extract/` tree no longer depends on `compiled-in-memory` introspection or bootstrap-only paths.

## Non-Goals

- Altering how linklets are generated or optimized.
- Introducing new backends; the intent is to surface hooks only.
- Replacing evaluator interfaces like `eval` or `compile-single`.

## Current Behaviour Snapshot

- `eval/main.rkt:73-128` expands raw inputs via `per-top-level`, sending parsed expressions to `compile-top` or `compile-module`.
- `compile/top.rkt`, `compile/form.rkt`, and `compile/module.rkt` assemble `(linklet …)` bodies and immediately call `compile-linklet`, passing context through `info` hashes, import-key vectors, `get-import` callbacks, and flag lists.
- Module bundles, syntax literals, and multi-top metadata are stored inside `compiled-in-memory` results that downstream code consumes.

## High-Level Design

1. **Emitter Abstraction**  
   Thread a new keyword argument `#:emit-linklet` through the compilation stack. Its default value wraps `compile-linklet` so call sites continue to behave exactly as before but without branchy conditionals in hot paths.

2. **Planning API**  
   Provide `compile->linklet-plan`, `plan-compile-top`, and `plan-compile-module` in a new `compile/plan.rkt`. These functions run the existing expansion and compilation pipeline using a recorder emitter.

3. **Data Structures**  
   - `linklet-input` prefab struct
     - `id`: symbolic tag (e.g. `'link`, `'decl`, `(cons 'body phase)`, `(list 'submodule sub-name phase)`).
     - `phase`: integer phase or `#f`.
     - `role`: symbol such as `'body`, `'link`, `'syntax-literals`, `'data`, `'transformer`.
     - `expr`: `(linklet …)` S-expression or correlated linklet.
     - `info`: hash passed to `compile-linklet`.
     - `import-keys`: vector passed to `compile-linklet`.
     - `get-import`: callback used when cross-module optimization fetches more linklets.
     - `options`: list of flags (`'serializable`, `'unsafe`, `'use-prompt`, `'quick`, `'unlimited-compile`, etc.).
     - `imports`: list of dependency `link` records (module plus phase).
     - `exports`: list of exported symbols or pairs (for renamings).
     - `side-effects?`: boolean for phase-level side-effect tracking.
   - `linklet-plan` struct
     - `inputs`: ordered list of `linklet-input` entries.
     - `metadata`: module-level context such as `phase-to-link-module-uses`, syntax literal tables, portal syntax offsets, purity flags, submodule descriptors, and module identity.
     - `finalize`: procedure that rebuilds the compiled artifact.

4. **Finalizer Contract**  
   `finalize` accepts a mapping from each `id` to the result of compiling that linklet (plus any revised import-key vector). It replays bundle assembly exactly as the current code, yielding the same `compiled-in-memory` (or correlated version). A helper `apply-linklet-plan` runs each `linklet-input` through a backend—defaulting to `compile-linklet`—and feeds the results to `finalize`.

5. **Extractor Integration**  
   - Use `plan-compile-module` in `extract/get-linklet.rkt` to cache plans and finalizers instead of compiled bundles.
   - Build `linklet-info` data directly from `linklet-input` fields, eliminating reliance on `bootstrap:linklet-as-s-expr?`.
   - Preserve compilation order via the plan’s `inputs` list.
  - Replace flattening and simplification routines to read `linklet-input-expr`.
   - Keep a bridge that invokes the finalizer when bytecode or size measurements are required.

6. **Backward Compatibility**  
   Rewrite `compile` as:
   ```racket
   (define (compile s ns serializable? expand)
     (define-values (plan finalize)
       (compile->linklet-plan s ns #:serializable? serializable? #:expand expand))
     (define compiled (apply-linklet-plan plan))
     (finalize compiled))
   ```
   Because `apply-linklet-plan` uses the default emitter, behaviour remains unchanged.

## Implementation Details

- **Emitter Threading**  
  Update all `compile-linklet` call sites (notably `compile/top.rkt`, `compile/form.rkt`, `compile/module.rkt`, `compile/multi-top-data.rkt`) to accept `#:emit-linklet`, replacing direct calls with `(emit-linklet id expr info import-keys get-import options)` and forwarding multiple return values via `call-with-values`.

- **Plan Recorder**  
  The planner’s emitter creates `linklet-input` records, stores them, and returns placeholders (typically `(values expr import-keys)`) so upstream logic continues to function. Module-level metadata—syntax literal vectors, portal syntax data, module path indexes, purity flags, submodule lists—is captured in `linklet-plan.metadata`.

- **Finalizer**  
  Consumes compiled linklets keyed by `id`, substitutes them into the stored structures, and invokes the same `compiled-in-memory` constructors as today.

- **Extractor Changes**  
  Cache `linklet-plan`/`finalize` pairs, generate `linklet-info` from the plan, adjust flattening and GC passes to operate on recorded expressions, and provide utilities to run `finalize` when needed.

## Testing Strategy

- Unit tests that compare `finalize` composed with `apply-linklet-plan` against the legacy `compile` across:
  - single expressions and modules,
  - multiple phases,
  - serializable vs non-serializable,
  - unsafe and unlimited-compile flags,
  - correlated linklet mode.
- Extractor regression tests ensuring flattened linklets and reports match pre-change outputs for representative modules (standard library pieces, cross-phase exporters, syntax-heavy modules).
- Cross-module optimization tests verifying `make-module-use-to-linklet` callbacks receive identical keys and produce equivalent linklets.
- Host coverage on Racket CS and BC to ensure backend neutrality.
- Performance benchmarks showing no significant regression (default emitter adds no conditional overhead).

## Migration Plan

1. Introduce `#:emit-linklet` plumbing with a default emitter that wraps `compile-linklet`.
2. Implement planner structs (`linklet-input`, `linklet-plan`), recorder emitter, and finalizer; add internal invariance tests.
3. Refactor `compile` to call the planner and finalizer while preserving observable behaviour.
4. Transition the extractor:
   - Stage 1: support both `compiled-in-memory` and plan outputs under a feature flag.
   - Stage 2: remove the legacy path after parity on real workloads.
5. Document the new API in expander documentation, including examples for tooling authors.
6. Monitor CI and extractor workloads for regressions.

## Future Extensions

- Allow alternative backends to plug into `apply-linklet-plan` (code generators, analyzers).
- Provide plan metadata to other tools (profilers, static analyzers) without re-expansion.
- Support serialized caching of `linklet-plan` inputs for incremental builds before invoking the backend.

