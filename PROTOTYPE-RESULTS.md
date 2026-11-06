# PR feedback workflow prototype

This independent branch adds a manual `CI feedback prototype` workflow. It does
not change the existing pull_request workflow or require an environment change.
Run it on this branch through workflow_dispatch to measure runner timings.

The macOS runtime build publishes a host artifact in its own job. Native iOS
needs only that job, so it can overlap test-package setup and desktop tests.
Each platform prepares and compiles test packages once, archives the runtime,
source package tree and explicit user package directory, then fans out into
`core` and `other` test jobs. The original all-in-one script remains its default.
Phase markers distinguish package download/compilation from test execution.

A command-capture check confirms the union of the two shards preserves all 18
existing suite invocations, their timeout options, and the unbounded core suite.
Installation runs once in the preparation mode and zero times in the shards.
Validation: `python3 .github/scripts/tests/test-shards.py`, `bash -n` and a YAML
parse/dependency check passed locally. No hosted workflow has been run yet.

This manual workflow measures the desktop/native-iOS experiment. The existing
PR workflow continues to exercise PB iOS independently. The prototype does not
replace that coverage, enable caching, or claim a measured hosted speedup.
Package catalog versions are still selected at setup time, as in current CI.

Artifacts preserve symlinks and modes through tar. Prepared jobs require the
same absolute checkout/user paths on a given platform; this needs verification
on hosted runners. Upload/download overhead, extra runners, artifact sizes and
actual shard balance must be measured. Setup currently waits for both host
platforms; native iOS waits only for macOS. A later reusable-workflow split can
remove the cross-platform setup barrier if it matters in measurements.

The independent `prototype/zuo-action-cache` and
`prototype/reproducible-chez-boots` branches can be combined for another manual
experiment after review. No hosted action cache or remote transport is enabled
by this branch.
