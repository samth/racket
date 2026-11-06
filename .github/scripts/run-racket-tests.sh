#!/usr/bin/env bash

set -euo pipefail
HERE="$(dirname "$0")"
RACKET="racket"
RACO="raco"

MODE=all
SHARD=all
while (($#)); do
    case "$1" in
        --install-only) MODE=install ;;
        --skip-install) MODE=test ;;
        --shard)
            shift
            SHARD="${1:?--shard requires all, core, or other}"
            ;;
        *) echo "Unknown test option: $1" >&2; exit 2 ;;
    esac
    shift
done
case "$SHARD" in all|core|other) ;; *) echo "Unknown shard: $SHARD" >&2; exit 2 ;; esac
phase_start() { printf '\n== PHASE %s %s ==\n' "$1" "$(date -u +%Y-%m-%dT%H:%M:%SZ)"; }

echo Using `which "$RACKET"`
"$RACKET" -v

CPUS="$("$RACKET" -e '(processor-count)')"

if [[ "$MODE" != test ]]; then
    phase_start package-install-and-compile-start
    echo Installing db-test and racket-test using `which "$RACO"`
    "$RACO" pkg install --auto --skip-installed db-test racket-test
    phase_start package-install-and-compile-end
fi
if [[ "$MODE" == install ]]; then exit 0; fi
phase_start "tests-$SHARD-start"

do_test/notimeout() {
    "$RACO" test -j "$CPUS" "$@"
}

do_test() {
    do_test/notimeout --timeout 300 "$@"
}


# What Gets Tested
# ~~~~~~~~~~~~~~~~
# These tests run in GitHub Actions, where the environment isn't suited
# to run all the tests. This script tries to run most of the tests that
# can easily run in that environment. The rest of the tests are run by
# DrDr[1] whenever changes are made to the master branch.
#
# [1]: http://drdr.racket-lang.org/

# Core Tests.
# ~~~~~~~~~~~~~~~~
# The core test suite of Racket itself.

if [[ "$SHARD" != other ]]; then
    printf '\n\n\n\n%s\n\n' "== Testing core tests, 'tests/racket/test' =="
    do_test/notimeout -l "tests/racket/test"
fi
if [[ "$SHARD" == core ]]; then
    phase_start "tests-$SHARD-end"
    exit 0
fi


# Collection Tests
# ~~~~~~~~~~~~~~~~
# Tests where `raco test` can discover and run all the tests.

COLLECTIONS_TO_TEST=(
  tests/file
  tests/future
  tests/generic
  tests/json
  tests/match
  tests/net
  tests/setup
  tests/stxparse
  tests/syntax
  tests/units
  tests/utils
  tests/xml
)

for collection in "${COLLECTIONS_TO_TEST[@]}"; do
    printf '\n\n\n\n%s\n\n' "== Testing collection '$collection' =="
    do_test -c "$collection"
done


# Module Tests
# ~~~~~~~~~~~~
# Tests where a central module controls what gets tested.

MODULES_TO_TEST=(
  tests/db/all-tests
  tests/openssl/basic
  tests/openssl/https
  tests/zo-path
)

for mpath in "${MODULES_TO_TEST[@]}"; do
    printf '\n\n\n\n%s\n\n' "== Testing module path '$mpath' =="
    do_test -l "$mpath"
done


# Special Cases
# ~~~~~~~~~~~~~
# Tests that don't fit in the previous two buckets.

printf '\n\n\n\n%s\n\n' "== Testing 'tests/racket/contract/all' =="

"$RACKET" -l tests/racket/contract/all

phase_start "tests-$SHARD-end"
