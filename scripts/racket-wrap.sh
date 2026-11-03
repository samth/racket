#!/bin/bash
# Stand-in for racket/bin/racket when regenerating schemified files with
# a base build that lacks compiler-lib: turn `-N raco -l- raco make FILE...`
# into compiler/cm calls, and pass everything else to the real racket.
REAL=/tmp/rkt-conc/workerspin/racket/bin/racket
if [ "$1" = "-N" ] && [ "$2" = "raco" ] && [ "$3" = "-l-" ] && [ "$4" = "raco" ] && [ "$5" = "make" ]; then
  shift 5
  for f in "$@"; do
    case "$f" in
      -*) ;;
      *) "$REAL" -l racket/base -l compiler/cm -e "(managed-compile-zo \"$f\")" || exit 1 ;;
    esac
  done
  exit 0
fi
exec "$REAL" "$@"
