#!/bin/sh
set -eu

exec lambda-sudoku "$@" \
  --solutions "$SOLUTIONS" \
  --depth "$DEPTH" \
  +RTS "-N${THREADS}" -s/dev/stdout -RTS
