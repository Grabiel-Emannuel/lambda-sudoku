#!/usr/bin/env bash
set -euo pipefail

usage() {
  echo "Usage: $0 DEPTH SOLUTIONS THREADS INPUT.csv" >&2
}

is_positive_integer() {
  [[ $1 =~ ^[0-9]+$ ]] && (( 10#$1 > 0 ))
}

if [[ $# -ne 4 ]]; then
  usage
  exit 2
fi

depth=$1
solutions=$2
threads=$3
input=$4

if ! is_positive_integer "$depth"; then
  echo "DEPTH must be a positive integer: $depth" >&2
  usage
  exit 2
fi

if ! is_positive_integer "$solutions"; then
  echo "SOLUTIONS must be a positive integer: $solutions" >&2
  usage
  exit 2
fi

if ! is_positive_integer "$threads"; then
  echo "THREADS must be a positive integer: $threads" >&2
  usage
  exit 2
fi

if [[ ! -f $input || ! -r $input ]]; then
  echo "Input file does not exist or is not readable: $input" >&2
  exit 2
fi

input_dir=$(cd -- "$(dirname -- "$input")" && pwd -P)
input_name=$(basename -- "$input")

exec podman run --rm \
  -v "$input_dir:/input:ro,z" \
  -e "DEPTH=$depth" \
  -e "SOLUTIONS=$solutions" \
  -e "THREADS=$threads" \
  lambda-sudoku --path "/input/$input_name"
