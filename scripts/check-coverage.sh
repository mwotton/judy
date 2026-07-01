#!/usr/bin/env bash
set -euo pipefail

tix_file=$(find dist-newstyle -path '*/t/tests/hpc/vanilla/tix/tests.tix' -print -quit)
if [[ -z "$tix_file" ]]; then
  echo "Could not find Cabal HPC .tix file" >&2
  exit 1
fi

mapfile -t hpc_dirs < <(find dist-newstyle -path '*/extra-compilation-artifacts/hpc/vanilla/mix' -type d | sort)
if [[ ${#hpc_dirs[@]} -eq 0 ]]; then
  echo "Could not find Cabal HPC .mix directories" >&2
  exit 1
fi

hpc_args=()
for dir in "${hpc_dirs[@]}"; do
  hpc_args+=(--hpcdir="$dir")
done

report=$(hpc report "$tix_file" "${hpc_args[@]}" --include=Data.Judy --srcdir=. --per-module)
echo "$report"

required_markers=(
  "100% expressions used"
  "100% boolean coverage"
  "100% alternatives used"
  "100% local declarations used"
  "100% top-level declarations used"
)

for marker in "${required_markers[@]}"; do
  if ! grep -q "$marker" <<<"$report"; then
    echo "Coverage check failed: missing '$marker'" >&2
    exit 1
  fi
done
