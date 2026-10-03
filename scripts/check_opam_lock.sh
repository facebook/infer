#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Check that the current opam switch has exactly the package versions recorded in the given opam
# lock files (default: opam/infer.opam.locked), e.g. after `make devsetup`, whose developer tools
# must not up- or downgrade the locked dependencies of infer. Packages that are only locked for
# tests ({= "..." & with-test}) are not checked.

set -e
set -o pipefail

OPAM=${OPAM:-opam}
if [ $# -eq 0 ]; then
  set -- "$(dirname "$0")"/../opam/infer.opam.locked
fi

installed=$(mktemp)
trap 'rm -f "$installed"' EXIT
"$OPAM" list --installed --short --columns=name,version --color=never > "$installed"
if [ ! -s "$installed" ]; then
  echo "*** \`$OPAM list --installed\` returned no packages" >&2
  exit 1
fi

awk '
  FNR == NR { installed[$1] = $2; next }
  /^  "[^"]+" [{]= "[^"]+"/ && !/with-test/ {
    split($0, fields, "\"")
    name = fields[2]; locked = fields[4]
    checked++
    if (!(name in installed)) {
      printf "%s: not installed, locked at %s in %s\n", name, locked, FILENAME
      mismatches++
    } else if (installed[name] != locked) {
      printf "%s: %s installed, locked at %s in %s\n", name, installed[name], locked, FILENAME
      mismatches++
    }
  }
  END {
    if (mismatches > 0) {
      printf "*** %d package(s) differ from the opam lock files\n", mismatches
      exit 1
    }
    printf "%d lock entries checked, all match the opam switch\n", checked
  }' "$installed" "$@" >&2
