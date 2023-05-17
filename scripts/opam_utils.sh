#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# source this script to import its utility functions

opam_retry () {
  "$@" || {
    echo >&2;
    printf '*** `%s` failed\n' "$*" >&2;
    echo '*** Updating opam then retrying' >&2;
    opam update &&
    "$@" || {
      echo >&2;
      printf '*** ERROR: `%s` failed\n' "$*" >&2;
      exit 1
    }
  }
}

opam_failed () {
    local command=$1
    echo
    printf '*** ERROR: %s failed\n' "$command" >&2
    printf "*** ERROR: Try running \$(opam update) then running this script again\n" >&2
    exit 1
}

opam_require_version_2 () {
    local status=0
    local version=0
    { version=$(opam --version 2>/dev/null); status=$?; }
    if [ "$status" != 0 ]; then
        printf '*** ERROR: `opam --version` failed, please install opam version 2\n' >&2
        env >&2
        exit 1
    fi
    case $version in
        2*) ;;
        *)
            printf '*** ERROR: opam version "%s" is not supported, please install opam version 2\n' "$version" >&2
            printf '*** NOTE: opam is "%s"\n' "$(which opam)" >&2
            env >&2
            exit 1
    esac
}

# assumes opam is available and initialized
opam_switch_create_if_needed () {
    local switch=$1
    local options=$2
    local switch_exists=no
    printf "looking if switch %s exists in this list:\n" "$switch";
    opam switch list --short
    for installed_switch in $(opam switch list --short); do
        if [ "$installed_switch" == "$switch" ]; then
            switch_exists=yes
            break
        fi
    done
    printf "verdict: %s\n" $switch_exists;
    if [ "$switch_exists" = "no" ]; then
        if [ -e "$(opam var root)/$switch" ] ; then
            rm -rf "$(opam var root)/$switch" || true
        fi
        opam switch create "$switch" $options
    fi
}

opam_require_version_2

# removes packages that cannot be found by ocamlfind
opam_remove_broken_package () {
    local pkg="$1"
    if ! ocamlfind query "${pkg}"; then
        echo "ocamlfind cannot find ${pkg} package. Removing the package..."
        opam remove "${pkg}" || true
    fi
}
