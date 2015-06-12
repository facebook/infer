#!/bin/bash
set -x
set -e

# This script fetches the revision of the 'facebook-clang-plugins'
# required by Infer.

INFER_ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PLUGIN_REPO=https://github.com/facebook/facebook-clang-plugins
VERSION_FILE="$INFER_ROOT/dependencies/clang-plugin/clang-plugin-version.config"


# Since we need to use autotools, ensure that we don't do it
# in a path that contains a space.  The path will be cached here
PLUGIN_POINTER_FILE="$INFER_ROOT/.facebook-clang-plugin-dir"
if [ ! -e "$PLUGIN_POINTER_FILE" ]; then
    mktemp -d /tmp/facebook-clang-plugin-setup.XXXXXX > "$PLUGIN_POINTER_FILE"
fi

PLUGIN_SETUP_DIR="$( cat "$PLUGIN_POINTER_FILE" )"
if [ ! -d "$PLUGIN_SETUP_DIR" ]; then
    mkdir -p "$PLUGIN_SETUP_DIR"
fi

PLUGIN_DIR="$PLUGIN_SETUP_DIR/facebook-clang-plugin"

# check if the repo is already in place
if [ ! -e "$PLUGIN_DIR" ]; then
    echo "$PLUGIN_DIR not found, cloning..."
    git $GIT_OPTIONS clone $PLUGIN_REPO "$PLUGIN_DIR"
fi

# update revision if needed
echo "Checking out the right version of the clang plugin..."
pushd "$PLUGIN_DIR"
git checkout master
git $GIT_OPTIONS pull
git checkout $(cat "$VERSION_FILE")
popd
