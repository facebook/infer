#!/bin/bash

# Copyright (c) 2015 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

set -x
set -e

# This script fetches the revision of the 'facebook-clang-plugins'
# required by Infer.

INFER_ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PLUGIN_REPO=https://github.com/facebook/facebook-clang-plugins
PLUGIN_DIR="$INFER_ROOT/../facebook-clang-plugin"
VERSION_FILE="$INFER_ROOT/dependencies/clang-plugin/clang-plugin-version.config"

# check if the repo is already in place
if [ ! -e "$PLUGIN_DIR" ]; then
    echo "$PLUGIN_DIR not found, cloning..."
    git $GIT_OPTIONS clone $PLUGIN_REPO "$PLUGIN_DIR"
fi

# update revision if needed
echo "Checking out the right version of the clang plugin..."
pushd $PLUGIN_DIR
git checkout master
git $GIT_OPTIONS pull
git checkout $(cat "$VERSION_FILE")
popd
