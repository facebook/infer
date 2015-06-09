#!/bin/bash
#
# Copyright (c) 2014 - Facebook.  All rights reserved.
#

set -e

parent=$(dirname "$0")
SCRIPT_DIR=$( cd "$parent" && pwd )
README_MSG="To get and compile facebook-clang-plugins, please consult the README file."

function echoerr {
    echo $@ >&2
}

# Counting the passed arguments
if [ "$#" -lt 1 ]; then
    echoerr "Error: not enough arguments."
    echoerr "USAGE: $0 <facebook-clang-plugins root>"
    echoerr "$README_MSG"
    exit 1
fi

REPO_DIR=$1

# Check that the directory passed in input exists
([ -d "$REPO_DIR" ] && [ -d "$REPO_DIR/libtooling" ]) || {
    echoerr "$REPO_DIR does not exist or is invalid.";
    echoerr "Please check that the path to facebook-clang-plugins is correct.";
    echoerr "$README_MSG";
    exit 1;
}

VERSION_FILE="$SCRIPT_DIR/../dependencies/clang-plugin/clang-plugin-version.config"

cd $REPO_DIR
[ ! -d ".git" ] && {
    echoerr "SKIPPING the facebook-clang-plugins version check since";
    echoerr "$REPO_DIR is NOT a Git repository";
    exit 0;
}

echoerr "Checking that the revision of facebook-clang-plugins is correct..."
GIT_CURRENT_REVISION=$(git log --pretty=format:'%H' -n 1)
GIT_EXPECTED_REVISION=$(cat "$VERSION_FILE")

echoerr "Current revision is $GIT_CURRENT_REVISION"
echoerr "Expected revision is $GIT_EXPECTED_REVISION"

if [ "$GIT_CURRENT_REVISION" != "$GIT_EXPECTED_REVISION" ]; then
    echoerr "Revisions mismatching. Please, run update-fcp.sh to get the revision needed by Infer."
    exit 1
else
    echoerr "Clang plugin is up to date! Continue..."
fi
