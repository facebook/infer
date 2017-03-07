#!/bin/bash

# Copyright (c) 2017 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

# create a release of the Infer Java annotations to send to oss.sonatype.org

set -x
set -e

REMOTE="$( git remote get-url origin )"
OSS_REMOTE=https://github.com/facebook/infer.git

# check if we're in the open-source repo
if ! git remote get-url origin | grep -q "\bgithub\.com\b"; then
    echo "Please run this script from the Git repo for open-source Infer!"
    exit 1
fi

echo "Starting release..."

mvn release:clean release:prepare
mvn release:perform -DpushChanges=false
