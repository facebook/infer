#!/bin/bash

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# create a release of the Infer Java annotations to send to oss.sonatype.org

set -x
set -e

# This fixes 'gpg: signing failed: Inappropriate ioctl for device' error
# during signing
export GPG_TTY=$(tty)

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
ROOTDIR="$( cd $DIR/.. && pwd)"

# check if we're in the open-source repo
if ! git remote get-url origin | grep -q "\bgithub\.com\b"; then
    echo "Please run this script from the Git repo for open-source Infer!"
    exit 1
fi

echo "Starting release..."

( cd "$ROOTDIR/infer/annotations" && \
      mvn -e release:clean release:prepare && \
      mvn -e release:perform -DpushChanges=false )
