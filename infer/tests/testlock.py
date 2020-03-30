#!/usr/bin/env python3

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os
import sys
import fcntl
import subprocess

TESTS_DIR = os.path.dirname(os.path.realpath(__file__))
LOCKFILE = os.path.join(TESTS_DIR, 'testlock.mutex')

args = sys.argv[1:]

with open(LOCKFILE, 'r') as lockfile:
    fd = lockfile.fileno()

    fcntl.flock(fd, fcntl.LOCK_EX)
    try:
        subprocess.call(args)
    finally:
        fcntl.flock(fd, fcntl.LOCK_UN)
