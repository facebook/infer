# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from setup import *
import glob
import os

jar_paths = glob.glob(JAR_PATH + "/*")

print()
print("*** Program stats ***")

for jar in jar_paths:
    out = os.popen("./" + SCAN_JAR_PATH + " " + jar).read().strip()
    print(out)
