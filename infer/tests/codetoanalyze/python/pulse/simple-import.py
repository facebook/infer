# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import level1
from level1 import taintSink

def basicFlowBad() -> None:
    tainted = level1.taintSource()
    taintSink(tainted)

def basicFlowOk(untainted: int) -> None:
    level1.taintSink(untainted)
