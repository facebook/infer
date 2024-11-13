# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import level1
from level1 import taint_sink

def basic_flow_bad() -> None:
    tainted = level1.taint_source()
    taint_sink(tainted)

def basic_flow_ok(untainted: int) -> None:
    level1.taint_sink(untainted)

basic_flow_bad()
basic_flow_ok(0)
