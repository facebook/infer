# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import taint
from taint import sink

def basic_flow_bad() -> None:
    tainted = taint.source()
    sink(tainted)

def basic_flow_ok(untainted: int) -> None:
    sink(untainted)

basic_flow_bad()
basic_flow_ok(0)
