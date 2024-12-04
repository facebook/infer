# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import random
import taint


def basic_flow_bad() -> None:
    tainted = taint.source()
    taint.sink(tainted)


def basic_flow_ok(untainted: int) -> None:
    taint.sink(untainted)


def fst(x: int, y: int) -> int:
    return x


def call_fst_bad(untainted) -> None:
    arg = fst(taint.source(), untainted)
    taint.sink(arg)


def call_fst_ok(untainted) -> None:
    arg = fst(untainted, taint.source())
    taint.sink(arg)


def sink_fst_arg(x, y) -> None:
    taint.sink(x)


def call_sink_fst_arg_bad(untainted) -> None:
    sink_fst_arg(taint.source(), untainted)


def call_sink_fst_arg_ok(untainted) -> None:
    sink_fst_arg(untainted, taint.source())


def call_taint_sink_on_global_bad1():
    global g
    taint.sink(g)


def call_taint_sink_on_global_ok():
    global g
    taint.sink(g)


def call_taint_sink_on_global_bad2():
    global g
    g = taint.source()
    taint.sink(g)

g = taint.source()
call_taint_sink_on_global_bad1()
g = 0
call_taint_sink_on_global_ok()
call_taint_sink_on_global_bad2()
