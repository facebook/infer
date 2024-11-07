# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


def taint_source() ->int:
    return 42

def taint_sink(args) -> None:
    pass

def basic_flow_bad() -> None:
    tainted = taint_source()
    taint_sink(tainted)

def basic_flow_ok(untainted: int) -> None:
    taint_sink(untainted)

def fst(x: int, y: int) -> int:
    return x

def call_fst_bad(untainted) -> None:
    arg = fst(taint_source(), untainted)
    taint_sink(arg)

def call_fst_ok(untainted) -> None:
    arg = fst(untainted, taint_source())
    taint_sink(arg)

def sink_fst_arg(x, y) -> None:
    taint_sink(x)

def call_sink_fst_arg_bad(untainted) -> None:
    sink_fst_arg(taint_source(), untainted)

def call_sink_fst_arg_ok(untainted) -> None:
    sink_fst_arg(untainted, taint_source())

def call_taint_sink_on_global_bad1():
    global g
    taint_sink(g)

def call_taint_sink_on_global_ok():
    global g
    taint_sink(g)

def call_taint_sink_on_global_bad2():
    global g
    g = taint_source()
    taint_sink(g)

# we need to call these functions in order to activate specialization
basic_flow_bad()
basic_flow_ok(0)
call_fst_bad(0)
call_fst_ok(0)
call_sink_fst_arg_bad(0)
call_sink_fst_arg_ok(0)
g = taint_source()
call_taint_sink_on_global_bad1()
g = 0
call_taint_sink_on_global_ok()
call_taint_sink_on_global_bad2()
