# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


def taintSource() ->int:
    return 42

def taintSink(args) -> None:
    pass

def basicFlowBad() -> None:
    tainted = taintSource()
    taintSink(tainted)

def basicFlowOk(untainted: int) -> None:
    taintSink(untainted)
