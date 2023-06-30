# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

class C10:
    def taintSource(self) -> int:
        return 451

    def regularSource(self) -> int:
        return 42

class C11:
    def taintSink(self, arg: int) -> None:
        pass

def flow_good(c1: C10, c2: C11) -> None:
    c2.taintSink(c1.regularSource())

def flow_bad(c1: C10, c2: C11) -> None:
    c2.taintSink(c1.taintSource())
