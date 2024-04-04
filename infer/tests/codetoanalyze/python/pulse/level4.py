# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

class C20:
    @staticmethod
    def taintSink(arg: int) -> None:
        pass

    @staticmethod
    def taintSource() -> int:
        return 451

class C21:
    def transform(self) -> int:
        return 42

class C22(C21):
    def transform(self) -> int :
        return C20.taintSource()

class C23:
    @staticmethod
    def getC21() -> C21:
        return C21()

    @staticmethod
    def getC22AsC21() -> C21:
        return C22()

    def transformGood() -> None:
        C20.taintSink(C23.getC21().transform())

    def transformBad() -> None:
        C20.taintSink(C23.getC22AsC21().transform())
