# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import level1
from abc import ABC, abstractmethod

class A:
    def getTainted(self) -> int:
        return level1.taintSource()

    def getUntainted(self):
        return 1

class C(ABC):
    a: A

    # TODO(vsiles) support static class member
    # public static A $globalvar;

    @abstractmethod
    def get(self) -> A:
        pass


def fromParamsBad(a: A) -> None:
    level1.taintSink(a.getTainted())

def fromParamsGood(a: A) -> None:
    level1.taintSink(a.getUntainted())

def fromPropertyThroughParamBad(c: C) -> None:
    level1.taintSink(c.a.getTainted())

def fromPropertyThroughParamGood(c: C) -> None:
    level1.taintSink(c.a.getUntainted())

def fromPropertyThroughNewBad() -> None:
    c = C()
    level1.taintSink(c.a.getTainted())

def fromPropertyThroughNewGood() -> None:
    c = C()
    level1.taintSink(c.a.getUntainted())

# def fromGlobalBad(c: C) -> None:
#     level1.taintSink(C::$globalvar.getTainted())

# def fromGlobalGood(c: C) -> None:
#     level1.taintSink(C::$globalvar.getUntainted())

def FN_fromCallResultBad(c: C) -> None:
    level1.taintSink(c.get().getTainted())

def fromCallResultGood(c: C) -> None:
    level1.taintSink(c.get().getUntainted())
