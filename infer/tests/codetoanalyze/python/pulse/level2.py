# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

class C1:
    @staticmethod
    def taintSource() -> str:
        return "tainted"

class C2:
    @staticmethod
    def taintSink(arg: str) -> None:
        pass

class C3:
    @staticmethod
    def transformer(flag: bool) -> str:
        if flag:
            return C1.taintSource();
        else:
            return "untainted"


C2.taintSink(C3.transformer(True))  # Taint detected
C2.taintSink(C3.transformer(False)) # untainted
