# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

class C:
    def __init__(self, name: str | None, ip: str | None) -> None:
        self.name = name
        self.ip = ip


def foo(i: int) -> str | None:
    if i==0:
        return None
    else:
        return f"i={i}"
