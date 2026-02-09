# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
from typing import Optional

class C:
    def __init__(self, name: Optional[str], ip: Optional[str]) -> None:
        self.name = name
        self.ip = ip


def foo(i: int) -> Optional[str]:
    if i==0:
        return None
    else:
        return f"i={i}"
