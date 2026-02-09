# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import list

def greet(name: str) -> str:
    return f"Hello, {name}!"

def length(l: list[Any]):
    return len(l)
