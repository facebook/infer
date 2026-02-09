# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Example where an assertion is added (semantic change)
def foo(self) -> None:
    obj = self.obj
    assert obj is not None
    x = obj.prop
