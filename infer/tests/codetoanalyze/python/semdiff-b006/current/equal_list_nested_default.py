# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


def f(x=None):
    if x is None:
        x = [[1, 2], [2, 6], [3, 12]]
    return len(x)
