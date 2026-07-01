# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


def f(a, *, x=None):
    if x is not None:
        x = []
    return len(x)
