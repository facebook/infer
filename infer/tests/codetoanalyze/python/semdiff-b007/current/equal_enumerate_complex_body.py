# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


def f(l):
    total = 0
    for x in l:
        y = g(x)
        if y > 0:
            total = total + y
            print(x, y)
        else:
            h(x)
    return total
