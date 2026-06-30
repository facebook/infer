# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

def f(d):
    out = []
    for k, v in d.items():
        for e in k:
            out.append(e)
    return out
