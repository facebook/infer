# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


def f(cfg=None):
    if cfg is None:
        cfg = {"mode": "bilinear"}
    return len(cfg)
