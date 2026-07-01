# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


def f(cfg=None):
    if cfg is None:
        cfg = {"type": "Xavier", "layer": "Conv2d"}
    return len(cfg)
