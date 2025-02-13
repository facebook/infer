# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import asyncio


def fp_int_key_access_ok():
    d = {"ABC": 1, 2: asyncio.sleep(1)}
    return d[2]


def fp_str_key_access_ok():
    d = {"ABC": 1, "DEF": asyncio.sleep(1)}
    return d["DEF"]
