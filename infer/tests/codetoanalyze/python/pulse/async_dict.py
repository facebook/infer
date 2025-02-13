# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import asyncio


def fp_int_key_access_ok():
    d = {"ABC": 1, 2: asyncio.sleep(1)}
    return d[2]


def str_key_access_ok():
    d = {"ABC": 1, "DEF": asyncio.sleep(1)}
    return d["DEF"]


def int_key_access_bad():
    d = {123: 1, 456: asyncio.sleep(1)}
    return d[123]


async def str_key_access_bad():
    d = {"ABC": asyncio.sleep(), 123: 456, "DEF": await asyncio.sleep(1)}
    return d["DEF"]
