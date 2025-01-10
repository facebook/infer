# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import asyncio


async def fp_await_condition_typing_ok():
    if type(5) == int:
        await asyncio.sleep(1)
    else:
        asyncio.sleep(1)


async def await_condition_typing_bad():
    if type(5) == str:
        await asyncio.sleep(1)
    else:
        asyncio.sleep(1)


async def await_condition_typing_with_arg_bad(x):
    if type(x) == str:
        await asyncio.sleep(1)
    else:
        asyncio.sleep(1)
