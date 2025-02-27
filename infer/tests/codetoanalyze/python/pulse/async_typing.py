# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import asyncio
import types


async def await_condition_typing_int_ok():
    x = int(5)
    if type(x) == int:
        await asyncio.sleep(1)
    else:
        asyncio.sleep(1)


async def await_condition_typing_int_bad():
    if type(5) == str:
        await asyncio.sleep(1)
    else:
        asyncio.sleep(1)


async def await_condition_typing_str_ok():
    x = str("abc")
    if type(x) == str and type(x) is str:
        await asyncio.sleep(1)
    else:
        asyncio.sleep(1)


async def await_condition_typing_with_arg_bad(x):
    if type(x) == str:
        await asyncio.sleep(1)
    else:
        asyncio.sleep(1)


class Name:
    def __init__(self, name):
        self.name = name


async def fp_await_condition_typing_user_defined_ok():
    x = Name("foo")
    if type(x) == Name:
        await asyncio.sleep(1)
    else:
        asyncio.sleep(1)


async def fp_await_condition_typing_fun_ok():
    def x():
        return -1

    if type(x) == types.FunctionType:
        await asyncio.sleep(1)
    else:
        asyncio.sleep(1)


async def await_condition_is_none_bad():
    x = None
    if x is None:
        asyncio.sleep(1)
    else:
        await asyncio.sleep(1)


async def await_condition_is_none_ok():
    x = None
    if x is None:
        await asyncio.sleep(1)
    else:
        asyncio.sleep(1)


async def await_condition_bool_none_bad():
    x = None
    if x:
        await asyncio.sleep(1)
    else:
        asyncio.sleep(1)


async def await_condition_bool_none_ok():
    x = None
    if x:
        asyncio.sleep(1)
    else:
        await asyncio.sleep(1)


async def fp_await_condition_val_equal_ok():
    x = Name("foo")
    y = Name("foo")
    if x == y:
        await asyncio.sleep(1)
    else:
        asyncio.sleep(1)


async def await_condition_phys_equal_bad():
    x = Name("foo")
    y = Name("foo")
    if x is y:
        await asyncio.sleep(1)
    else:
        asyncio.sleep(1)
