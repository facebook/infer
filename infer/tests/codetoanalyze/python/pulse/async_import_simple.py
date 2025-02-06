# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import asyncio
import async_utils as utils
from async_utils import (
    await_it as async_await,
    dont_await_it as async_dont_await,
    sleep,
    C
)
from not_captured import unknown

async def with_import_bad():
    await utils.dont_await_it(sleep())


async def with_import_ok():
    await utils.await_it(sleep())


async def with_from_import_bad():
    await async_dont_await(sleep())


async def with_from_import_ok():
    await async_await(sleep())


async def with_imported_class_bad():
    await async_dont_await(C.sleep())


async def with_imported_class_ok():
    await async_await(C.sleep())


async def asyncio_gather_3_elements_ok():
    await asyncio.gather(
        *[
            sleep(),
            sleep(),
            sleep(),
        ]
    )


async def asyncio_gather_unknown_list_ok(l):
    tasks = [asyncio.sleep(i) for i in l]
    await asyncio.gather(*tasks)


async def make_awaitables_dict(cond):
    awaitables_dict = {}
    if cond:
        awaitables_dict["a key"] = sleep()
    return awaitables_dict


def unknown_call():
    unknown(asyncio.sleep(1))
