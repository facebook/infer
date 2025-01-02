# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import asyncio
from dir1.testmod import (
    await_it as await_it1,
    dont_await_it as dont_await_it1,
)
from dir1.dir3.testmod import (
    await_it as await_it3,
    dont_await_it as dont_await_it3,
    C
)
import dir1.dir4.testmod
from dir2.testmod import (
    await_it as await_it2,
    dont_await_it as dont_await_it2,
)
import dir2.dir5.testmod as import5
from dir2.dir6 import testmod as import6


async def bad1():
    await dont_await_it1(asyncio.sleep(1))


async def ok1():
    await await_it1(asyncio.sleep(1))


async def bad2():
    await dont_await_it2(asyncio.sleep(1))


async def ok2():
    await await_it2(asyncio.sleep(1))


async def bad3():
    await dont_await_it3(asyncio.sleep(1))


async def ok3():
    await await_it3(asyncio.sleep(1))


async def from_class_bad3():
    await dont_await_it3(C.async_fun())


async def from_class_ok3():
    await await_it3(C.async_fun())


async def bad4():
    await dir1.dir4.testmod.dont_await_it(asyncio.sleep(1))
# we still need this explicit toplevel call until we adapt specialization
# types to import-packages without alias names but this is not a frequent
# pattern in our experiments so far
asyncio.run(bad4())

async def ok4():
    await dir1.dir4.testmod.await_it(asyncio.sleep(1))
asyncio.run(ok4())


async def bad5():
    await import5.dont_await_it(asyncio.sleep(1))


async def ok5():
    await import5.await_it(asyncio.sleep(1))


async def bad6():
    await import6.dont_await_it(asyncio.sleep(1))


async def ok6():
    await import6.await_it(asyncio.sleep(1))
