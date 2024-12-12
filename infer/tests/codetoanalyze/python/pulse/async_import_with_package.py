# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import asyncio
from dir1.testmod import await_it as await_it1, dont_await_it as dont_await_it1
from dir1.dir3.testmod import await_it as await_it3, dont_await_it as dont_await_it3
from dir1.dir4.testmod import await_it as await_it4, dont_await_it as dont_await_it4
from dir2.testmod import await_it as await_it2, dont_await_it as dont_await_it2
from dir2.dir5.testmod import await_it as await_it5, dont_await_it as dont_await_it5
from dir2.dir6.testmod import await_it as await_it6, dont_await_it as dont_await_it6


async def bad1():
    await dont_await_it1(asyncio.sleep(1))
asyncio.run(bad1())


async def ok1():
    await await_it1(asyncio.sleep(1))
asyncio.run(ok1())


async def bad2():
    await dont_await_it2(asyncio.sleep(1))
asyncio.run(bad2())


async def ok2():
    await await_it2(asyncio.sleep(1))
asyncio.run(ok2())


async def bad3():
    await dont_await_it3(asyncio.sleep(1))
asyncio.run(bad3())


async def ok3():
    await await_it3(asyncio.sleep(1))
asyncio.run(ok3())


async def bad4():
    await dont_await_it4(asyncio.sleep(1))
asyncio.run(bad4())


async def ok4():
    await await_it4(asyncio.sleep(1))
asyncio.run(ok4())


async def bad5():
    await dont_await_it5(asyncio.sleep(1))
asyncio.run(bad5())


async def ok5():
    await await_it5(asyncio.sleep(1))
asyncio.run(ok5())


async def bad6():
    await dont_await_it6(asyncio.sleep(1))
asyncio.run(bad6())


async def ok6():
    await await_it6(asyncio.sleep(1))
asyncio.run(ok6())
