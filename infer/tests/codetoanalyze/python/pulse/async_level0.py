# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import asyncio


async def sleep(i):
    await asyncio.sleep(i)


async def sleep_bad(i):
    asyncio.sleep(i)


async def call_sleep_ok():
    await sleep(1)


async def call_sleep_bad():
    sleep(1)


async def call_sleep_unknown_call_ok():
    unknown(sleep(1))


async def call_sleep_with_temp_ok():
    temp = sleep(1)
    await temp


async def call_sleep_with_branchs_ok(b):
    temp = sleep(1)
    if b:
        await temp
    else:
        await temp


async def call_sleep_with_branchs_bad1(b):
    temp = sleep(1)
    if b:
        await temp


async def call_sleep_with_branchs_bad2(b):
    temp = sleep(1)
    if b:
        return
    await temp


async def tuple0_bad():
    t = (sleep(1), sleep(2))
    await t[0]


async def tuple1_bad():
    t = (sleep(1), sleep(2))
    await t[1]


async def tuple_ok():
    t = (sleep(1), sleep(2))
    await t[0]
    await t[1]


async def with_str_bad():
    unwaited = sleep(1)
    return str(unwaited)


async def with_str_ok():
    unwaited = sleep(1)
    s = str(unwaited)
    awaited = await unwaited
    return (awaited, s)


def fst(x: int, y: int) -> int:
    return x


async def call_fst_ok(i):
    await fst(sleep(1), i)


async def call_fst_bad(i):
    return fst(i, sleep(1))


def main_ok():
    asyncio.run(sleep(10))


async def multi_tasks_ok():
    asyncio.gather(sleep(1), sleep(2))


main_ok()

# no FP here
asyncio.run(sleep(10))
asyncio.run(multi_tasks_ok())
