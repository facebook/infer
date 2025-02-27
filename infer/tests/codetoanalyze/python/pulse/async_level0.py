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


async def compare_lt_bad():
    i = sleep()
    if i < 0:
        pass


async def compare_lt_ok():
    i = await sleep()
    if i < 0:
        pass


async def compare_gt_bad():
    i = sleep()
    if i > 0:
        pass


async def compare_gt_ok():
    i = await sleep()
    if i > 0:
        pass


async def use_py_bool_bad(b):
    x = sleep()
    return x if b else None


async def use_py_bool_ok(b):
    x = await sleep()
    return x if b else None


def true():
    return True


async def use_py_bool_true_ok():
    x = sleep()
    if true():
        await x


def false():
    return False


async def use_py_bool_false_ok():
    x = sleep()
    if false():
        pass
    else:
        await x


# FN because of py_store_subscript model
def FN_set_dict_with_unwaited_bad(key):
    d = {}
    d[key] = asyncio.sleep(0)


async def use_py_get_iter_bad():
    x = sleep()
    return [a.f for a in x]


async def use_py_get_iter_ok():
    x = await sleep()
    return [a.f for a in x]


async def gather_condition_awaitable_ok():
    awaitable = sleep()
    asyncio.gather(awaitable if awaitable else sleep())


async def concat_list_left_ok(l):
    return [sleep()] + l


async def concat_list_right_ok(l):
    return l + [sleep()]


async def concat_tuple_left_ok(l):
    return (sleep(), sleep()) + l


async def concat_tuple_right_ok(l):
    return l + (sleep(), sleep())


def main_ok():
    asyncio.run(sleep(10))


async def multi_tasks_ok():
    asyncio.gather(sleep(1), sleep(2))


main_ok()

# no FP here
asyncio.run(sleep(10))
asyncio.run(multi_tasks_ok())
