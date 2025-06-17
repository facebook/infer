# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import asyncio
from unknown import (_async_fun, async_fun)

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


# we decide that boolification of an unawaited awaitable is too dangerous
async def gather_condition_awaitable_bad():
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


async def do_not_await_arg(arg):
    pass


async def await_arg(arg):
    await arg


async def use_await_arg_ok():
    unawaited = async_fun()
    await await_arg(unawaited)


async def use_do_not_await_arg_bad():
    unawaited = async_fun()
    await do_not_await_arg(unawaited)


async def use_await_arg_named_arg_ok():
    unawaited = async_fun()
    await await_arg(arg=unawaited)


async def use_do_not_await_arg_named_arg_bad():
    unawaited = async_fun()
    await do_not_await_arg(arg=unawaited)


async def do_not_await_arg2_with_star(arg1, *, arg2):
    pass


async def await_arg2_with_star(arg1, *, arg2):
    await arg2


async def use_await_arg2_named_arg_with_star_ok():
    unawaited = async_fun()
    await await_arg2_with_star(None, arg2=unawaited)


async def FN_use_do_not_await_arg2_named_arg_with_star_bad():
    unawaited = async_fun()
    await do_not_await_arg2_with_star(None, arg2=unawaited)


def get_option_awaitable(b):
    if b:
        return asyncio.sleep(1)
    else:
        return None


async def FP_3_12_call_get_option_awaitable_eq_test_none_ok(b):
    unawaited = get_option_awaitable(b)
    if unawaited is None:
        return None
    return await unawaited


async def call_get_option_awaitable_eq_test_none_bad(b):
    unawaited = get_option_awaitable(b)
    if unawaited is None:
        return await unawaited


async def async_naming_convention_test1_bad():
    _async_fun()


async def async_naming_convention_test1_ok():
    await _async_fun()


async def async_naming_convention_test2_bad():
    async_fun()


async def async_naming_convention_test2_ok():
    await async_fun()

class C:
    async def async_instance_method(self):
        pass

    async def async_naming_convention_test3_bad(self):
        self.async_instance_method()


    async def async_naming_convention_test3_ok():
        await self.async_instance_method()


    async def _async_private_instance_method(self):
        pass

    async def async_naming_convention_test4_bad(self):
        self._async_private_instance_method()


    async def async_naming_convention_test4_ok():
        await self._async_private_instance_method()


async def iter_arg_bad(f):
    iter = async_fun()
    for i in iter:
        f(i)
    return iter


async def iter_arg_ok(f):
    iter = await async_fun()
    for i in iter:
        f(i)
    return iter


def my_iter(f, l):
    for x in l:
        f(x)


async def my_iter_arg_bad(f):
    l = async_fun()
    my_iter(f, l)
    return l


async def my_iter_arg_ok(f):
    l = await async_fun()
    my_iter(f, l)
    return l


async def FN_3_10_none_test_bad():
    opt = async_fun()
    if opt is None:
        do()
    return opt


async def not_none_test_bad():
    opt = async_fun()
    if opt is not None:
        do()
    return opt


async def none_test_ok():
    opt = await async_fun()
    if opt is None:
        do()
    return opt

async def not_none_test_ok():
    opt = await async_fun()
    if opt is not None:
        do()
    return opt


def main_ok():
    asyncio.run(sleep(10))


async def multi_tasks_ok():
    asyncio.gather(sleep(1), sleep(2))


main_ok()

# no FP here
asyncio.run(sleep(10))
asyncio.run(multi_tasks_ok())
