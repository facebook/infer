# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import asyncio


async def foo():
    await asyncio.sleep(1)


async def lambda_bad():
    x = lambda: foo()
    x()


async def lambda_good():
    x = lambda: foo()
    await x()


async def lambda_param_bad(f):
    f()


async def lambda_param_bad_call_bad():
    lambda_param_bad(lambda: foo())


lambda_param_bad(lambda: foo())
