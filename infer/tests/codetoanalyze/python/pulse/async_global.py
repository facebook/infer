# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import asyncio


async def set_unwaited_global():
    global g
    g = asyncio.sleep()


async def await_global():
    global g
    await g

#ok
asyncio.run(set_unwaited_global())
asyncio.run(await_global())
g = 0

#bad
asyncio.run(set_unwaited_global())
g = 0
