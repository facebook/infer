# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import asyncio


class Car:
    def __init__(self, make, model):
        self.make = make
        self.model = model

    def drive(self):
        return 0

    async def refuel(self):
        await asyncio.sleep(2)


async def car_instance_ok():
    my_car = Car("Fusca", "1999")
    my_car.drive()
    await my_car.refuel()


async def fn_car_instance_bad():
    my_car = Car("Fusca", "1999")
    my_car.drive()
    return my_car.refuel()
