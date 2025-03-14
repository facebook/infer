# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


class C:

    async def gen():
        pass


class D:
    def __init__(self):
        self.unawaited = C.gen()

    def main():
        x = C.gen()


async def instantiate_D_ok():
    d = D()
    await d.unawaited


def instantiate_D_bad():
    d = D()


if __name__ == '__main__':
    # common pattern
    pass
