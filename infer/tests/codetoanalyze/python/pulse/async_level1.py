# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

class C:
    async def gen():
        pass

class D:
    def main():
        x = C.gen()

if __name__ == '__main__':
    # common pattern
    pass
