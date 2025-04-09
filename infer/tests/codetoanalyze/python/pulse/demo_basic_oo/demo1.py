# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# InferPython now supports objects instanciation and duck typing

class Duck:
    def quack(self):
        return True


class Cow:
    def quack(self):
        return False


def a_duck_can_quack_ok():
    duck = Duck()
    assert duck.quack()


def a_cow_can_not_quack_bad():
    cow = Cow()
    assert cow.quack()
