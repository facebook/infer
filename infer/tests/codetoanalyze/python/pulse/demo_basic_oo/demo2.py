# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

class Duck:
    def quack(self):
        return True


class Cow:
    def quack(self):
        return False


# InferPython tries to do without type annotations hints
# Remark we really don't know much about 'animal' param here
def animal_can_quack(animal):
    return animal.quack()


def a_duck_can_quack_ok():
    duck = Duck()
    assert animal_can_quack(duck)


def a_cow_can_not_quack_bad():
    cow = Cow()
    assert animal_can_quack(cow)
