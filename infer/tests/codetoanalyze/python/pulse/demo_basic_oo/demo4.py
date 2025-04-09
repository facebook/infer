# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

#InferPython now supports method overriding

class Animal:
    def quack(self):
        return False


    def can_quack(self):
        return self.quack()



class Duck(Animal):
    # we ovveride quack()
    def quack(self):
        return True


class Cow(Animal):
    # quack() in inherited from Animal
    pass


def a_duck_can_quack_ok():
    duck = Duck()
    assert duck.can_quack()


def a_cow_can_not_quack_bad():
    cow = Cow()
    assert cow.can_quack()
