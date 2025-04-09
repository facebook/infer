# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

class Animal:
    quack = False

    def can_quack(self):
        return self.quack

class Duck(Animal):
    def __init__(self):
        self.quack = True

class Cow(Animal):
    pass


# This is too hard for us right now.
# In the corresponding duck-specialized summary of Animal.can_quack, we make
# a wrong assumption about the localization of 'quack' attribute
# I would liek to invalidate such a wrong assumption with a new kind of
# specialization kay, but this is further work
def FP_a_duck_can_quack_ok():
    duck = Duck()
    assert duck.can_quack()


def a_cow_can_not_quack_bad():
    cow = Cow()
    assert cow.can_quack()
