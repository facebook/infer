# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

class A:
    def __init__(self, val):
        self.attr = val

    def get(self):
        return self.attr

    def set(self, val):
        self.attr = val


def get_attr_after_init_bad():
    a = A(False)
    assert a.get()


def get_attr_after_init_ok():
    a = A(True)
    assert a.get()


def get_attr_after_set_bad():
    a = A(True)
    a.set(False)
    assert a.get()


def get_attr_after_set_ok():
    a = A(False)
    a.set(True)
    assert a.get()
