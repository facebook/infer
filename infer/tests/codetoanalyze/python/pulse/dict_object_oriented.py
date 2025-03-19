# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

class Box:
    def __init__(self, key):
        self.key = key

    def get_key(self):
        return self.key

def set_attribute_with_init_bad():
    my_dict = {"key0": 0, "key1": 1}
    box = Box("key2")
    return my_dict[box.key]


def set_attribute_with_init_ok():
    my_dict = {"key0": 0, "key1": 1}
    box = Box("key0")
    return my_dict[box.key]


def set_attribute_with_init_read_with_getter_bad():
    my_dict = {"key0": 0, "key1": 1}
    box = Box("key2")
    return my_dict[box.get_key()]

def set_attribute_with_init_read_with_getter_ok():
    my_dict = {"key0": 0, "key1": 1}
    box = Box("key0")
    return my_dict[box.get_key()]
