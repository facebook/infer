# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


class Box:
    def __init__(self, key):
        self.key = key

    def get_key(self):
        return self.key

    def set_key(self, val):
        self.key = val


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


def set_attribute_with_setter_read_with_getter_bad():
    my_dict = {"key0": 0, "key1": 1}
    box = Box("key0")
    box.set_key("key2")
    return my_dict[box.get_key()]


def set_attribute_with_setter_read_with_getter_ok():
    my_dict = {"key0": 0, "key1": 1}
    box = Box("key2")
    box.set_key("key0")
    return my_dict[box.get_key()]


# requires specialization because box could be anything
def return_get_key(box):
    return box.get_key()


def set_attribute_with_init_read_with_return_get_key_bad():
    my_dict = {"key0": 0, "key1": 1}
    box = Box("key2")
    return my_dict[return_get_key(box)]


def set_attribute_with_init_read_with_return_get_key_ok():
    my_dict = {"key0": 0, "key1": 1}
    box = Box("key0")
    return my_dict[return_get_key(box)]


# requires specialization because box and dict could be anything
# the generated summary returns an unknown value because of subscript builtin
def read_dict_using_get_key(d, box):
    return d[box.get_key()]


def fn_set_attribute_with_init_read_with_external_call_bad():
    my_dict = {"key0": 0, "key1": 1}
    box = Box("key2")
    return read_dict_using_get_key(my_dict, box)


def set_attribute_with_init_read_with_external_call_ok():
    my_dict = {"key0": 0, "key1": 1}
    box = Box("key0")
    return read_dict_using_get_key(my_dict, box)


class D:
    class_property1 = "key1"

    def __init__(self):
        self.key = "instance_key"


def read_class_companion_property_bad():
    my_dict = {"key2": 0, "key": 1}
    return my_dict[D.class_property1]


def read_class_companion_property_ok():
    my_dict = {"key1": 0, "key": 1}
    return my_dict[D.class_property1]


def instance_attribute_from_class_companion_bad():
    my_dict = {"key2": 0, "key": 1}
    d = D()
    return my_dict[d.class_property1]


def instance_attribute_from_class_companion_ok():
    my_dict = {"key1": 0, "key": 1}
    d = D()
    return my_dict[d.class_property1]


def instance_attribute_bad():
    my_dict = {"key2": 0, "key": 1}
    d = D()
    return my_dict[d.key]


def instance_attribute_ok():
    my_dict = {"key1": 0, "instance_key": 1}
    d = D()
    return my_dict[d.key]


# TODO: implement missing-attribute logic
# (we will need to make sure the capture is deep enough regarding inherited class)
# TODO: decide if missing-attribute deserves a specific signal, distinct from dict-missing-key
def fn_neither_instance_nor_class_attribute_bad():
    d = D()
    return d.unknown


class DictField:

    def get_key_bad(self):
        self.d = {"name": "Alice", "city": "New York"}
        return self.d["bla"]
