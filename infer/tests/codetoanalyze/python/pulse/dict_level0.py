# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

def dict_missing_key_const_str_ok():
    d = dict(name="Alice", age=25, city="New York")
    return d["name"]


def dict_missing_key_const_str_bad():
    d = {"John": 30, "Mary": 28}
    return d["Samantha"]

def fn_dict_size1_missing_key_const_str_bad():
    d = {"John": 30} #use build_map instead of build_const_key_map
    return d["Samantha"]


def dict_missing_key_const_str_with_int_key_bad():
    d = {"John": 30, "Mary": 28, 1: 234}
    return d["Samantha"]


def dict_missing_key_const_str_store_int_key_bad():
    d = {"John": 30, "Mary": 28}
    d[1] = 234
    return d["Samantha"]


def dict_missing_key_const_str_store_str_key_ok(s):
    d = {"John": 30, "Mary": 28}
    s = str(s)
    d[s] = 234
    return d["Samantha"]


def dict_set_key_after_init_ok():
    d = {"John": 30, "Mary": 28}
    d["Samantha"] = 60
    return d["Samantha"]


def get_dict():
    return {"John": 30, "Mary": 28}


def dict_access_fun_call_ok():
    ages = get_dict()
    return ages["John"]


def dict_access_fun_call_bad():
    ages = get_dict()
    return ages["Samantha"]


def get_val():
    return 1


def dict_missing_key_var_ok():
    y = get_val()
    d = {"ABC": 1, y: 2}
    return d[1]


def dict_missing_key_dict_builtin_const_map_bad():
    d = dict({"John": 30, "Mary": 28, 1: 234})
    return d["missing_key"]


def dict_missing_key_builtin_empty_bad():
    d = dict()
    return d["missing_key"]


def fn_dict_missing_key_builtin_empty_list_bad():
    d = dict([])
    return d["missing_key"]


def fn_dict_missing_key_builtin_list_comp_bad():
    d = dict([(x, x) for x in range(10)])
    return d["missing_key"]


def fn_dict_missing_key_named_arguments_bad():
    d = dict(name="Alice", city="New York")
    return d["missing_key"]


def dict_missing_str_key_in_op_ok():
    d = dict({"John": 30, "Mary": 28, 1: 234})
    if "missing_key" in d:
        return d["missing_key"]
    else:
        return 0


def dict_missing_str_key_in_op_bad():
    d = dict({"John": None, "Mary": 28, 1: 234})
    if "John" in d:
        return d["missing_key"]
    else:
        return 0


def dict_missing_str_key_not_const_in_op_ok(x):
    k = str(x)
    d = dict({"John": None, "Mary": 28, 1: 234, k: "unknown"})
    if "missing_key" in d:
        return d["missing_key"]
    else:
        return 0


def dict_missing_str_key_not_const_in_op_ok2(x):
    d = dict({"John": None, "Mary": 28, 1: 234})
    k = str(x)
    d[k] = "unknown"
    if "John" in d:
        return d["missing_key"]
    else:
        return 0


def dict_missing_int_key_in_op_ok():
    d = dict({"John": 30, "Mary": 28, 1: 234})
    if 1 in d:
        return d[1]
    else:
        return 0


def fp_dict_missing_int_key_in_op_ok():
    d = dict({"John": 30, "Mary": 28, 1: 234})
    if 2 in d:
        return d["missing"]
    else:
        return 0


class DictField:

    def get_key_bad(self):
        self.d = {"name": "Alice", "city": "New York"}
        return self.d["bla"]


class D:
    class_property1 = "key1"

    def __init__(self):
        self.key = "instance_key"


def read_class_companion_property_bad():
    my_dict = {'key2': 0, "key": 1}
    return my_dict[D.class_property1]


def read_class_companion_property_ok():
    my_dict = {'key1': 0, "key": 1}
    return my_dict[D.class_property1]


def instance_attribute_from_class_companion_bad():
    my_dict = {'key2': 0, "key": 1}
    d = D()
    return my_dict[d.class_property1]


def instance_attribute_from_class_companion_ok():
    my_dict = {'key1': 0, "key": 1}
    d = D()
    return my_dict[d.class_property1]

def instance_attribute_bad():
    my_dict = {'key2': 0, "key": 1}
    d = D()
    return my_dict[d.key]


def instance_attribute_ok():
    my_dict = {'key1': 0, "instance_key": 1}
    d = D()
    return my_dict[d.key]

#TODO: implement missing-attribute logic (we will need to make sure the capture is deep enough regarding inherited class)
#TODO: decide if missing-attribute deserves a specific signal, distinct from dict-missing-key
def fn_neither_instance_nor_class_attribute_bad():
    d = D()
    return d.unknown
