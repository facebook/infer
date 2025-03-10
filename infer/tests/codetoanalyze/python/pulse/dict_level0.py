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


class DictField:

    def fn_get_key_bad(self):
        self.d = {"name": "Alice", "city": "New York"}
        return self.d["bla"]
