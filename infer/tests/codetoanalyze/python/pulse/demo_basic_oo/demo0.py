# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# InferPython has a new checker: assertion error detection

def returns_false():
    return False


def returns_true():
    return True


def assert_ok():
    assert returns_true()


def assert_bad():
    assert returns_false()
