# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

class A:
    class_attrA_false = False
    class_attrA_true = True
    def __init__(self, val):
        self.attrA = val

    def get_attrA(self):
        return self.attrA

    def set_attrA(self, val):
        self.attrA = val


class B(A):
    class_attrB_false = False
    class_attrB_true = True
    def get_attrB(self):
        return self.attrB

    def set_attrB(self, val):
        self.attrB = val


def not_inherited_attribute_bad():
    b = B()
    b.attrB = False
    assert b.attrB


def not_inherited_attribute_ok():
    b = B()
    b.attrB = True
    assert b.attrB


def not_inherited_method_bad():
    b = B()
    b.set_attrB(False)
    assert b.get_attrB()


def not_inherited_method_ok():
    b = B()
    b.set_attrB(True)
    assert b.get_attrB()


def inherited_class_attribute_B_bad():
    b = B()
    assert b.class_attrB_false


def inherited_class_attribute_B_ok():
    b = B()
    assert b.class_attrB_true


def inherited_class_attribute_A_bad():
    b = B()
    assert b.class_attrA_false


def inherited_class_attribute_A_ok():
    b = B()
    assert b.class_attrA_true


def inherited_method_bad():
    b = B()
    b.set_attrA(False)
    assert b.get_attrA()


def inherited_method_ok():
    b = B()
    b.set_attrA(True)
    assert b.get_attrA()


def inherited_class_modified_attribute_B_1_bad():
    B.class_attrB_true = False
    b = B()
    assert b.class_attrB_true


def inherited_class_modified_attribute_B_1_ok():
    B.class_attrB_false = True
    b = B()
    assert b.class_attrB_false


def inherited_class_modified_attribute_B_2_bad():
    b = B()
    B.class_attrB_true = False
    assert b.class_attrB_true


def inherited_class_modified_attribute_B_2_ok():
    b = B()
    B.class_attrB_false = True
    assert b.class_attrB_false


def FN_inherited_class_modified_attribute_A_1_bad():
    A.class_attrA_true = False
    b = B()
    assert b.class_attrA_true


def FP_inherited_class_modified_attribute_A_1_ok():
    A.class_attrA_false = True
    b = B()
    assert b.class_attrA_false


def FN_inherited_class_modified_attribute_A_2_bad():
    b = B()
    A.class_attrA_true = False
    assert b.class_attrA_true


def FP_inherited_class_modified_attribute_A_2_ok():
    b = B()
    A.class_attrA_false = True
    assert b.class_attrA_false
