# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# the purpose of these tests is not to raise Infer signals,
# but rather to check for absence of crashes

def long_opstack_with_duplicated_calls():
    l = make(
        x1 = e1,
        x2 = e2,
        x3 = e3,
        x4 = e4,
        x5 = e5,
        x6 = e6,
        x7 = e7,
        x8 = e8,
        x_cond = v_true if b else v_false,
        x9 = e9,
        x10 = e10,
        x11 = e11,
        x12 = e12,
        x13 = e13,
        x14 = e14,
        x15 = e15,
    )
    return [ f(x) for x in l ]
