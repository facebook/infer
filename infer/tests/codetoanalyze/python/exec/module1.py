# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

f = 'module1.f'

def get():
    return f

def set(v):
    global f
    f = v

def fact(n):
    if n<=0:
        return 1
    else:
        return n * fact(n-1)
