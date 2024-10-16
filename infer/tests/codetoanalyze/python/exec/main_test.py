# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

x = 42
print(x)
#stdout: 42
y = True
print(y, None)
#stdout: True None
y = False
print(y, "hello world")
#stdout: False hello world

# global scope
def incr(k):
    global n
    n += k

def no_effect(k):
    n = k

n = 0
incr(3)
incr(2)
no_effect(-1)
print('n =', n)
#stdout: n = 5

# recursive function
def fact(n):
    if n<=0:
        return 1
    else:
        return n * fact(n-1)

print('fact(5) =', fact(5))
#stdout: fact(5) = 120
