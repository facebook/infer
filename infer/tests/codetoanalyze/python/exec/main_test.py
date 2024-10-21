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

#basic import module
import module1
print('module1.f =', module1.f)
#stdout: module1.f = module1.f
module1.f = 'explicitly modified from main'
print('module1.f =', module1.f)
#stdout: module1.f = explicitly modified from main
module1.set('modified with a setter')
print('module1.f =', module1.get())
#stdout: module1.f = modified with a setter
#testing recursive function + import
print('module1.fact(5) =', module1.fact(5))
#stdout: module1.fact(5) = 120
from module1 import get as get1, set, fact as factorial
set('modified with an imported setter')
print('module1.f =', get1())
#stdout: module1.f = modified with an imported setter
from module1 import f as f_from_module1
print('module1.f =', f_from_module1)
#stdout: module1.f = modified with an imported setter
#testing recursive function + from .. import
print('factorial(5) =', factorial(5))
#stdout: factorial(5) = 120

#class body scope
x = 'global'
class C:
    saved_x = x
    x = 'local to class body'
    def get_x():
      return x
    def get_C_x():
      return C.x
print('x is', C.get_x())
#stdout: x is global
x = 'assigned by module body'
print('x is', C.get_x())
#stdout: x is assigned by module body
C.x = 'assigned as a class attribute'
print('x is', C.get_C_x())
#stdout: x is assigned as a class attribute
print('saved x is', C.saved_x)
#stdout: saved x is global
print(C.__name__)
#stdout: C

#Tuples
l = (1, '1', (0, True))
print(l)
#stdout: (1, '1', (0, True))
d = {}
print(d)
#stdout: {}
key1 = 'k1'
def key2():
      return 'key2'
d = {key1: 'val1', key2(): 'val2'}
print(d)
#stdout: {'k1': 'val1', 'key2': 'val2'}
d = {'x': 0, 'y': 'something'}
print(d)
#stdout: {'x': 0, 'y': 'something'}
print(d['x'])
#stdout: 0
d['z'] = True
print(d)
#stdout: {'x': 0, 'y': 'something', 'z': True}
