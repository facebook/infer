(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

let%expect_test _ =
  let source =
    {|
x = 42
print(x)
builtin_print = print
print = 0
builtin_print(print, None, "hello world", True, False)
|}
  in
  PyIR.test ~show:true source ;
  F.printf "Running interpreter:@\n" ;
  PyIR.test ~run:PyIRExec.run source ;
  [%expect
    {|
    Running interpreter:
    IR error: Unsupported opcode: PUSH_NULL
    IR error: Unsupported opcode: PUSH_NULL |}]


let%expect_test _ =
  let source = {|
def fst(y, x):
    return y

x = 'x'
y = 'y'
print("fst(x, y) =", fst(x, y))
|} in
  PyIR.test ~show:true source ;
  F.printf "Running interpreter:@\n" ;
  PyIR.test ~run:PyIRExec.run source ;
  [%expect
    {|
    Running interpreter:
    IR error: Unsupported opcode: PUSH_NULL
    IR error: Unsupported opcode: PUSH_NULL |}]


let%expect_test _ =
  let source =
    {|
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
|}
  in
  PyIR.test ~show:true source ;
  F.printf "Running interpreter:@\n" ;
  PyIR.test ~run:PyIRExec.run source ;
  [%expect
    {|
    Running interpreter:
    IR error: Unsupported opcode: PUSH_NULL
    IR error: Unsupported opcode: PUSH_NULL |}]


let%expect_test _ =
  let source =
    {|
def fact(n):
    if n<=0:
        return 1
    else:
        return n * fact(n-1)

print('fact(5) =', fact(5))
|}
  in
  PyIR.test ~show:true source ;
  F.printf "Running interpreter:@\n" ;
  PyIR.test ~run:PyIRExec.run source ;
  [%expect
    {|
    Running interpreter:
    IR error: Unsupported opcode: PUSH_NULL
    IR error: Unsupported opcode: PUSH_NULL |}]


let%expect_test _ =
  let main =
    ( "main"
    , {|
import module1

print('module1.f =', module1.f)

module1.f = 'explicitly modified from main'
print('module1.f =', module1.f)

module1.set('modified with a setter')
print('module1.f =', module1.get())

from module1 import get as get1, set
set('modified with an imported setter')
print('module1.f =', get1())
from module1 import f as f_from_module1
print('module1.f =', f_from_module1)
|}
    )
  in
  let module1 =
    ("module1", {|
f = 'module1.f'

def get():
    return f

def set(v):
    global f
    f = v
|})
  in
  PyIR.test_files ~run:PyIRExec.run_files [main; module1] ;
  [%expect
    {|
    nothing to execute
    IR error: Unsupported opcode: PUSH_NULL
    IR error: Unsupported opcode: RETURN_CONST |}]


let%expect_test _ =
  let source =
    {|
x = 'global'
class C:
    saved_x = x
    x = 'local to class body'
    def get_x():
      return x
    def get_C_x():
      return C.x
print('x is', C.get_x())
x = 'assigned by module body'
print('x is', C.get_x())
C.x = 'assigned as a class attribute'
print('x is', C.get_C_x())
print('saved x is', C.saved_x)
|}
  in
  PyIR.test ~show:true source ;
  F.printf "Running interpreter:@\n" ;
  PyIR.test ~run:PyIRExec.run source ;
  [%expect
    {|
    Running interpreter:
    IR error: Unsupported opcode: PUSH_NULL
    IR error: Unsupported opcode: PUSH_NULL |}]


let%expect_test _ =
  let source =
    {|
l = (1, '1', (0, True))
print(l)
d = {}
print(d)
key1 = 'k1'
def key2():
      return 'key2'
d = {key1: 'val1', key2(): 'val2'}
print(d)
d = {'x': 0, 'y': 'something'}
print(d)
print(d['x'])
d['z'] = True
print(d)
|}
  in
  PyIR.test ~show:true source ;
  F.printf "Running interpreter:@\n" ;
  PyIR.test ~run:PyIRExec.run source ;
  [%expect
    {|
    Running interpreter:
    IR error: Unsupported opcode: PUSH_NULL
    IR error: Unsupported opcode: PUSH_NULL |}]
