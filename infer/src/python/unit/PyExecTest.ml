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
  PyIR.test source ;
  F.printf "Running interpreter:@\n" ;
  PyIR.test ~run:PyIRExec.run source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          TOPLEVEL[x] <- 42
          n0 <- TOPLEVEL[print]
          n1 <- TOPLEVEL[x]
          n2 <- $Call(n0, n1, None)
          n3 <- TOPLEVEL[print]
          TOPLEVEL[builtin_print] <- n3
          TOPLEVEL[print] <- 0
          n4 <- TOPLEVEL[builtin_print]
          n5 <- TOPLEVEL[print]
          n6 <- $Call(n4, n5, None, "hello world", true, false, None)
          return None



    Running interpreter:
    42
    0 None hello world True False |}]


let%expect_test _ =
  let source = {|
def fst(y, x):
    return y

x = 'x'
y = 'y'
print("fst(x, y) =", fst(x, y))
|} in
  PyIR.test source ;
  F.printf "Running interpreter:@\n" ;
  PyIR.test ~run:PyIRExec.run source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $MakeFunction["fst", "dummy.fst", None, None, None, None]
          TOPLEVEL[fst] <- n0
          TOPLEVEL[x] <- "x"
          TOPLEVEL[y] <- "y"
          n1 <- TOPLEVEL[print]
          n2 <- TOPLEVEL[fst]
          n3 <- TOPLEVEL[x]
          n4 <- TOPLEVEL[y]
          n5 <- $Call(n2, n3, n4, None)
          n6 <- $Call(n1, "fst(x, y) =", n5, None)
          return None


      function dummy.fst(y, x):
        b0:
          n0 <- LOCAL[y]
          return n0



    Running interpreter:
    fst(x, y) = x |}]


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
  PyIR.test source ;
  F.printf "Running interpreter:@\n" ;
  PyIR.test ~run:PyIRExec.run source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $MakeFunction["incr", "dummy.incr", None, None, None, None]
          TOPLEVEL[incr] <- n0
          n1 <- $MakeFunction["no_effect", "dummy.no_effect", None, None, None, None]
          TOPLEVEL[no_effect] <- n1
          GLOBAL[n] <- 0
          n2 <- TOPLEVEL[incr]
          n3 <- $Call(n2, 3, None)
          n4 <- TOPLEVEL[incr]
          n5 <- $Call(n4, 2, None)
          n6 <- TOPLEVEL[no_effect]
          n7 <- $Call(n6, -1, None)
          n8 <- TOPLEVEL[print]
          n9 <- GLOBAL[n]
          n10 <- $Call(n8, "n =", n9, None)
          return None


      function dummy.incr(k):
        b0:
          n0 <- GLOBAL[n]
          n1 <- LOCAL[k]
          n2 <- $Inplace.Add(n0, n1, None)
          GLOBAL[n] <- n2
          return None


      function dummy.no_effect(k):
        b0:
          n0 <- LOCAL[k]
          LOCAL[n] <- n0
          return None



    Running interpreter:
    n = 5 |}]


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
  PyIR.test source ;
  F.printf "Running interpreter:@\n" ;
  PyIR.test ~run:PyIRExec.run source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $MakeFunction["fact", "dummy.fact", None, None, None, None]
          TOPLEVEL[fact] <- n0
          n1 <- TOPLEVEL[print]
          n2 <- TOPLEVEL[fact]
          n3 <- $Call(n2, 5, None)
          n4 <- $Call(n1, "fact(5) =", n3, None)
          return None


      function dummy.fact(n):
        b0:
          n0 <- LOCAL[n]
          n1 <- $Compare.le(n0, 0, None)
          if n1 then jmp b1 else jmp b2

        b1:
          return 1

        b2:
          n2 <- LOCAL[n]
          n3 <- GLOBAL[fact]
          n4 <- LOCAL[n]
          n5 <- $Binary.Subtract(n4, 1, None)
          n6 <- $Call(n3, n5, None)
          n7 <- $Binary.Multiply(n2, n6, None)
          return n7



    Running interpreter:
    fact(5) = 120 |}]


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
    module1.f = module1.f
    module1.f = explicitly modified from main
    module1.f = modified with a setter
    module1.f = modified with an imported setter
    module1.f = modified with an imported setter |}]


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
  PyIR.test source ;
  F.printf "Running interpreter:@\n" ;
  PyIR.test ~run:PyIRExec.run source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          TOPLEVEL[x] <- "global"
          n0 <- $MakeFunction["C", "dummy.C", None, None, None, None]
          n1 <- $BuildClass(n0, "C", None)
          TOPLEVEL[C] <- n1
          n2 <- TOPLEVEL[print]
          n3 <- TOPLEVEL[C]
          n4 <- $CallMethod[get_x](n3, None)
          n5 <- $Call(n2, "x is", n4, None)
          TOPLEVEL[x] <- "assigned by module body"
          n6 <- TOPLEVEL[print]
          n7 <- TOPLEVEL[C]
          n8 <- $CallMethod[get_x](n7, None)
          n9 <- $Call(n6, "x is", n8, None)
          n10 <- TOPLEVEL[C]
          n10.x <- "assigned as a class attribute"
          n11 <- TOPLEVEL[print]
          n12 <- TOPLEVEL[C]
          n13 <- $CallMethod[get_C_x](n12, None)
          n14 <- $Call(n11, "x is", n13, None)
          n15 <- TOPLEVEL[print]
          n16 <- TOPLEVEL[C]
          n17 <- n16.saved_x
          n18 <- $Call(n15, "saved x is", n17, None)
          return None


      function dummy.C():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          n1 <- TOPLEVEL[x]
          TOPLEVEL[saved_x] <- n1
          TOPLEVEL[x] <- "local to class body"
          n2 <- $MakeFunction["get_x", "dummy.C.get_x", None, None, None, None]
          TOPLEVEL[get_x] <- n2
          n3 <- $MakeFunction["get_C_x", "dummy.C.get_C_x", None, None, None, None]
          TOPLEVEL[get_C_x] <- n3
          return None


      function dummy.C.get_C_x():
        b0:
          n0 <- GLOBAL[C]
          n1 <- n0.x
          return n1


      function dummy.C.get_x():
        b0:
          n0 <- GLOBAL[x]
          return n0



    Running interpreter:
    x is global
    x is assigned by module body
    x is assigned as a class attribute
    saved x is global |}]


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
  PyIR.test source ;
  F.printf "Running interpreter:@\n" ;
  PyIR.test ~run:PyIRExec.run source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          TOPLEVEL[l] <- $BuildTuple(1, "1", $BuildTuple(0, true))
          n0 <- TOPLEVEL[print]
          n1 <- TOPLEVEL[l]
          n2 <- $Call(n0, n1, None)
          TOPLEVEL[d] <- $BuildMap()
          n3 <- TOPLEVEL[print]
          n4 <- TOPLEVEL[d]
          n5 <- $Call(n3, n4, None)
          TOPLEVEL[key1] <- "k1"
          n6 <- $MakeFunction["key2", "dummy.key2", None, None, None, None]
          TOPLEVEL[key2] <- n6
          n7 <- TOPLEVEL[key1]
          n8 <- TOPLEVEL[key2]
          n9 <- $Call(n8, None)
          TOPLEVEL[d] <- $BuildMap(n7, "val1", n9, "val2")
          n10 <- TOPLEVEL[print]
          n11 <- TOPLEVEL[d]
          n12 <- $Call(n10, n11, None)
          n13 <- $BuildConstKeyMap($BuildTuple("x", "y"), 0, "something", None)
          TOPLEVEL[d] <- n13
          n14 <- TOPLEVEL[print]
          n15 <- TOPLEVEL[d]
          n16 <- $Call(n14, n15, None)
          n17 <- TOPLEVEL[print]
          n18 <- TOPLEVEL[d]
          n19 <- n18["x"]
          n20 <- $Call(n17, n19, None)
          n21 <- TOPLEVEL[d]
          n21["z"] <- true
          n22 <- TOPLEVEL[print]
          n23 <- TOPLEVEL[d]
          n24 <- $Call(n22, n23, None)
          return None


      function dummy.key2():
        b0:
          return "key2"



    Running interpreter:
    (1, '1', (0, True))
    {}
    {'k1': 'val1', 'key2': 'val2'}
    {'x': 0, 'y': 'something'}
    0
    {'x': 0, 'y': 'something', 'z': True} |}]
