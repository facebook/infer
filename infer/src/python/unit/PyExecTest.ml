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
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          TOPLEVEL[x] <- 42
          n3 <- TOPLEVEL[print]
          n4 <- TOPLEVEL[x]
          n5 <- $Call(n3, n4, n0)
          n6 <- TOPLEVEL[print]
          TOPLEVEL[builtin_print] <- n6
          TOPLEVEL[print] <- 0
          n7 <- TOPLEVEL[builtin_print]
          n8 <- TOPLEVEL[print]
          n9 <- $Call(n7, n8, n0, "hello world", true, false, n0)
          return n0



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
  PyIR.test ~show:true source ;
  F.printf "Running interpreter:@\n" ;
  PyIR.test ~run:PyIRExec.run source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.fst", n0, n0, n0, n0]
          TOPLEVEL[fst] <- n3
          TOPLEVEL[x] <- "x"
          TOPLEVEL[y] <- "y"
          n4 <- TOPLEVEL[print]
          n5 <- TOPLEVEL[fst]
          n6 <- TOPLEVEL[x]
          n7 <- TOPLEVEL[y]
          n8 <- $Call(n5, n6, n7, n0)
          n9 <- $Call(n4, "fst(x, y) =", n8, n0)
          return n0


      function dummy.fst(y, x):
        b0:
          n0 <- None
          n3 <- LOCAL[y]
          return n3



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
  PyIR.test ~show:true source ;
  F.printf "Running interpreter:@\n" ;
  PyIR.test ~run:PyIRExec.run source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.incr", n0, n0, n0, n0]
          TOPLEVEL[incr] <- n3
          n4 <- $MakeFunction["dummy.no_effect", n0, n0, n0, n0]
          TOPLEVEL[no_effect] <- n4
          GLOBAL[n] <- 0
          n5 <- TOPLEVEL[incr]
          n6 <- $Call(n5, 3, n0)
          n7 <- TOPLEVEL[incr]
          n8 <- $Call(n7, 2, n0)
          n9 <- TOPLEVEL[no_effect]
          n10 <- $Call(n9, -1, n0)
          n11 <- TOPLEVEL[print]
          n12 <- GLOBAL[n]
          n13 <- $Call(n11, "n =", n12, n0)
          return n0


      function dummy.incr(k):
        b0:
          n0 <- None
          n3 <- GLOBAL[n]
          n4 <- LOCAL[k]
          n5 <- $Inplace.Add(n3, n4, n0)
          GLOBAL[n] <- n5
          return n0


      function dummy.no_effect(k):
        b0:
          n0 <- None
          n3 <- LOCAL[k]
          LOCAL[n] <- n3
          return n0



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
  PyIR.test ~show:true source ;
  F.printf "Running interpreter:@\n" ;
  PyIR.test ~run:PyIRExec.run source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.fact", n0, n0, n0, n0]
          TOPLEVEL[fact] <- n3
          n4 <- TOPLEVEL[print]
          n5 <- TOPLEVEL[fact]
          n6 <- $Call(n5, 5, n0)
          n7 <- $Call(n4, "fact(5) =", n6, n0)
          return n0


      function dummy.fact(n):
        b0:
          n0 <- None
          n3 <- LOCAL[n]
          n4 <- $Compare.le(n3, 0, n0)
          if n4 then jmp b1 else jmp b2

        b1:
          return 1

        b2:
          n5 <- LOCAL[n]
          n6 <- GLOBAL[fact]
          n7 <- LOCAL[n]
          n8 <- $Binary.Subtract(n7, 1, n0)
          n9 <- $Call(n6, n8, n0)
          n10 <- $Binary.Multiply(n5, n9, n0)
          return n10



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
  PyIR.test ~show:true source ;
  F.printf "Running interpreter:@\n" ;
  PyIR.test ~run:PyIRExec.run source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          TOPLEVEL[x] <- "global"
          n3 <- $MakeFunction["dummy.C", n0, n0, n0, n0]
          n4 <- $BuildClass(n3, "C", n0)
          TOPLEVEL[C] <- n4
          n5 <- TOPLEVEL[print]
          n6 <- TOPLEVEL[C]
          n7 <- n6.get_x
          n8 <- $Call(n7, n0)
          n9 <- $Call(n5, "x is", n8, n0)
          TOPLEVEL[x] <- "assigned by module body"
          n10 <- TOPLEVEL[print]
          n11 <- TOPLEVEL[C]
          n12 <- n11.get_x
          n13 <- $Call(n12, n0)
          n14 <- $Call(n10, "x is", n13, n0)
          n15 <- TOPLEVEL[C]
          n15.x <- "assigned as a class attribute"
          n16 <- TOPLEVEL[print]
          n17 <- TOPLEVEL[C]
          n18 <- n17.get_C_x
          n19 <- $Call(n18, n0)
          n20 <- $Call(n16, "x is", n19, n0)
          n21 <- TOPLEVEL[print]
          n22 <- TOPLEVEL[C]
          n23 <- n22.saved_x
          n24 <- $Call(n21, "saved x is", n23, n0)
          return n0


      function dummy.C():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "C"
          n4 <- TOPLEVEL[x]
          TOPLEVEL[saved_x] <- n4
          TOPLEVEL[x] <- "local to class body"
          n5 <- $MakeFunction["dummy.C.get_x", n0, n0, n0, n0]
          TOPLEVEL[get_x] <- n5
          n6 <- $MakeFunction["dummy.C.get_C_x", n0, n0, n0, n0]
          TOPLEVEL[get_C_x] <- n6
          return n0


      function dummy.C.get_C_x():
        b0:
          n0 <- None
          n3 <- GLOBAL[C]
          n4 <- n3.x
          return n4


      function dummy.C.get_x():
        b0:
          n0 <- None
          n3 <- GLOBAL[x]
          return n3



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
  PyIR.test ~show:true source ;
  F.printf "Running interpreter:@\n" ;
  PyIR.test ~run:PyIRExec.run source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          TOPLEVEL[l] <- $BuildTuple(1, "1", $BuildTuple(0, true))
          n3 <- TOPLEVEL[print]
          n4 <- TOPLEVEL[l]
          n5 <- $Call(n3, n4, n0)
          n6 <- $BuildMap()
          TOPLEVEL[d] <- n6
          n7 <- TOPLEVEL[print]
          n8 <- TOPLEVEL[d]
          n9 <- $Call(n7, n8, n0)
          TOPLEVEL[key1] <- "k1"
          n10 <- $MakeFunction["dummy.key2", n0, n0, n0, n0]
          TOPLEVEL[key2] <- n10
          n11 <- TOPLEVEL[key1]
          n12 <- TOPLEVEL[key2]
          n13 <- $Call(n12, n0)
          n14 <- $BuildMap(n11, "val1", n13, "val2")
          TOPLEVEL[d] <- n14
          n15 <- TOPLEVEL[print]
          n16 <- TOPLEVEL[d]
          n17 <- $Call(n15, n16, n0)
          n18 <- $BuildConstKeyMap($BuildTuple("x", "y"), 0, "something", n0)
          TOPLEVEL[d] <- n18
          n19 <- TOPLEVEL[print]
          n20 <- TOPLEVEL[d]
          n21 <- $Call(n19, n20, n0)
          n22 <- TOPLEVEL[print]
          n23 <- TOPLEVEL[d]
          n24 <- n23["x"]
          n25 <- $Call(n22, n24, n0)
          n26 <- TOPLEVEL[d]
          n26["z"] <- true
          n27 <- TOPLEVEL[print]
          n28 <- TOPLEVEL[d]
          n29 <- $Call(n27, n28, n0)
          return n0


      function dummy.key2():
        b0:
          n0 <- None
          return "key2"



    Running interpreter:
    (1, '1', (0, True))
    {}
    {'k1': 'val1', 'key2': 'val2'}
    {'x': 0, 'y': 'something'}
    0
    {'x': 0, 'y': 'something', 'z': True} |}]
