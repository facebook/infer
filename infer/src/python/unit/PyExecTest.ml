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
        b0: @2
          n0 <- None @2
          TOPLEVEL[x] <- 42 @2
          jmp b1
           @2
        b1: @3
          n3 <- TOPLEVEL[print] @3
          n4 <- TOPLEVEL[x] @3
          n5 <- $Call(n3, n4, n0) @3
          jmp b2
           @3
        b2: @4
          n6 <- TOPLEVEL[print] @4
          TOPLEVEL[builtin_print] <- n6 @4
          jmp b3
           @4
        b3: @5
          TOPLEVEL[print] <- 0 @5
          jmp b4
           @5
        b4: @6
          n7 <- TOPLEVEL[builtin_print] @6
          n8 <- TOPLEVEL[print] @6
          n9 <- $Call(n7, n8, n0, "hello world", true, false, n0) @6
          return n0
           @6


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
        b0: @2
          n0 <- None @2
          n3 <- $MakeFunction["dummy.fst", n0, n0, n0, n0] @2
          TOPLEVEL[fst] <- n3 @2
          jmp b1
           @2
        b1: @5
          TOPLEVEL[x] <- "x" @5
          jmp b2
           @5
        b2: @6
          TOPLEVEL[y] <- "y" @6
          jmp b3
           @6
        b3: @7
          n4 <- TOPLEVEL[print] @7
          n5 <- TOPLEVEL[fst] @7
          n6 <- TOPLEVEL[x] @7
          n7 <- TOPLEVEL[y] @7
          n8 <- $Call(n5, n6, n7, n0) @7
          n9 <- $Call(n4, "fst(x, y) =", n8, n0) @7
          return n0
           @7

      function dummy.fst(y, x):
        b0: @3
          n0 <- None @3
          n3 <- LOCAL[y] @3
          return n3
           @3


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
        b0: @2
          n0 <- None @2
          n3 <- $MakeFunction["dummy.incr", n0, n0, n0, n0] @2
          TOPLEVEL[incr] <- n3 @2
          jmp b1
           @2
        b1: @6
          n4 <- $MakeFunction["dummy.no_effect", n0, n0, n0, n0] @6
          TOPLEVEL[no_effect] <- n4 @6
          jmp b2
           @6
        b2: @9
          GLOBAL[n] <- 0 @9
          jmp b3
           @9
        b3: @10
          n5 <- TOPLEVEL[incr] @10
          n6 <- $Call(n5, 3, n0) @10
          jmp b4
           @10
        b4: @11
          n7 <- TOPLEVEL[incr] @11
          n8 <- $Call(n7, 2, n0) @11
          jmp b5
           @11
        b5: @12
          n9 <- TOPLEVEL[no_effect] @12
          n10 <- $Call(n9, -1, n0) @12
          jmp b6
           @12
        b6: @13
          n11 <- TOPLEVEL[print] @13
          n12 <- GLOBAL[n] @13
          n13 <- $Call(n11, "n =", n12, n0) @13
          return n0
           @13

      function dummy.incr(k):
        b0: @4
          n0 <- None @4
          n3 <- GLOBAL[n] @4
          n4 <- LOCAL[k] @4
          n5 <- $Inplace.Add(n3, n4, n0) @4
          GLOBAL[n] <- n5 @4
          return n0
           @4

      function dummy.no_effect(k):
        b0: @7
          n0 <- None @7
          n3 <- LOCAL[k] @7
          LOCAL[n] <- n3 @7
          return n0
           @7


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
        b0: @2
          n0 <- None @2
          n3 <- $MakeFunction["dummy.fact", n0, n0, n0, n0] @2
          TOPLEVEL[fact] <- n3 @2
          jmp b1
           @2
        b1: @8
          n4 <- TOPLEVEL[print] @8
          n5 <- TOPLEVEL[fact] @8
          n6 <- $Call(n5, 5, n0) @8
          n7 <- $Call(n4, "fact(5) =", n6, n0) @8
          return n0
           @8

      function dummy.fact(n):
        b0: @3
          n0 <- None @3
          n3 <- LOCAL[n] @3
          n4 <- $Compare.le(n3, 0, n0) @3
          if n4 then jmp b1 else jmp b2
           @3
        b1: @4
          return 1
           @4
        b2: @6
          n5 <- LOCAL[n] @6
          n6 <- GLOBAL[fact] @6
          n7 <- LOCAL[n] @6
          n8 <- $Binary.Subtract(n7, 1, n0) @6
          n9 <- $Call(n6, n8, n0) @6
          n10 <- $Binary.Multiply(n5, n9, n0) @6
          return n10
           @6


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
        b0: @2
          n0 <- None @2
          TOPLEVEL[x] <- "global" @2
          jmp b1
           @2
        b1: @3
          n3 <- $MakeFunction["dummy.C", n0, n0, n0, n0] @3
          n4 <- $BuildClass(n3, "C", n0) @3
          TOPLEVEL[C] <- n4 @3
          jmp b2
           @3
        b2: @10
          n5 <- TOPLEVEL[print] @10
          n6 <- TOPLEVEL[C] @10
          n7 <- $CallMethod[get_x](n6, n0) @10
          n8 <- $Call(n5, "x is", n7, n0) @10
          jmp b3
           @10
        b3: @11
          TOPLEVEL[x] <- "assigned by module body" @11
          jmp b4
           @11
        b4: @12
          n9 <- TOPLEVEL[print] @12
          n10 <- TOPLEVEL[C] @12
          n11 <- $CallMethod[get_x](n10, n0) @12
          n12 <- $Call(n9, "x is", n11, n0) @12
          jmp b5
           @12
        b5: @13
          n13 <- TOPLEVEL[C] @13
          n13.x <- "assigned as a class attribute" @13
          jmp b6
           @13
        b6: @14
          n14 <- TOPLEVEL[print] @14
          n15 <- TOPLEVEL[C] @14
          n16 <- $CallMethod[get_C_x](n15, n0) @14
          n17 <- $Call(n14, "x is", n16, n0) @14
          jmp b7
           @14
        b7: @15
          n18 <- TOPLEVEL[print] @15
          n19 <- TOPLEVEL[C] @15
          n20 <- n19.saved_x @15
          n21 <- $Call(n18, "saved x is", n20, n0) @15
          return n0
           @15

      function dummy.C():
        b0: @3
          n0 <- None @3
          n3 <- TOPLEVEL[__name__] @3
          TOPLEVEL[__module__] <- n3 @3
          TOPLEVEL[__qualname__] <- "C" @3
          jmp b1
           @3
        b1: @4
          n4 <- TOPLEVEL[x] @4
          TOPLEVEL[saved_x] <- n4 @4
          jmp b2
           @4
        b2: @5
          TOPLEVEL[x] <- "local to class body" @5
          jmp b3
           @5
        b3: @6
          n5 <- $MakeFunction["dummy.C.get_x", n0, n0, n0, n0] @6
          TOPLEVEL[get_x] <- n5 @6
          jmp b4
           @6
        b4: @8
          n6 <- $MakeFunction["dummy.C.get_C_x", n0, n0, n0, n0] @8
          TOPLEVEL[get_C_x] <- n6 @8
          return n0
           @8

      function dummy.C.get_C_x():
        b0: @9
          n0 <- None @9
          n3 <- GLOBAL[C] @9
          n4 <- n3.x @9
          return n4
           @9

      function dummy.C.get_x():
        b0: @7
          n0 <- None @7
          n3 <- GLOBAL[x] @7
          return n3
           @7


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
        b0: @2
          n0 <- None @2
          TOPLEVEL[l] <- $BuildTuple(1, "1", $BuildTuple(0, true)) @2
          jmp b1
           @2
        b1: @3
          n3 <- TOPLEVEL[print] @3
          n4 <- TOPLEVEL[l] @3
          n5 <- $Call(n3, n4, n0) @3
          jmp b2
           @3
        b10: @13
          n22 <- TOPLEVEL[print] @13
          n23 <- TOPLEVEL[d] @13
          n24 <- n23["x"] @13
          n25 <- $Call(n22, n24, n0) @13
          jmp b11
           @13
        b11: @14
          n26 <- TOPLEVEL[d] @14
          n26["z"] <- true @14
          jmp b12
           @14
        b12: @15
          n27 <- TOPLEVEL[print] @15
          n28 <- TOPLEVEL[d] @15
          n29 <- $Call(n27, n28, n0) @15
          return n0
           @15
        b2: @4
          n6 <- $BuildMap() @4
          TOPLEVEL[d] <- n6 @4
          jmp b3
           @4
        b3: @5
          n7 <- TOPLEVEL[print] @5
          n8 <- TOPLEVEL[d] @5
          n9 <- $Call(n7, n8, n0) @5
          jmp b4
           @5
        b4: @6
          TOPLEVEL[key1] <- "k1" @6
          jmp b5
           @6
        b5: @7
          n10 <- $MakeFunction["dummy.key2", n0, n0, n0, n0] @7
          TOPLEVEL[key2] <- n10 @7
          jmp b6
           @7
        b6: @9
          n11 <- TOPLEVEL[key1] @9
          n12 <- TOPLEVEL[key2] @9
          n13 <- $Call(n12, n0) @9
          n14 <- $BuildMap(n11, "val1", n13, "val2") @9
          TOPLEVEL[d] <- n14 @9
          jmp b7
           @9
        b7: @10
          n15 <- TOPLEVEL[print] @10
          n16 <- TOPLEVEL[d] @10
          n17 <- $Call(n15, n16, n0) @10
          jmp b8
           @10
        b8: @11
          n18 <- $BuildMap("x", 0, "y", "something") @11
          TOPLEVEL[d] <- n18 @11
          jmp b9
           @11
        b9: @12
          n19 <- TOPLEVEL[print] @12
          n20 <- TOPLEVEL[d] @12
          n21 <- $Call(n19, n20, n0) @12
          jmp b10
           @12

      function dummy.key2():
        b0: @8
          n0 <- None @8
          return "key2"
           @8


    Running interpreter:
    (1, '1', (0, True))
    {}
    {'k1': 'val1', 'key2': 'val2'}
    {'x': 0, 'y': 'something'}
    0
    {'x': 0, 'y': 'something', 'z': True} |}]
