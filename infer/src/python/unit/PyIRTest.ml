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
# user-defined top level function
def my_fun(x, y):
        print(x)
        print(y)
        # local variable z
        z = x + y
        return z

a = 10
# global variable z
z = my_fun(42, a)
print(z)
      |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.my_fun", n0, n0, n0, n0]
          TOPLEVEL[my_fun] <- n3
          TOPLEVEL[a] <- 10
          n4 <- TOPLEVEL[my_fun]
          n5 <- TOPLEVEL[a]
          n6 <- $Call(n4, 42, n5, n0)
          TOPLEVEL[z] <- n6
          n7 <- TOPLEVEL[print]
          n8 <- TOPLEVEL[z]
          n9 <- $Call(n7, n8, n0)
          return n0


      function dummy.my_fun(x, y):
        b0:
          n0 <- None
          n3 <- GLOBAL[print]
          n4 <- LOCAL[x]
          n5 <- $Call(n3, n4, n0)
          n6 <- GLOBAL[print]
          n7 <- LOCAL[y]
          n8 <- $Call(n6, n7, n0)
          n9 <- LOCAL[x]
          n10 <- LOCAL[y]
          n11 <- $Binary.Add(n9, n10, n0)
          LOCAL[z] <- n11
          n12 <- LOCAL[z]
          return n12 |}]


let%expect_test _ =
  let source =
    {|
# testing global python attribute
def update_global():
        global z
        z = z + 1

z = 0
update_global()
print(z)
      |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.update_global", n0, n0, n0, n0]
          TOPLEVEL[update_global] <- n3
          GLOBAL[z] <- 0
          n4 <- TOPLEVEL[update_global]
          n5 <- $Call(n4, n0)
          n6 <- TOPLEVEL[print]
          n7 <- GLOBAL[z]
          n8 <- $Call(n6, n7, n0)
          return n0


      function dummy.update_global():
        b0:
          n0 <- None
          n3 <- GLOBAL[z]
          n4 <- $Binary.Add(n3, 1, n0)
          GLOBAL[z] <- n4
          return n0 |}]


let%expect_test _ =
  let source =
    {|
def coin():
    return False

def f(x, y):
    if coin():
          return x
    else:
          return y
      |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.coin", n0, n0, n0, n0]
          TOPLEVEL[coin] <- n3
          n4 <- $MakeFunction["dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n4
          return n0


      function dummy.coin():
        b0:
          n0 <- None
          return false


      function dummy.f(x, y):
        b0:
          n0 <- None
          n3 <- GLOBAL[coin]
          n4 <- $Call(n3, n0)
          if n4 then jmp b1 else jmp b2

        b1:
          n6 <- LOCAL[x]
          return n6

        b2:
          n5 <- LOCAL[y]
          return n5 |}]


let%expect_test _ =
  let source =
    {|
def coin():
    return False

def f(x, y):
    z = 0
    if coin():
          z = x
    else:
          z = y
    return z
      |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.coin", n0, n0, n0, n0]
          TOPLEVEL[coin] <- n3
          n4 <- $MakeFunction["dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n4
          return n0


      function dummy.coin():
        b0:
          n0 <- None
          return false


      function dummy.f(x, y):
        b0:
          n0 <- None
          LOCAL[z] <- 0
          n3 <- GLOBAL[coin]
          n4 <- $Call(n3, n0)
          if n4 then jmp b1 else jmp b2

        b1:
          n7 <- LOCAL[x]
          LOCAL[z] <- n7
          n8 <- LOCAL[z]
          return n8

        b2:
          n5 <- LOCAL[y]
          LOCAL[z] <- n5
          n6 <- LOCAL[z]
          return n6 |}]


let%expect_test _ =
  let source =
    {|
def coin():
    return False

def f(x, y):
    z = 0
    if coin():
          if coin():
            z = x
          else:
            return 1664
          z = z + 1
    else:
          z = z + 1
          if coin():
            return 42
          else:
            z = y
    return z
      |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.coin", n0, n0, n0, n0]
          TOPLEVEL[coin] <- n3
          n4 <- $MakeFunction["dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n4
          return n0


      function dummy.coin():
        b0:
          n0 <- None
          return false


      function dummy.f(x, y):
        b0:
          n0 <- None
          LOCAL[z] <- 0
          n3 <- GLOBAL[coin]
          n4 <- $Call(n3, n0)
          if n4 then jmp b1 else jmp b5

        b1:
          n11 <- GLOBAL[coin]
          n12 <- $Call(n11, n0)
          if n12 then jmp b2 else jmp b3

        b2:
          n13 <- LOCAL[x]
          LOCAL[z] <- n13
          jmp b4

        b3:
          return 1664

        b4:
          n14 <- LOCAL[z]
          n15 <- $Binary.Add(n14, 1, n0)
          LOCAL[z] <- n15
          n16 <- LOCAL[z]
          return n16

        b5:
          n5 <- LOCAL[z]
          n6 <- $Binary.Add(n5, 1, n0)
          LOCAL[z] <- n6
          n7 <- GLOBAL[coin]
          n8 <- $Call(n7, n0)
          if n8 then jmp b6 else jmp b7

        b6:
          return 42

        b7:
          n9 <- LOCAL[y]
          LOCAL[z] <- n9
          n10 <- LOCAL[z]
          return n10 |}]


let%expect_test _ =
  let source = {|
def foo(x):
    pass

def f(x):
    foo(1 if x else 0)
      |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.foo", n0, n0, n0, n0]
          TOPLEVEL[foo] <- n3
          n4 <- $MakeFunction["dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n4
          return n0


      function dummy.f(x):
        b0:
          n0 <- None
          n3 <- GLOBAL[foo]
          n4 <- LOCAL[x]
          if n4 then jmp b1 else jmp b2

        b1:
          n6 <- $Call(n3, 1, n0)
          return n0

        b2:
          n5 <- $Call(n3, 0, n0)
          return n0


      function dummy.foo(x):
        b0:
          n0 <- None
          return n0 |}]


let%expect_test _ =
  let source = {|
for x in range(10):
    print(x)
      |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[range]
          n4 <- $Call(n3, 10, n0)
          n5 <- $GetIter(n4, n0)
          jmp b1

        b1:
          n6 <- $NextIter(n5, n0)
          n7 <- $HasNextIter(n5, n0)
          if n7 then jmp b2 else jmp b3

        b2:
          TOPLEVEL[x] <- n6
          n8 <- TOPLEVEL[print]
          n9 <- TOPLEVEL[x]
          n10 <- $Call(n8, n9, n0)
          jmp b1

        b3:
          return n0 |}]


let%expect_test _ =
  let source =
    {|
def f(x, y, l, bar, toto):
    for x in l:
        with bar(), toto() as obj:
            if y:
                continue
            print('nop')
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n3
          return n0


      function dummy.f(x, y, l, bar, toto):
        b0:
          n0 <- None
          n3 <- LOCAL[l]
          n4 <- $GetIter(n3, n0)
          jmp b1

        b1:
          n5 <- $NextIter(n4, n0)
          n6 <- $HasNextIter(n4, n0)
          if n6 then jmp b2 else jmp b13

        b12:
          jmp b1

        b13:
          return n0

        b2:
          LOCAL[x] <- n5
          n7 <- LOCAL[bar]
          n8 <- $Call(n7, n0)
          n9 <- $CallMethod[__enter__](n8, n0)
          n10 <- LOCAL[toto]
          n11 <- $Call(n10, n0)
          n12 <- $CallMethod[__enter__](n11, n0)
          LOCAL[obj] <- n12
          n13 <- LOCAL[y]
          if n13 then jmp b3 else jmp b4

        b3:
          n18 <- $CallMethod[__exit__](n11, n0)
          n19 <- $CallMethod[__exit__](n8, n0)
          jmp b1

        b4:
          n14 <- GLOBAL[print]
          n15 <- $Call(n14, "nop", n0)
          n16 <- $CallMethod[__exit__](n11, n0)
          jmp b8

        b8:
          n17 <- $CallMethod[__exit__](n8, n0)
          jmp b12 |}]


let%expect_test _ =
  let source =
    {|
print(42)

def print(x):
        return x

print(42)

def f(x):
        print(x)
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[print]
          n4 <- $Call(n3, 42, n0)
          n5 <- $MakeFunction["dummy.print", n0, n0, n0, n0]
          TOPLEVEL[print] <- n5
          n6 <- TOPLEVEL[print]
          n7 <- $Call(n6, 42, n0)
          n8 <- $MakeFunction["dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n8
          return n0


      function dummy.f(x):
        b0:
          n0 <- None
          n3 <- GLOBAL[print]
          n4 <- LOCAL[x]
          n5 <- $Call(n3, n4, n0)
          return n0


      function dummy.print(x):
        b0:
          n0 <- None
          n3 <- LOCAL[x]
          return n3 |}]


let%expect_test _ =
  let source =
    {|
def f0(x: int, y, z:float):
        pass

def f1(x, y:str) -> bool:
        pass
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[int]
          n4 <- TOPLEVEL[float]
          n5 <- $BuildTuple("x", n3, "z", n4)
          n6 <- $MakeFunction["dummy.f0", n0, n0, n5, n0]
          TOPLEVEL[f0] <- n6
          n7 <- TOPLEVEL[str]
          n8 <- TOPLEVEL[bool]
          n9 <- $BuildTuple("y", n7, "return", n8)
          n10 <- $MakeFunction["dummy.f1", n0, n0, n9, n0]
          TOPLEVEL[f1] <- n10
          return n0


      function dummy.f0(x, y, z):
        b0:
          n0 <- None
          return n0


      function dummy.f1(x, y):
        b0:
          n0 <- None
          return n0 |}]


let%expect_test _ =
  let source =
    {|
def expect_int(x: int):
        pass

def get() -> int:
        return 42

expect_int(get())
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[int]
          n4 <- $BuildTuple("x", n3)
          n5 <- $MakeFunction["dummy.expect_int", n0, n0, n4, n0]
          TOPLEVEL[expect_int] <- n5
          n6 <- TOPLEVEL[int]
          n7 <- $BuildTuple("return", n6)
          n8 <- $MakeFunction["dummy.get", n0, n0, n7, n0]
          TOPLEVEL[get] <- n8
          n9 <- TOPLEVEL[expect_int]
          n10 <- TOPLEVEL[get]
          n11 <- $Call(n10, n0)
          n12 <- $Call(n9, n11, n0)
          return n0


      function dummy.expect_int(x):
        b0:
          n0 <- None
          return n0


      function dummy.get():
        b0:
          n0 <- None
          return 42 |}]


let%expect_test _ =
  let source =
    {|
def expect(x: object) -> None:
        pass

def get() -> int:
        return 42

expect(get())
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[object]
          n4 <- $BuildTuple("x", n3, "return", n0)
          n5 <- $MakeFunction["dummy.expect", n0, n0, n4, n0]
          TOPLEVEL[expect] <- n5
          n6 <- TOPLEVEL[int]
          n7 <- $BuildTuple("return", n6)
          n8 <- $MakeFunction["dummy.get", n0, n0, n7, n0]
          TOPLEVEL[get] <- n8
          n9 <- TOPLEVEL[expect]
          n10 <- TOPLEVEL[get]
          n11 <- $Call(n10, n0)
          n12 <- $Call(n9, n11, n0)
          return n0


      function dummy.expect(x):
        b0:
          n0 <- None
          return n0


      function dummy.get():
        b0:
          n0 <- None
          return 42 |}]


let%expect_test _ =
  let source = {|
def f(x, y):
  return (x == y)
        |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n3
          return n0


      function dummy.f(x, y):
        b0:
          n0 <- None
          n3 <- LOCAL[x]
          n4 <- LOCAL[y]
          n5 <- $Compare.eq(n3, n4, n0)
          return n5 |}]


let%expect_test _ =
  let source = "True != False" in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $Compare.neq(true, false, n0)
          return n0 |}]


let%expect_test _ =
  let source = {|
def f(x, y, z, t):
        return (x and y) or (z and t)
        |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n3
          return n0


      function dummy.f(x, y, z, t):
        b0:
          n0 <- None
          n3 <- LOCAL[x]
          if n3 then jmp b1 else jmp b2

        b1:
          n4 <- LOCAL[y]
          if n4 then jmp b4(n4) else jmp b2

        b2:
          n5 <- LOCAL[z]
          if n5 then jmp b3 else jmp b4(n5)

        b3:
          n6 <- LOCAL[t]
          jmp b4(n6)

        b4(n7):
          return n7 |}]


let%expect_test _ =
  let source = {|
def f(x, y):
  return (x > y)
        |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n3
          return n0


      function dummy.f(x, y):
        b0:
          n0 <- None
          n3 <- LOCAL[x]
          n4 <- LOCAL[y]
          n5 <- $Compare.gt(n3, n4, n0)
          return n5 |}]


let%expect_test _ =
  let source = {|
def f(x, y):
  return (x <= y)
        |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n3
          return n0


      function dummy.f(x, y):
        b0:
          n0 <- None
          n3 <- LOCAL[x]
          n4 <- LOCAL[y]
          n5 <- $Compare.le(n3, n4, n0)
          return n5 |}]


let%expect_test _ =
  let source =
    {|
def is_check(x):
          return x is None

def is_not_check(x):
          return x is not None

def in_check(x, l):
          return x in l

def in_not_check(x, l):
          return not (x in l)
          |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.is_check", n0, n0, n0, n0]
          TOPLEVEL[is_check] <- n3
          n4 <- $MakeFunction["dummy.is_not_check", n0, n0, n0, n0]
          TOPLEVEL[is_not_check] <- n4
          n5 <- $MakeFunction["dummy.in_check", n0, n0, n0, n0]
          TOPLEVEL[in_check] <- n5
          n6 <- $MakeFunction["dummy.in_not_check", n0, n0, n0, n0]
          TOPLEVEL[in_not_check] <- n6
          return n0


      function dummy.in_check(x, l):
        b0:
          n0 <- None
          n3 <- LOCAL[x]
          n4 <- LOCAL[l]
          n5 <- $Compare.in(n3, n4, n0)
          return n5


      function dummy.in_not_check(x, l):
        b0:
          n0 <- None
          n3 <- LOCAL[x]
          n4 <- LOCAL[l]
          n5 <- $Compare.not_in(n3, n4, n0)
          return n5


      function dummy.is_check(x):
        b0:
          n0 <- None
          n3 <- LOCAL[x]
          n4 <- $Compare.is(n3, n0, n0)
          return n4


      function dummy.is_not_check(x):
        b0:
          n0 <- None
          n3 <- LOCAL[x]
          n4 <- $Compare.is_not(n3, n0, n0)
          return n4 |}]


let%expect_test _ =
  let source =
    {|
t = (1, 2, 3) # will be a constant, not a BUILD_TUPLE
def f(x, y, z):
        return (x, y, z) # should be BUILD_TUPLE
|}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          TOPLEVEL[t] <- $BuildTuple(1, 2, 3)
          n3 <- $MakeFunction["dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n3
          return n0


      function dummy.f(x, y, z):
        b0:
          n0 <- None
          n3 <- LOCAL[x]
          n4 <- LOCAL[y]
          n5 <- LOCAL[z]
          n6 <- $BuildTuple(n3, n4, n5)
          return n6 |}]


let%expect_test _ =
  let source =
    {|
l = [1, 2, 3]
print(l)

def build_list():
          return [1, 2, 3]

[x, y, z] = build_list()
|}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $BuildList()
          n4 <- $ListExtend(n3, $BuildTuple(1, 2, 3), n0)
          TOPLEVEL[l] <- n3
          n5 <- TOPLEVEL[print]
          n6 <- TOPLEVEL[l]
          n7 <- $Call(n5, n6, n0)
          n8 <- $MakeFunction["dummy.build_list", n0, n0, n0, n0]
          TOPLEVEL[build_list] <- n8
          n9 <- TOPLEVEL[build_list]
          n10 <- $Call(n9, n0)
          TOPLEVEL[x] <- n10[0]
          TOPLEVEL[y] <- n10[1]
          TOPLEVEL[z] <- n10[2]
          return n0


      function dummy.build_list():
        b0:
          n0 <- None
          n3 <- $BuildList()
          n4 <- $ListExtend(n3, $BuildTuple(1, 2, 3), n0)
          return n3 |}]


let%expect_test _ =
  let source =
    {|
def f(foo, bar):
    with foo() as foo0:
        with bar() as bar0:
            print(bar0)
        print(foo0)

        return 42
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n3
          return n0


      function dummy.f(foo, bar):
        b0:
          n0 <- None
          n3 <- LOCAL[foo]
          n4 <- $Call(n3, n0)
          n5 <- $CallMethod[__enter__](n4, n0)
          LOCAL[foo0] <- n5
          n6 <- LOCAL[bar]
          n7 <- $Call(n6, n0)
          n8 <- $CallMethod[__enter__](n7, n0)
          LOCAL[bar0] <- n8
          n9 <- GLOBAL[print]
          n10 <- LOCAL[bar0]
          n11 <- $Call(n9, n10, n0)
          n12 <- $CallMethod[__exit__](n7, n0)
          jmp b4

        b4:
          n13 <- GLOBAL[print]
          n14 <- LOCAL[foo0]
          n15 <- $Call(n13, n14, n0)
          n16 <- $CallMethod[__exit__](n4, n0)
          return 42 |}]


let%expect_test _ =
  let source = {|
def f():
        pass

(a, b) = f()
|} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n3
          n4 <- TOPLEVEL[f]
          n5 <- $Call(n4, n0)
          TOPLEVEL[a] <- n5[0]
          TOPLEVEL[b] <- n5[1]
          return n0


      function dummy.f():
        b0:
          n0 <- None
          return n0 |}]


let%expect_test _ =
  let source = {|
def f(z, x, y):
        pass

f(0, y=2, x=1)
        |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n3
          n4 <- TOPLEVEL[f]
          n5 <- $Call(n4, 0, 2, 1, $BuildTuple("y", "x"))
          return n0


      function dummy.f(z, x, y):
        b0:
          n0 <- None
          return n0 |}]


let%expect_test _ =
  let source =
    {|
def f(m, a, b, c):
    while (a, b) not in m:
        b -= 1
    while (a, c) not in m:
        c += 1
  |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n3
          return n0


      function dummy.f(m, a, b, c):
        b0:
          n0 <- None
          n3 <- LOCAL[a]
          n4 <- LOCAL[b]
          n5 <- $BuildTuple(n3, n4)
          n6 <- LOCAL[m]
          n7 <- $Compare.not_in(n5, n6, n0)
          if n7 then jmp b1 else jmp b2

        b1:
          n8 <- LOCAL[b]
          n9 <- $Inplace.Subtract(n8, 1, n0)
          LOCAL[b] <- n9
          n10 <- LOCAL[a]
          n11 <- LOCAL[b]
          n12 <- $BuildTuple(n10, n11)
          n13 <- LOCAL[m]
          n14 <- $Compare.not_in(n12, n13, n0)
          if n14 then jmp b1 else jmp b2

        b2:
          n15 <- LOCAL[a]
          n16 <- LOCAL[c]
          n17 <- $BuildTuple(n15, n16)
          n18 <- LOCAL[m]
          n19 <- $Compare.not_in(n17, n18, n0)
          if n19 then jmp b3 else jmp b5

        b3:
          n20 <- LOCAL[c]
          n21 <- $Inplace.Add(n20, 1, n0)
          LOCAL[c] <- n21
          n22 <- LOCAL[a]
          n23 <- LOCAL[c]
          n24 <- $BuildTuple(n22, n23)
          n25 <- LOCAL[m]
          n26 <- $Compare.not_in(n24, n25, n0)
          if n26 then jmp b3 else jmp b4

        b4:
          return n0

        b5:
          return n0 |}]


let%expect_test _ =
  let source =
    {|
def f(name, args):
    return f"foo.{name!r}{name!s}{name!a}"

def test_arguments(x, y, width):
    return f'x={x*y:{width}}'
          |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n3
          n4 <- $MakeFunction["dummy.test_arguments", n0, n0, n0, n0]
          TOPLEVEL[test_arguments] <- n4
          return n0


      function dummy.f(name, args):
        b0:
          n0 <- None
          n3 <- LOCAL[name]
          n4 <- $FormatFn.repr(n3, n0)
          n5 <- $Format(n4, n0, n0)
          n6 <- LOCAL[name]
          n7 <- $FormatFn.str(n6, n0)
          n8 <- $Format(n7, n0, n0)
          n9 <- LOCAL[name]
          n10 <- $FormatFn.ascii(n9, n0)
          n11 <- $Format(n10, n0, n0)
          n12 <- $BuildString("foo.", n5, n8, n11)
          return n12


      function dummy.test_arguments(x, y, width):
        b0:
          n0 <- None
          n3 <- LOCAL[x]
          n4 <- LOCAL[y]
          n5 <- $Binary.Multiply(n3, n4, n0)
          n6 <- LOCAL[width]
          n7 <- $Format(n6, n0, n0)
          n8 <- $Format(n5, n7, n0)
          n9 <- $BuildString("x=", n8)
          return n9 |}]


let%expect_test _ =
  let source =
    {|
def pos(x):
        return +x

def neg(x):
        return -x

def test_not(x):
        return not x

def inv(x):
        return ~x
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.pos", n0, n0, n0, n0]
          TOPLEVEL[pos] <- n3
          n4 <- $MakeFunction["dummy.neg", n0, n0, n0, n0]
          TOPLEVEL[neg] <- n4
          n5 <- $MakeFunction["dummy.test_not", n0, n0, n0, n0]
          TOPLEVEL[test_not] <- n5
          n6 <- $MakeFunction["dummy.inv", n0, n0, n0, n0]
          TOPLEVEL[inv] <- n6
          return n0


      function dummy.inv(x):
        b0:
          n0 <- None
          n3 <- LOCAL[x]
          n4 <- $Unary.Invert(n3, n0)
          return n4


      function dummy.neg(x):
        b0:
          n0 <- None
          n3 <- LOCAL[x]
          n4 <- $Unary.Negative(n3, n0)
          return n4


      function dummy.pos(x):
        b0:
          n0 <- None
          n3 <- LOCAL[x]
          n4 <- $Unary.Positive(n3, n0)
          return n4


      function dummy.test_not(x):
        b0:
          n0 <- None
          n3 <- LOCAL[x]
          n4 <- $Unary.Not(n3, n0)
          return n4 |}]


let%expect_test _ =
  let source =
    {|
gx = 100
def f(ax):
    lx = 1000

    def inner():
        ix = 20
        global gx
        nonlocal lx
        print(gx) # prints 100
        print(ax) # prints 42
        print(lx) # prints 1664
        print(ix) # 20
        gx = 10
        lx = 2
        return lx

    lx = 1664
    return inner

g = f(42)
print(g()) # prints 2
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          GLOBAL[gx] <- 100
          n3 <- $MakeFunction["dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n3
          n4 <- TOPLEVEL[f]
          n5 <- $Call(n4, 42, n0)
          TOPLEVEL[g] <- n5
          n6 <- TOPLEVEL[print]
          n7 <- TOPLEVEL[g]
          n8 <- $Call(n7, n0)
          n9 <- $Call(n6, n8, n0)
          return n0


      function dummy.f(ax):
        b0:
          n0 <- None
          $StoreDeref(1,"lx", 1000)
          n3 <- $LoadClosure(0,"ax")
          n4 <- $LoadClosure(1,"lx")
          n5 <- $BuildTuple(n3, n4)
          n6 <- $MakeFunction["dummy.f.inner", n0, n0, n0, n5]
          LOCAL[inner] <- n6
          $StoreDeref(1,"lx", 1664)
          n7 <- LOCAL[inner]
          return n7


      function dummy.f.inner(ix):
        b0:
          n0 <- None
          LOCAL[ix] <- 20
          n3 <- GLOBAL[print]
          n4 <- GLOBAL[gx]
          n5 <- $Call(n3, n4, n0)
          n6 <- GLOBAL[print]
          n7 <- $LoadDeref(0,"ax")
          n8 <- $Call(n6, n7, n0)
          n9 <- GLOBAL[print]
          n10 <- $LoadDeref(1,"lx")
          n11 <- $Call(n9, n10, n0)
          n12 <- GLOBAL[print]
          n13 <- LOCAL[ix]
          n14 <- $Call(n12, n13, n0)
          GLOBAL[gx] <- 10
          $StoreDeref(1,"lx", 2)
          n15 <- $LoadDeref(1,"lx")
          return n15 |}]


let%expect_test _ =
  let source =
    {|
g = [x + 1 for x in l]
g0 = [x + 2 for x in l]
print(g)
print(g0)

def f(l):
   r = [x + 1 for x in l]
   r0 = [x + 2 for x in l]
   print(r)
   print(r0)
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy._$listcomp", n0, n0, n0, n0]
          n4 <- TOPLEVEL[l]
          n5 <- $GetIter(n4, n0)
          n6 <- $Call(n3, n5, n0)
          TOPLEVEL[g] <- n6
          n7 <- $MakeFunction["dummy._$listcomp1", n0, n0, n0, n0]
          n8 <- TOPLEVEL[l]
          n9 <- $GetIter(n8, n0)
          n10 <- $Call(n7, n9, n0)
          TOPLEVEL[g0] <- n10
          n11 <- TOPLEVEL[print]
          n12 <- TOPLEVEL[g]
          n13 <- $Call(n11, n12, n0)
          n14 <- TOPLEVEL[print]
          n15 <- TOPLEVEL[g0]
          n16 <- $Call(n14, n15, n0)
          n17 <- $MakeFunction["dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n17
          return n0


      function dummy._$listcomp(.0):
        b0:
          n0 <- None
          n3 <- $BuildList()
          n4 <- LOCAL[.0]
          jmp b1

        b1:
          n5 <- $NextIter(n4, n0)
          n6 <- $HasNextIter(n4, n0)
          if n6 then jmp b2 else jmp b3

        b2:
          LOCAL[x] <- n5
          n7 <- LOCAL[x]
          n8 <- $Binary.Add(n7, 1, n0)
          n9 <- $ListAppend(n3, n8, n0)
          jmp b1

        b3:
          return n3


      function dummy._$listcomp1(.0):
        b0:
          n0 <- None
          n3 <- $BuildList()
          n4 <- LOCAL[.0]
          jmp b1

        b1:
          n5 <- $NextIter(n4, n0)
          n6 <- $HasNextIter(n4, n0)
          if n6 then jmp b2 else jmp b3

        b2:
          LOCAL[x] <- n5
          n7 <- LOCAL[x]
          n8 <- $Binary.Add(n7, 2, n0)
          n9 <- $ListAppend(n3, n8, n0)
          jmp b1

        b3:
          return n3


      function dummy.f(l):
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.f._$listcomp", n0, n0, n0, n0]
          n4 <- LOCAL[l]
          n5 <- $GetIter(n4, n0)
          n6 <- $Call(n3, n5, n0)
          LOCAL[r] <- n6
          n7 <- $MakeFunction["dummy.f._$listcomp1", n0, n0, n0, n0]
          n8 <- LOCAL[l]
          n9 <- $GetIter(n8, n0)
          n10 <- $Call(n7, n9, n0)
          LOCAL[r0] <- n10
          n11 <- GLOBAL[print]
          n12 <- LOCAL[r]
          n13 <- $Call(n11, n12, n0)
          n14 <- GLOBAL[print]
          n15 <- LOCAL[r0]
          n16 <- $Call(n14, n15, n0)
          return n0


      function dummy.f._$listcomp(.0):
        b0:
          n0 <- None
          n3 <- $BuildList()
          n4 <- LOCAL[.0]
          jmp b1

        b1:
          n5 <- $NextIter(n4, n0)
          n6 <- $HasNextIter(n4, n0)
          if n6 then jmp b2 else jmp b3

        b2:
          LOCAL[x] <- n5
          n7 <- LOCAL[x]
          n8 <- $Binary.Add(n7, 1, n0)
          n9 <- $ListAppend(n3, n8, n0)
          jmp b1

        b3:
          return n3


      function dummy.f._$listcomp1(.0):
        b0:
          n0 <- None
          n3 <- $BuildList()
          n4 <- LOCAL[.0]
          jmp b1

        b1:
          n5 <- $NextIter(n4, n0)
          n6 <- $HasNextIter(n4, n0)
          if n6 then jmp b2 else jmp b3

        b2:
          LOCAL[x] <- n5
          n7 <- LOCAL[x]
          n8 <- $Binary.Add(n7, 2, n0)
          n9 <- $ListAppend(n3, n8, n0)
          jmp b1

        b3:
          return n3 |}]


let%expect_test _ =
  let source =
    {|
def f(l):
  r = {x + 1 for x in l }
  return r


def g(l):
  squared_dict = {num: num ** 2 for num in l}
  return r
          |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n3
          n4 <- $MakeFunction["dummy.g", n0, n0, n0, n0]
          TOPLEVEL[g] <- n4
          return n0


      function dummy.f(l):
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.f._$setcomp", n0, n0, n0, n0]
          n4 <- LOCAL[l]
          n5 <- $GetIter(n4, n0)
          n6 <- $Call(n3, n5, n0)
          LOCAL[r] <- n6
          n7 <- LOCAL[r]
          return n7


      function dummy.f._$setcomp(.0):
        b0:
          n0 <- None
          n3 <- $BuildSet()
          n4 <- LOCAL[.0]
          jmp b1

        b1:
          n5 <- $NextIter(n4, n0)
          n6 <- $HasNextIter(n4, n0)
          if n6 then jmp b2 else jmp b3

        b2:
          LOCAL[x] <- n5
          n7 <- LOCAL[x]
          n8 <- $Binary.Add(n7, 1, n0)
          n9 <- $SetAdd(n3, n8, n0)
          jmp b1

        b3:
          return n3


      function dummy.g(l):
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.g._$dictcomp", n0, n0, n0, n0]
          n4 <- LOCAL[l]
          n5 <- $GetIter(n4, n0)
          n6 <- $Call(n3, n5, n0)
          LOCAL[squared_dict] <- n6
          n7 <- GLOBAL[r]
          return n7


      function dummy.g._$dictcomp(.0):
        b0:
          n0 <- None
          n3 <- $BuildMap()
          n4 <- LOCAL[.0]
          jmp b1

        b1:
          n5 <- $NextIter(n4, n0)
          n6 <- $HasNextIter(n4, n0)
          if n6 then jmp b2 else jmp b3

        b2:
          LOCAL[num] <- n5
          n7 <- LOCAL[num]
          n8 <- LOCAL[num]
          n9 <- $Binary.Power(n8, 2, n0)
          n10 <- $DictSetItem(n3, n7, n9, n0)
          jmp b1

        b3:
          return n3 |}]


let%expect_test _ =
  let source =
    {|
async def f():
  return True

async def g():
  if await f():
    print(0)
  else:
    print(1)
          |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n3
          n4 <- $MakeFunction["dummy.g", n0, n0, n0, n0]
          TOPLEVEL[g] <- n4
          return n0


      async function dummy.f():
        b0:
          n0 <- None
          $GenStartCoroutine()
          return true


      async function dummy.g():
        b0:
          n0 <- None
          $GenStartCoroutine()
          n3 <- GLOBAL[f]
          n4 <- $Call(n3, n0)
          n5 <- $GetAwaitable(n4, n0)
          n6 <- $YieldFrom(n5, n0, n0)
          if n5 then jmp b1 else jmp b2

        b1:
          n9 <- GLOBAL[print]
          n10 <- $Call(n9, 0, n0)
          return n0

        b2:
          n7 <- GLOBAL[print]
          n8 <- $Call(n7, 1, n0)
          return n0 |}]


let%expect_test _ =
  let source = {|
def m(self, x, y, test):
    return foo(self, x if test else y)
|} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.m", n0, n0, n0, n0]
          TOPLEVEL[m] <- n3
          return n0


      function dummy.m(self, x, y, test):
        b0:
          n0 <- None
          n3 <- GLOBAL[foo]
          n4 <- LOCAL[self]
          n5 <- LOCAL[test]
          if n5 then jmp b1 else jmp b2

        b1:
          n8 <- LOCAL[x]
          n9 <- $Call(n3, n4, n8, n0)
          return n9

        b2:
          n6 <- LOCAL[y]
          n7 <- $Call(n3, n4, n6, n0)
          return n7 |}]


let%expect_test _ =
  let source = {|
def m(self, x, y, test):
    return self.foo(x if test else y)
|} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.m", n0, n0, n0, n0]
          TOPLEVEL[m] <- n3
          return n0


      function dummy.m(self, x, y, test):
        b0:
          n0 <- None
          n3 <- LOCAL[self]
          n4 <- LOCAL[test]
          if n4 then jmp b1 else jmp b2

        b1:
          n7 <- LOCAL[x]
          n8 <- $CallMethod[foo](n3, n7, n0)
          return n8

        b2:
          n5 <- LOCAL[y]
          n6 <- $CallMethod[foo](n3, n5, n0)
          return n6 |}]


let%expect_test _ =
  let source = {|
def m(x, y, test):
    return (x if test else y).foo()
|} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.m", n0, n0, n0, n0]
          TOPLEVEL[m] <- n3
          return n0


      function dummy.m(x, y, test):
        b0:
          n0 <- None
          n3 <- LOCAL[test]
          if n3 then jmp b1 else jmp b2

        b1:
          n6 <- LOCAL[x]
          n7 <- $CallMethod[foo](n6, n0)
          return n7

        b2:
          n4 <- LOCAL[y]
          n5 <- $CallMethod[foo](n4, n0)
          return n5 |}]


let%expect_test _ =
  let source =
    {|
class C:
    def foo(self):
        print('I am foo')

o = C()
o.foo()
o.foo = lambda : print('I am not foo')
o.foo()
#I am foo
#I am not foo
|}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.C", n0, n0, n0, n0]
          n4 <- $BuildClass(n3, "C", n0)
          TOPLEVEL[C] <- n4
          n5 <- TOPLEVEL[C]
          n6 <- $Call(n5, n0)
          TOPLEVEL[o] <- n6
          n7 <- TOPLEVEL[o]
          n8 <- $CallMethod[foo](n7, n0)
          n9 <- $MakeFunction["dummy._$lambda", n0, n0, n0, n0]
          n10 <- TOPLEVEL[o]
          n10.foo <- n9
          n11 <- TOPLEVEL[o]
          n12 <- $CallMethod[foo](n11, n0)
          return n0


      function dummy.C():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "C"
          n4 <- $MakeFunction["dummy.C.foo", n0, n0, n0, n0]
          TOPLEVEL[foo] <- n4
          return n0


      function dummy.C.foo(self):
        b0:
          n0 <- None
          n3 <- GLOBAL[print]
          n4 <- $Call(n3, "I am foo", n0)
          return n0


      function dummy._$lambda():
        b0:
          n0 <- None
          n3 <- GLOBAL[print]
          n4 <- $Call(n3, "I am not foo", n0)
          return n4 |}]


let%expect_test _ =
  let source = {|
res = dict.attr(0 if not False else 1)
|} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[dict]
          n4 <- $CallMethod[attr](n3, 0, n0)
          TOPLEVEL[res] <- n4
          return n0 |}]


let%expect_test _ =
  let source =
    {|
async def foo():
    for i in range(num):
        async with await read() as f:
            return
|}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.foo", n0, n0, n0, n0]
          TOPLEVEL[foo] <- n3
          return n0


      async function dummy.foo(i, f):
        b0:
          n0 <- None
          $GenStartCoroutine()
          n3 <- GLOBAL[range]
          n4 <- GLOBAL[num]
          n5 <- $Call(n3, n4, n0)
          n6 <- $GetIter(n5, n0)
          jmp b1

        b1:
          n7 <- $NextIter(n6, n0)
          n8 <- $HasNextIter(n6, n0)
          if n8 then jmp b2 else jmp b6

        b2:
          LOCAL[i] <- n7
          n9 <- GLOBAL[read]
          n10 <- $Call(n9, n0)
          n11 <- $GetAwaitable(n10, n0)
          n12 <- $YieldFrom(n11, n0, n0)
          n13 <- $CallMethod[__enter__](n11, n0)
          n14 <- $GetAwaitable(n13, n0)
          n15 <- $YieldFrom(n14, n0, n0)
          LOCAL[f] <- n14
          n16 <- $CallMethod[__exit__](n11, n0)
          n17 <- $GetAwaitable(n16, n0)
          n18 <- $YieldFrom(n17, n0, n0)
          return n0

        b6:
          return n0 |}]


(* the two examples below show that with Python 3.10, the size of the opstack may
   trigger a duplication of instructions (at bytecode level). *)
let%expect_test _ =
  let source =
    {|
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
cloned_call()
|}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[make]
          n4 <- $BuildMap()
          n5 <- TOPLEVEL[e1]
          n6 <- $DictSetItem(n4, "x1", n5, n0)
          n7 <- TOPLEVEL[e2]
          n8 <- $DictSetItem(n4, "x2", n7, n0)
          n9 <- TOPLEVEL[e3]
          n10 <- $DictSetItem(n4, "x3", n9, n0)
          n11 <- TOPLEVEL[e4]
          n12 <- $DictSetItem(n4, "x4", n11, n0)
          n13 <- TOPLEVEL[e5]
          n14 <- $DictSetItem(n4, "x5", n13, n0)
          n15 <- TOPLEVEL[e6]
          n16 <- $DictSetItem(n4, "x6", n15, n0)
          n17 <- TOPLEVEL[e7]
          n18 <- $DictSetItem(n4, "x7", n17, n0)
          n19 <- TOPLEVEL[e8]
          n20 <- $DictSetItem(n4, "x8", n19, n0)
          n21 <- TOPLEVEL[b]
          if n21 then jmp b1 else jmp b2

        b1:
          n41 <- TOPLEVEL[v_true]
          jmp b3

        b2:
          n22 <- TOPLEVEL[v_false]
          n23 <- $DictSetItem(n4, "x_cond", n22, n0)
          n24 <- TOPLEVEL[e9]
          n25 <- $DictSetItem(n4, "x9", n24, n0)
          n26 <- TOPLEVEL[e10]
          n27 <- $DictSetItem(n4, "x10", n26, n0)
          n28 <- TOPLEVEL[e11]
          n29 <- $DictSetItem(n4, "x11", n28, n0)
          n30 <- TOPLEVEL[e12]
          n31 <- $DictSetItem(n4, "x12", n30, n0)
          n32 <- TOPLEVEL[e13]
          n33 <- $DictSetItem(n4, "x13", n32, n0)
          n34 <- TOPLEVEL[e14]
          n35 <- $DictSetItem(n4, "x14", n34, n0)
          n36 <- TOPLEVEL[e15]
          n37 <- $DictSetItem(n4, "x15", n36, n0)
          n38 <- $CallFunctionEx(n3, $BuildTuple(), n4, n0)
          TOPLEVEL[l] <- n38
          n39 <- TOPLEVEL[cloned_call]
          n40 <- $Call(n39, n0)
          return n0

        b3:
          n42 <- $DictSetItem(n4, "x_cond", n41, n0)
          n43 <- TOPLEVEL[e9]
          n44 <- $DictSetItem(n4, "x9", n43, n0)
          n45 <- TOPLEVEL[e10]
          n46 <- $DictSetItem(n4, "x10", n45, n0)
          n47 <- TOPLEVEL[e11]
          n48 <- $DictSetItem(n4, "x11", n47, n0)
          n49 <- TOPLEVEL[e12]
          n50 <- $DictSetItem(n4, "x12", n49, n0)
          n51 <- TOPLEVEL[e13]
          n52 <- $DictSetItem(n4, "x13", n51, n0)
          n53 <- TOPLEVEL[e14]
          n54 <- $DictSetItem(n4, "x14", n53, n0)
          n55 <- TOPLEVEL[e15]
          n56 <- $DictSetItem(n4, "x15", n55, n0)
          n57 <- $CallFunctionEx(n3, $BuildTuple(), n4, n0)
          TOPLEVEL[l] <- n57
          n58 <- TOPLEVEL[cloned_call]
          n59 <- $Call(n58, n0)
          return n0 |}]


let%expect_test _ =
  let source =
    {|
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
)
not_cloned_call()
|}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[make]
          n4 <- TOPLEVEL[e1]
          n5 <- TOPLEVEL[e2]
          n6 <- TOPLEVEL[e3]
          n7 <- TOPLEVEL[e4]
          n8 <- TOPLEVEL[e5]
          n9 <- TOPLEVEL[e6]
          n10 <- TOPLEVEL[e7]
          n11 <- TOPLEVEL[e8]
          n12 <- TOPLEVEL[b]
          if n12 then jmp b1 else jmp b2

        b1:
          n14 <- TOPLEVEL[v_true]
          jmp b3(n14)

        b2:
          n13 <- TOPLEVEL[v_false]
          jmp b3(n13)

        b3(n15):
          n16 <- TOPLEVEL[e9]
          n17 <- TOPLEVEL[e10]
          n18 <- TOPLEVEL[e11]
          n19 <- TOPLEVEL[e12]
          n20 <- TOPLEVEL[e13]
          n21 <- TOPLEVEL[e14]
          n22 <- $Call(n3, n4, n5, n6, n7, n8, n9, n10, n11, n15, n16, n17, n18, n19, n20, n21, $BuildTuple("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x_cond", "x9", "x10", "x11", "x12", "x13", "x14"))
          TOPLEVEL[l] <- n22
          n23 <- TOPLEVEL[not_cloned_call]
          n24 <- $Call(n23, n0)
          return n0 |}]
