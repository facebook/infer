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
          n0 <- $MakeFunction["my_fun", "dummy.my_fun", None, None, None, None]
          TOPLEVEL[my_fun] <- n0
          TOPLEVEL[a] <- 10
          n1 <- TOPLEVEL[my_fun]
          n2 <- TOPLEVEL[a]
          n3 <- $Call(n1, 42, n2, None)
          TOPLEVEL[z] <- n3
          n4 <- TOPLEVEL[print]
          n5 <- TOPLEVEL[z]
          n6 <- $Call(n4, n5, None)
          return None


      function dummy.my_fun(x, y):
        b0:
          n0 <- GLOBAL[print]
          n1 <- LOCAL[x]
          n2 <- $Call(n0, n1, None)
          n3 <- GLOBAL[print]
          n4 <- LOCAL[y]
          n5 <- $Call(n3, n4, None)
          n6 <- LOCAL[x]
          n7 <- LOCAL[y]
          n8 <- $Binary.Add(n6, n7, None)
          LOCAL[z] <- n8
          n9 <- LOCAL[z]
          return n9 |}]


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
          n0 <- $MakeFunction["update_global", "dummy.update_global", None, None, None, None]
          TOPLEVEL[update_global] <- n0
          GLOBAL[z] <- 0
          n1 <- TOPLEVEL[update_global]
          n2 <- $Call(n1, None)
          n3 <- TOPLEVEL[print]
          n4 <- GLOBAL[z]
          n5 <- $Call(n3, n4, None)
          return None


      function dummy.update_global():
        b0:
          n0 <- GLOBAL[z]
          n1 <- $Binary.Add(n0, 1, None)
          GLOBAL[z] <- n1
          return None |}]


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
          n0 <- $MakeFunction["coin", "dummy.coin", None, None, None, None]
          TOPLEVEL[coin] <- n0
          n1 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n1
          return None


      function dummy.coin():
        b0:
          return false


      function dummy.f(x, y):
        b0:
          n0 <- GLOBAL[coin]
          n1 <- $Call(n0, None)
          if n1 then jmp b1 else jmp b2

        b1:
          n3 <- LOCAL[x]
          return n3

        b2:
          n2 <- LOCAL[y]
          return n2 |}]


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
          n0 <- $MakeFunction["coin", "dummy.coin", None, None, None, None]
          TOPLEVEL[coin] <- n0
          n1 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n1
          return None


      function dummy.coin():
        b0:
          return false


      function dummy.f(x, y):
        b0:
          LOCAL[z] <- 0
          n0 <- GLOBAL[coin]
          n1 <- $Call(n0, None)
          if n1 then jmp b1 else jmp b2

        b1:
          n3 <- LOCAL[x]
          LOCAL[z] <- n3
          jmp b3

        b2:
          n2 <- LOCAL[y]
          LOCAL[z] <- n2
          jmp b3

        b3:
          n4 <- LOCAL[z]
          return n4 |}]


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
          n0 <- $MakeFunction["coin", "dummy.coin", None, None, None, None]
          TOPLEVEL[coin] <- n0
          n1 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n1
          return None


      function dummy.coin():
        b0:
          return false


      function dummy.f(x, y):
        b0:
          LOCAL[z] <- 0
          n0 <- GLOBAL[coin]
          n1 <- $Call(n0, None)
          if n1 then jmp b1 else jmp b5

        b1:
          n7 <- GLOBAL[coin]
          n8 <- $Call(n7, None)
          if n8 then jmp b2 else jmp b3

        b2:
          n9 <- LOCAL[x]
          LOCAL[z] <- n9
          jmp b4

        b3:
          return 1664

        b4:
          n10 <- LOCAL[z]
          n11 <- $Binary.Add(n10, 1, None)
          LOCAL[z] <- n11
          jmp b8

        b5:
          n2 <- LOCAL[z]
          n3 <- $Binary.Add(n2, 1, None)
          LOCAL[z] <- n3
          n4 <- GLOBAL[coin]
          n5 <- $Call(n4, None)
          if n5 then jmp b6 else jmp b7

        b6:
          return 42

        b7:
          n6 <- LOCAL[y]
          LOCAL[z] <- n6
          jmp b8

        b8:
          n12 <- LOCAL[z]
          return n12 |}]


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
          n0 <- $MakeFunction["foo", "dummy.foo", None, None, None, None]
          TOPLEVEL[foo] <- n0
          n1 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n1
          return None


      function dummy.f(x):
        b0:
          n0 <- GLOBAL[foo]
          n1 <- LOCAL[x]
          if n1 then jmp b1 else jmp b2

        b1:
          jmp b3(1)

        b2:
          jmp b3(0)

        b3(n2):
          n3 <- $Call(n0, n2, None)
          return None


      function dummy.foo(x):
        b0:
          return None |}]


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
          n0 <- TOPLEVEL[range]
          n1 <- $Call(n0, 10, None)
          n2 <- $GetIter(n1, None)
          jmp b1

        b1:
          n3 <- $NextIter(n2, None)
          n4 <- $HasNextIter(n2, None)
          if n4 then jmp b2 else jmp b3

        b2:
          TOPLEVEL[x] <- n3
          n5 <- TOPLEVEL[print]
          n6 <- TOPLEVEL[x]
          n7 <- $Call(n5, n6, None)
          jmp b1

        b3:
          return None |}]


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
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          return None


      function dummy.f(x, y, l, bar, toto):
        b0:
          n0 <- LOCAL[l]
          n1 <- $GetIter(n0, None)
          jmp b1

        b1:
          n2 <- $NextIter(n1, None)
          n3 <- $HasNextIter(n1, None)
          if n3 then jmp b2 else jmp b11

        b10:
          jmp b1

        b11:
          return None

        b2:
          LOCAL[x] <- n2
          n4 <- LOCAL[bar]
          n5 <- $Call(n4, None)
          n6 <- $CallMethod[__enter__](n5, None)
          n7 <- LOCAL[toto]
          n8 <- $Call(n7, None)
          n9 <- $CallMethod[__enter__](n8, None)
          LOCAL[obj] <- n9
          n10 <- LOCAL[y]
          if n10 then jmp b3 else jmp b6

        b3:
          jmp b4

        b4:
          n15 <- $CallMethod[__enter__](n8, None, None, None, None)
          jmp b5

        b5:
          n16 <- $CallMethod[__enter__](n5, None, None, None, None)
          jmp b1

        b6:
          n11 <- GLOBAL[print]
          n12 <- $Call(n11, "nop", None)
          jmp b7

        b7:
          n13 <- $CallMethod[__enter__](n8, None, None, None, None)
          jmp b8

        b8:
          jmp b9

        b9:
          n14 <- $CallMethod[__enter__](n5, None, None, None, None)
          jmp b10 |}]


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
          n0 <- TOPLEVEL[print]
          n1 <- $Call(n0, 42, None)
          n2 <- $MakeFunction["print", "dummy.print", None, None, None, None]
          TOPLEVEL[print] <- n2
          n3 <- TOPLEVEL[print]
          n4 <- $Call(n3, 42, None)
          n5 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n5
          return None


      function dummy.f(x):
        b0:
          n0 <- GLOBAL[print]
          n1 <- LOCAL[x]
          n2 <- $Call(n0, n1, None)
          return None


      function dummy.print(x):
        b0:
          n0 <- LOCAL[x]
          return n0 |}]


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
          n0 <- TOPLEVEL[int]
          n1 <- TOPLEVEL[float]
          n2 <- $BuildConstKeyMap($BuildTuple("x", "z"), n0, n1, None)
          n3 <- $MakeFunction["f0", "dummy.f0", None, None, n2, None]
          TOPLEVEL[f0] <- n3
          n4 <- TOPLEVEL[str]
          n5 <- TOPLEVEL[bool]
          n6 <- $BuildConstKeyMap($BuildTuple("y", "return"), n4, n5, None)
          n7 <- $MakeFunction["f1", "dummy.f1", None, None, n6, None]
          TOPLEVEL[f1] <- n7
          return None


      function dummy.f0(x, y, z):
        b0:
          return None


      function dummy.f1(x, y):
        b0:
          return None |}]


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
          n0 <- TOPLEVEL[int]
          n1 <- $BuildConstKeyMap($BuildTuple("x"), n0, None)
          n2 <- $MakeFunction["expect_int", "dummy.expect_int", None, None, n1, None]
          TOPLEVEL[expect_int] <- n2
          n3 <- TOPLEVEL[int]
          n4 <- $BuildConstKeyMap($BuildTuple("return"), n3, None)
          n5 <- $MakeFunction["get", "dummy.get", None, None, n4, None]
          TOPLEVEL[get] <- n5
          n6 <- TOPLEVEL[expect_int]
          n7 <- TOPLEVEL[get]
          n8 <- $Call(n7, None)
          n9 <- $Call(n6, n8, None)
          return None


      function dummy.expect_int(x):
        b0:
          return None


      function dummy.get():
        b0:
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
          n0 <- TOPLEVEL[object]
          n1 <- $BuildConstKeyMap($BuildTuple("x", "return"), n0, None, None)
          n2 <- $MakeFunction["expect", "dummy.expect", None, None, n1, None]
          TOPLEVEL[expect] <- n2
          n3 <- TOPLEVEL[int]
          n4 <- $BuildConstKeyMap($BuildTuple("return"), n3, None)
          n5 <- $MakeFunction["get", "dummy.get", None, None, n4, None]
          TOPLEVEL[get] <- n5
          n6 <- TOPLEVEL[expect]
          n7 <- TOPLEVEL[get]
          n8 <- $Call(n7, None)
          n9 <- $Call(n6, n8, None)
          return None


      function dummy.expect(x):
        b0:
          return None


      function dummy.get():
        b0:
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
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          return None


      function dummy.f(x, y):
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[y]
          n2 <- $Compare.eq(n0, n1, None)
          return n2 |}]


let%expect_test _ =
  let source = "True != False" in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $Compare.neq(true, false, None)
          return None |}]


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
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          return None


      function dummy.f(x, y, z, t):
        b0:
          n0 <- LOCAL[x]
          if n0 then jmp b1 else jmp b2

        b1:
          n1 <- LOCAL[y]
          if n1 then jmp b4(n1) else jmp b2

        b2:
          n2 <- LOCAL[z]
          if n2 then jmp b3 else jmp b4(n2)

        b3:
          n3 <- LOCAL[t]
          jmp b4(n3)

        b4(n4):
          return n4 |}]


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
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          return None


      function dummy.f(x, y):
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[y]
          n2 <- $Compare.gt(n0, n1, None)
          return n2 |}]


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
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          return None


      function dummy.f(x, y):
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[y]
          n2 <- $Compare.le(n0, n1, None)
          return n2 |}]


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
          n0 <- $MakeFunction["is_check", "dummy.is_check", None, None, None, None]
          TOPLEVEL[is_check] <- n0
          n1 <- $MakeFunction["is_not_check", "dummy.is_not_check", None, None, None, None]
          TOPLEVEL[is_not_check] <- n1
          n2 <- $MakeFunction["in_check", "dummy.in_check", None, None, None, None]
          TOPLEVEL[in_check] <- n2
          n3 <- $MakeFunction["in_not_check", "dummy.in_not_check", None, None, None, None]
          TOPLEVEL[in_not_check] <- n3
          return None


      function dummy.in_check(x, l):
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[l]
          n2 <- $Compare.in(n0, n1, None)
          return n2


      function dummy.in_not_check(x, l):
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[l]
          n2 <- $Compare.not_in(n0, n1, None)
          return n2


      function dummy.is_check(x):
        b0:
          n0 <- LOCAL[x]
          n1 <- $Compare.is(n0, None, None)
          return n1


      function dummy.is_not_check(x):
        b0:
          n0 <- LOCAL[x]
          n1 <- $Compare.is_not(n0, None, None)
          return n1 |}]


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
          TOPLEVEL[t] <- $BuildTuple(1, 2, 3)
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          return None


      function dummy.f(x, y, z):
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[y]
          n2 <- LOCAL[z]
          return $BuildTuple(n0, n1, n2) |}]


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
          TOPLEVEL[l] <- $BuildList(1, 2, 3)
          n0 <- TOPLEVEL[print]
          n1 <- TOPLEVEL[l]
          n2 <- $Call(n0, n1, None)
          n3 <- $MakeFunction["build_list", "dummy.build_list", None, None, None, None]
          TOPLEVEL[build_list] <- n3
          n4 <- TOPLEVEL[build_list]
          n5 <- $Call(n4, None)
          TOPLEVEL[x] <- n5[0]
          TOPLEVEL[y] <- n5[1]
          TOPLEVEL[z] <- n5[2]
          return None


      function dummy.build_list():
        b0:
          return $BuildList(1, 2, 3) |}]


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
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          return None


      function dummy.f(foo, bar):
        b0:
          n0 <- LOCAL[foo]
          n1 <- $Call(n0, None)
          n2 <- $CallMethod[__enter__](n1, None)
          LOCAL[foo0] <- n2
          n3 <- LOCAL[bar]
          n4 <- $Call(n3, None)
          n5 <- $CallMethod[__enter__](n4, None)
          LOCAL[bar0] <- n5
          n6 <- GLOBAL[print]
          n7 <- LOCAL[bar0]
          n8 <- $Call(n6, n7, None)
          jmp b1

        b1:
          n9 <- $CallMethod[__enter__](n4, None, None, None, None)
          jmp b2

        b2:
          n10 <- GLOBAL[print]
          n11 <- LOCAL[foo0]
          n12 <- $Call(n10, n11, None)
          jmp b3

        b3:
          n13 <- $CallMethod[__enter__](n1, None, None, None, None)
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
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          n1 <- TOPLEVEL[f]
          n2 <- $Call(n1, None)
          TOPLEVEL[a] <- n2[0]
          TOPLEVEL[b] <- n2[1]
          return None


      function dummy.f():
        b0:
          return None |}]


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
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          n1 <- TOPLEVEL[f]
          n2 <- $Call(n1, 0, 2, 1, $BuildTuple("y", "x"))
          return None


      function dummy.f(z, x, y):
        b0:
          return None |}]


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
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          return None


      function dummy.f(m, a, b, c):
        b0:
          n0 <- LOCAL[a]
          n1 <- LOCAL[b]
          n2 <- LOCAL[m]
          n3 <- $Compare.not_in($BuildTuple(n0, n1), n2, None)
          if n3 then jmp b1 else jmp b2

        b1:
          n10 <- LOCAL[b]
          n11 <- $Inplace.Subtract(n10, 1, None)
          LOCAL[b] <- n11
          jmp b0

        b2:
          n4 <- LOCAL[a]
          n5 <- LOCAL[c]
          n6 <- LOCAL[m]
          n7 <- $Compare.not_in($BuildTuple(n4, n5), n6, None)
          if n7 then jmp b3 else jmp b4

        b3:
          n8 <- LOCAL[c]
          n9 <- $Inplace.Add(n8, 1, None)
          LOCAL[c] <- n9
          jmp b2

        b4:
          return None |}]


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
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          n1 <- $MakeFunction["test_arguments", "dummy.test_arguments", None, None, None, None]
          TOPLEVEL[test_arguments] <- n1
          return None


      function dummy.f(name, args):
        b0:
          n0 <- LOCAL[name]
          n1 <- $FormatFn.repr(n0, None)
          n2 <- $Format(n1, None, None)
          n3 <- LOCAL[name]
          n4 <- $FormatFn.str(n3, None)
          n5 <- $Format(n4, None, None)
          n6 <- LOCAL[name]
          n7 <- $FormatFn.ascii(n6, None)
          n8 <- $Format(n7, None, None)
          return $BuildString("foo.", n2, n5, n8)


      function dummy.test_arguments(x, y, width):
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[y]
          n2 <- $Binary.Multiply(n0, n1, None)
          n3 <- LOCAL[width]
          n4 <- $Format(n3, None, None)
          n5 <- $Format(n2, n4, None)
          return $BuildString("x=", n5) |}]


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
          n0 <- $MakeFunction["pos", "dummy.pos", None, None, None, None]
          TOPLEVEL[pos] <- n0
          n1 <- $MakeFunction["neg", "dummy.neg", None, None, None, None]
          TOPLEVEL[neg] <- n1
          n2 <- $MakeFunction["test_not", "dummy.test_not", None, None, None, None]
          TOPLEVEL[test_not] <- n2
          n3 <- $MakeFunction["inv", "dummy.inv", None, None, None, None]
          TOPLEVEL[inv] <- n3
          return None


      function dummy.inv(x):
        b0:
          n0 <- LOCAL[x]
          n1 <- $Unary.Invert(n0, None)
          return n1


      function dummy.neg(x):
        b0:
          n0 <- LOCAL[x]
          n1 <- $Unary.Negative(n0, None)
          return n1


      function dummy.pos(x):
        b0:
          n0 <- LOCAL[x]
          n1 <- $Unary.Positive(n0, None)
          return n1


      function dummy.test_not(x):
        b0:
          n0 <- LOCAL[x]
          n1 <- $Unary.Not(n0, None)
          return n1 |}]


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
          GLOBAL[gx] <- 100
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          n1 <- TOPLEVEL[f]
          n2 <- $Call(n1, 42, None)
          TOPLEVEL[g] <- n2
          n3 <- TOPLEVEL[print]
          n4 <- TOPLEVEL[g]
          n5 <- $Call(n4, None)
          n6 <- $Call(n3, n5, None)
          return None


      function dummy.f(ax):
        b0:
          $StoreDeref(1,"lx", 1000)
          n0 <- $LoadClosure(0,"ax")
          n1 <- $LoadClosure(1,"lx")
          n2 <- $MakeFunction["inner", "dummy.f.inner", None, None, None, $BuildTuple(n0, n1)]
          LOCAL[inner] <- n2
          $StoreDeref(1,"lx", 1664)
          n3 <- LOCAL[inner]
          return n3


      function dummy.f.inner(ix):
        b0:
          LOCAL[ix] <- 20
          n0 <- GLOBAL[print]
          n1 <- GLOBAL[gx]
          n2 <- $Call(n0, n1, None)
          n3 <- GLOBAL[print]
          n4 <- $LoadDeref(0,"ax")
          n5 <- $Call(n3, n4, None)
          n6 <- GLOBAL[print]
          n7 <- $LoadDeref(1,"lx")
          n8 <- $Call(n6, n7, None)
          n9 <- GLOBAL[print]
          n10 <- LOCAL[ix]
          n11 <- $Call(n9, n10, None)
          GLOBAL[gx] <- 10
          $StoreDeref(1,"lx", 2)
          n12 <- $LoadDeref(1,"lx")
          return n12 |}]


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
          n0 <- $MakeFunction["_$listcomp", "dummy._$listcomp", None, None, None, None]
          n1 <- TOPLEVEL[l]
          n2 <- $GetIter(n1, None)
          n3 <- $Call(n0, n2, None)
          TOPLEVEL[g] <- n3
          n4 <- $MakeFunction["_$listcomp", "dummy._$listcomp", None, None, None, None]
          n5 <- TOPLEVEL[l]
          n6 <- $GetIter(n5, None)
          n7 <- $Call(n4, n6, None)
          TOPLEVEL[g0] <- n7
          n8 <- TOPLEVEL[print]
          n9 <- TOPLEVEL[g]
          n10 <- $Call(n8, n9, None)
          n11 <- TOPLEVEL[print]
          n12 <- TOPLEVEL[g0]
          n13 <- $Call(n11, n12, None)
          n14 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n14
          return None


      function dummy._$listcomp(.0):
        b0:
          n0 <- LOCAL[.0]
          jmp b1

        b1:
          n1 <- $NextIter(n0, None)
          n2 <- $HasNextIter(n0, None)
          if n2 then jmp b2 else jmp b3

        b2:
          LOCAL[x] <- n1
          n3 <- LOCAL[x]
          n4 <- $Binary.Add(n3, 2, None)
          n5 <- $ListAppend($BuildList(), n4, None)
          jmp b1

        b3:
          return $BuildList()


      function dummy.f._$listcomp(.0):
        b0:
          n0 <- LOCAL[.0]
          jmp b1

        b1:
          n1 <- $NextIter(n0, None)
          n2 <- $HasNextIter(n0, None)
          if n2 then jmp b2 else jmp b3

        b2:
          LOCAL[x] <- n1
          n3 <- LOCAL[x]
          n4 <- $Binary.Add(n3, 2, None)
          n5 <- $ListAppend($BuildList(), n4, None)
          jmp b1

        b3:
          return $BuildList()


      function dummy.f(l):
        b0:
          n0 <- $MakeFunction["_$listcomp", "dummy.f._$listcomp", None, None, None, None]
          n1 <- LOCAL[l]
          n2 <- $GetIter(n1, None)
          n3 <- $Call(n0, n2, None)
          LOCAL[r] <- n3
          n4 <- $MakeFunction["_$listcomp", "dummy.f._$listcomp", None, None, None, None]
          n5 <- LOCAL[l]
          n6 <- $GetIter(n5, None)
          n7 <- $Call(n4, n6, None)
          LOCAL[r0] <- n7
          n8 <- GLOBAL[print]
          n9 <- LOCAL[r]
          n10 <- $Call(n8, n9, None)
          n11 <- GLOBAL[print]
          n12 <- LOCAL[r0]
          n13 <- $Call(n11, n12, None)
          return None |}]


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
    {xxx|
    module dummy:

      function toplevel():
        b0:
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          n1 <- $MakeFunction["g", "dummy.g", None, None, None, None]
          TOPLEVEL[g] <- n1
          return None


      function dummy.g._$dictcomp(.0):
        b0:
          n0 <- LOCAL[.0]
          jmp b1

        b1:
          n1 <- $NextIter(n0, None)
          n2 <- $HasNextIter(n0, None)
          if n2 then jmp b2 else jmp b3

        b2:
          LOCAL[num] <- n1
          n3 <- LOCAL[num]
          n4 <- LOCAL[num]
          n5 <- $Binary.Power(n4, 2, None)
          n6 <- $DictSetItem($BuildMap(), n3, n5, None)
          jmp b1

        b3:
          return $BuildMap()


      function dummy.f._$setcomp(.0):
        b0:
          n0 <- LOCAL[.0]
          jmp b1

        b1:
          n1 <- $NextIter(n0, None)
          n2 <- $HasNextIter(n0, None)
          if n2 then jmp b2 else jmp b3

        b2:
          LOCAL[x] <- n1
          n3 <- LOCAL[x]
          n4 <- $Binary.Add(n3, 1, None)
          n5 <- $SetAdd($BuildSet(), n4, None)
          jmp b1

        b3:
          return $BuildSet()


      function dummy.f(l):
        b0:
          n0 <- $MakeFunction["_$setcomp", "dummy.f._$setcomp", None, None, None, None]
          n1 <- LOCAL[l]
          n2 <- $GetIter(n1, None)
          n3 <- $Call(n0, n2, None)
          LOCAL[r] <- n3
          n4 <- LOCAL[r]
          return n4


      function dummy.g(l):
        b0:
          n0 <- $MakeFunction["_$dictcomp", "dummy.g._$dictcomp", None, None, None, None]
          n1 <- LOCAL[l]
          n2 <- $GetIter(n1, None)
          n3 <- $Call(n0, n2, None)
          LOCAL[squared_dict] <- n3
          n4 <- GLOBAL[r]
          return n4 |xxx}]


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
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          n1 <- $MakeFunction["g", "dummy.g", None, None, None, None]
          TOPLEVEL[g] <- n1
          return None


      function dummy.f():
        b0:
          return true


      function dummy.g():
        b0:
          n0 <- GLOBAL[f]
          n1 <- $Call(n0, None)
          n2 <- $GetAwaitable(n1, None)
          n3 <- $YieldFrom(n2, None, None)
          if n2 then jmp b1 else jmp b2

        b1:
          n6 <- GLOBAL[print]
          n7 <- $Call(n6, 0, None)
          jmp b3

        b2:
          n4 <- GLOBAL[print]
          n5 <- $Call(n4, 1, None)
          jmp b3

        b3:
          return None |}]


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
          n0 <- $MakeFunction["m", "dummy.m", None, None, None, None]
          TOPLEVEL[m] <- n0
          return None


      function dummy.m(self, x, y, test):
        b0:
          n0 <- GLOBAL[foo]
          n1 <- LOCAL[self]
          n2 <- LOCAL[test]
          if n2 then jmp b1 else jmp b2

        b1:
          n4 <- LOCAL[x]
          jmp b3(n4)

        b2:
          n3 <- LOCAL[y]
          jmp b3(n3)

        b3(n5):
          n6 <- $Call(n0, n1, n5, None)
          return n6 |}]


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
          n0 <- $MakeFunction["m", "dummy.m", None, None, None, None]
          TOPLEVEL[m] <- n0
          return None


      function dummy.m(self, x, y, test):
        b0:
          n0 <- LOCAL[self]
          n1 <- LOCAL[test]
          if n1 then jmp b1 else jmp b2

        b1:
          n3 <- LOCAL[x]
          jmp b3(n3)

        b2:
          n2 <- LOCAL[y]
          jmp b3(n2)

        b3(n4):
          n5 <- $CallMethod[foo](n0, n4, None)
          return n5 |}]


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
          n0 <- $MakeFunction["m", "dummy.m", None, None, None, None]
          TOPLEVEL[m] <- n0
          return None


      function dummy.m(x, y, test):
        b0:
          n0 <- LOCAL[test]
          if n0 then jmp b1 else jmp b2

        b1:
          n2 <- LOCAL[x]
          jmp b3(n2)

        b2:
          n1 <- LOCAL[y]
          jmp b3(n1)

        b3(n3):
          n4 <- $CallMethod[foo](n3, None)
          return n4 |}]


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
          n0 <- $MakeFunction["C", "dummy.C", None, None, None, None]
          n1 <- $BuildClass(n0, "C", None)
          TOPLEVEL[C] <- n1
          n2 <- TOPLEVEL[C]
          n3 <- $Call(n2, None)
          TOPLEVEL[o] <- n3
          n4 <- TOPLEVEL[o]
          n5 <- $CallMethod[foo](n4, None)
          n6 <- $MakeFunction["_$lambda", "dummy._$lambda", None, None, None, None]
          n7 <- TOPLEVEL[o]
          n7.foo <- n6
          n8 <- TOPLEVEL[o]
          n9 <- $CallMethod[foo](n8, None)
          return None


      function dummy.C():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          n1 <- $MakeFunction["foo", "dummy.C.foo", None, None, None, None]
          TOPLEVEL[foo] <- n1
          return None


      function dummy._$lambda():
        b0:
          n0 <- GLOBAL[print]
          n1 <- $Call(n0, "I am not foo", None)
          return n1


      function dummy.C.foo(self):
        b0:
          n0 <- GLOBAL[print]
          n1 <- $Call(n0, "I am foo", None)
          return None |}]


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
          n0 <- TOPLEVEL[dict]
          jmp b2

        b2:
          n1 <- $CallMethod[attr](n0, 0, None)
          TOPLEVEL[res] <- n1
          return None |}]


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
          n0 <- $MakeFunction["foo", "dummy.foo", None, None, None, None]
          TOPLEVEL[foo] <- n0
          return None


      function dummy.foo(i, f):
        b0:
          n0 <- GLOBAL[range]
          n1 <- GLOBAL[num]
          n2 <- $Call(n0, n1, None)
          n3 <- $GetIter(n2, None)
          jmp b1

        b1:
          n4 <- $NextIter(n3, None)
          n5 <- $HasNextIter(n3, None)
          if n5 then jmp b2 else jmp b6

        b2:
          LOCAL[i] <- n4
          n6 <- GLOBAL[read]
          n7 <- $Call(n6, None)
          n8 <- $GetAwaitable(n7, None)
          n9 <- $YieldFrom(n8, None, None)
          n10 <- $CallMethod[__enter__](n8, None)
          n11 <- $GetAwaitable(n10, None)
          n12 <- $YieldFrom(n11, None, None)
          LOCAL[f] <- n11
          jmp b3

        b3:
          n13 <- $CallMethod[__enter__](n8, None, None, None, None)
          n14 <- $GetAwaitable(n13, None)
          n15 <- $YieldFrom(n14, None, None)
          return None

        b6:
          return None |}]
