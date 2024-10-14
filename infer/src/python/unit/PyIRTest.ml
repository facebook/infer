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

      toplevel:
        b0:
          TOPLEVEL[my_fun] <- $FuncObj(my_fun, dummy.my_fun, {})
          TOPLEVEL[a] <- 10
          n0 <- TOPLEVEL[my_fun]
          n1 <- TOPLEVEL[a]
          n2 <- $Call(n0, 42, n1, None)
          TOPLEVEL[z] <- n2
          n3 <- TOPLEVEL[print]
          n4 <- TOPLEVEL[z]
          n5 <- $Call(n3, n4, None)
          return None


      dummy.my_fun:
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

      toplevel:
        b0:
          TOPLEVEL[update_global] <- $FuncObj(update_global, dummy.update_global, {})
          GLOBAL[z] <- 0
          n0 <- TOPLEVEL[update_global]
          n1 <- $Call(n0, None)
          n2 <- TOPLEVEL[print]
          n3 <- GLOBAL[z]
          n4 <- $Call(n2, n3, None)
          return None


      dummy.update_global:
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

      toplevel:
        b0:
          TOPLEVEL[coin] <- $FuncObj(coin, dummy.coin, {})
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return None


      dummy.coin:
        b0:
          return false


      dummy.f:
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

      toplevel:
        b0:
          TOPLEVEL[coin] <- $FuncObj(coin, dummy.coin, {})
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return None


      dummy.coin:
        b0:
          return false


      dummy.f:
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

      toplevel:
        b0:
          TOPLEVEL[coin] <- $FuncObj(coin, dummy.coin, {})
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return None


      dummy.coin:
        b0:
          return false


      dummy.f:
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

      toplevel:
        b0:
          TOPLEVEL[foo] <- $FuncObj(foo, dummy.foo, {})
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return None


      dummy.f:
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


      dummy.foo:
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

      toplevel:
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

      toplevel:
        b0:
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return None


      dummy.f:
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

      toplevel:
        b0:
          n0 <- TOPLEVEL[print]
          n1 <- $Call(n0, 42, None)
          TOPLEVEL[print] <- $FuncObj(print, dummy.print, {})
          n2 <- TOPLEVEL[print]
          n3 <- $Call(n2, 42, None)
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return None


      dummy.f:
        b0:
          n0 <- GLOBAL[print]
          n1 <- LOCAL[x]
          n2 <- $Call(n0, n1, None)
          return None


      dummy.print:
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

      toplevel:
        b0:
          n0 <- TOPLEVEL[int]
          n1 <- TOPLEVEL[float]
          TOPLEVEL[f0] <- $FuncObj(f0, dummy.f0, {})
          n2 <- TOPLEVEL[str]
          n3 <- TOPLEVEL[bool]
          TOPLEVEL[f1] <- $FuncObj(f1, dummy.f1, {})
          return None


      dummy.f0:
        b0:
          return None


      dummy.f1:
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

      toplevel:
        b0:
          n0 <- TOPLEVEL[int]
          TOPLEVEL[expect_int] <- $FuncObj(expect_int, dummy.expect_int, {})
          n1 <- TOPLEVEL[int]
          TOPLEVEL[get] <- $FuncObj(get, dummy.get, {})
          n2 <- TOPLEVEL[expect_int]
          n3 <- TOPLEVEL[get]
          n4 <- $Call(n3, None)
          n5 <- $Call(n2, n4, None)
          return None


      dummy.expect_int:
        b0:
          return None


      dummy.get:
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

      toplevel:
        b0:
          n0 <- TOPLEVEL[object]
          TOPLEVEL[expect] <- $FuncObj(expect, dummy.expect, {})
          n1 <- TOPLEVEL[int]
          TOPLEVEL[get] <- $FuncObj(get, dummy.get, {})
          n2 <- TOPLEVEL[expect]
          n3 <- TOPLEVEL[get]
          n4 <- $Call(n3, None)
          n5 <- $Call(n2, n4, None)
          return None


      dummy.expect:
        b0:
          return None


      dummy.get:
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

      toplevel:
        b0:
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return None


      dummy.f:
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

      toplevel:
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

      toplevel:
        b0:
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return None


      dummy.f:
        b0:
          n0 <- LOCAL[x]
          if n0 then jmp b1 else jmp b2

        b1:
          n1 <- LOCAL[y]
          if $Not(n1) then jmp b2 else jmp b4(n1)

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

      toplevel:
        b0:
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return None


      dummy.f:
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

      toplevel:
        b0:
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return None


      dummy.f:
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

      toplevel:
        b0:
          TOPLEVEL[is_check] <- $FuncObj(is_check, dummy.is_check, {})
          TOPLEVEL[is_not_check] <- $FuncObj(is_not_check, dummy.is_not_check, {})
          TOPLEVEL[in_check] <- $FuncObj(in_check, dummy.in_check, {})
          TOPLEVEL[in_not_check] <- $FuncObj(in_not_check, dummy.in_not_check, {})
          return None


      dummy.in_check:
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[l]
          n2 <- $Compare.in(n0, n1, None)
          return n2


      dummy.in_not_check:
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[l]
          n2 <- $Compare.not_in(n0, n1, None)
          return n2


      dummy.is_check:
        b0:
          n0 <- LOCAL[x]
          n1 <- $Compare.is(n0, None, None)
          return n1


      dummy.is_not_check:
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

      toplevel:
        b0:
          TOPLEVEL[t] <- (1,2,3)
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return None


      dummy.f:
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[y]
          n2 <- LOCAL[z]
          return (unpacked)(n0, n1, n2) |}]


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

      toplevel:
        b0:
          TOPLEVEL[l] <- (unpacked)[1, 2, 3]
          n0 <- TOPLEVEL[print]
          n1 <- TOPLEVEL[l]
          n2 <- $Call(n0, n1, None)
          TOPLEVEL[build_list] <- $FuncObj(build_list, dummy.build_list, {})
          n3 <- TOPLEVEL[build_list]
          n4 <- $Call(n3, None)
          TOPLEVEL[x] <- n4[0]
          TOPLEVEL[y] <- n4[1]
          TOPLEVEL[z] <- n4[2]
          return None


      dummy.build_list:
        b0:
          return (unpacked)[1, 2, 3] |}]


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

      toplevel:
        b0:
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return None


      dummy.f:
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

      toplevel:
        b0:
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          n0 <- TOPLEVEL[f]
          n1 <- $Call(n0, None)
          TOPLEVEL[a] <- n1[0]
          TOPLEVEL[b] <- n1[1]
          return None


      dummy.f:
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

      toplevel:
        b0:
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          n0 <- TOPLEVEL[f]
          n1 <- $Call(n0, 0, 2, 1, ("y","x"))
          return None


      dummy.f:
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

      toplevel:
        b0:
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return None


      dummy.f:
        b0:
          n0 <- LOCAL[a]
          n1 <- LOCAL[b]
          n2 <- LOCAL[m]
          n3 <- $Compare.not_in((unpacked)(n0, n1), n2, None)
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
          n7 <- $Compare.not_in((unpacked)(n4, n5), n6, None)
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

      toplevel:
        b0:
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          TOPLEVEL[test_arguments] <- $FuncObj(test_arguments, dummy.test_arguments, {})
          return None


      dummy.f:
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
          return $Concat(unpacked)("foo.", n2, n5, n8)


      dummy.test_arguments:
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[y]
          n2 <- $Binary.Multiply(n0, n1, None)
          n3 <- LOCAL[width]
          n4 <- $Format(n3, None, None)
          n5 <- $Format(n2, n4, None)
          return $Concat(unpacked)("x=", n5) |}]


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

      toplevel:
        b0:
          TOPLEVEL[pos] <- $FuncObj(pos, dummy.pos, {})
          TOPLEVEL[neg] <- $FuncObj(neg, dummy.neg, {})
          TOPLEVEL[test_not] <- $FuncObj(test_not, dummy.test_not, {})
          TOPLEVEL[inv] <- $FuncObj(inv, dummy.inv, {})
          return None


      dummy.inv:
        b0:
          n0 <- LOCAL[x]
          n1 <- $Unary.Invert(n0, None)
          return n1


      dummy.neg:
        b0:
          n0 <- LOCAL[x]
          n1 <- $Unary.Negative(n0, None)
          return n1


      dummy.pos:
        b0:
          n0 <- LOCAL[x]
          n1 <- $Unary.Positive(n0, None)
          return n1


      dummy.test_not:
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

      toplevel:
        b0:
          GLOBAL[gx] <- 100
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          n0 <- TOPLEVEL[f]
          n1 <- $Call(n0, 42, None)
          TOPLEVEL[g] <- n1
          n2 <- TOPLEVEL[print]
          n3 <- TOPLEVEL[g]
          n4 <- $Call(n3, None)
          n5 <- $Call(n2, n4, None)
          return None


      dummy.f:
        b0:
          n0 <- $StoreDeref[1,"lx"](1000, None)
          n1 <- $LoadClosure[0,"ax"](None)
          n2 <- $LoadClosure[1,"lx"](None)
          LOCAL[inner] <- $FuncObj(inner, dummy.f.inner, {})
          n3 <- $StoreDeref[1,"lx"](1664, None)
          n4 <- LOCAL[inner]
          return n4


      dummy.f.inner:
        b0:
          LOCAL[ix] <- 20
          n0 <- GLOBAL[print]
          n1 <- GLOBAL[gx]
          n2 <- $Call(n0, n1, None)
          n3 <- GLOBAL[print]
          n4 <- $LoadDeref[0,"ax"](None)
          n5 <- $Call(n3, n4, None)
          n6 <- GLOBAL[print]
          n7 <- $LoadDeref[1,"lx"](None)
          n8 <- $Call(n6, n7, None)
          n9 <- GLOBAL[print]
          n10 <- LOCAL[ix]
          n11 <- $Call(n9, n10, None)
          GLOBAL[gx] <- 10
          n12 <- $StoreDeref[1,"lx"](2, None)
          n13 <- $LoadDeref[1,"lx"](None)
          return n13 |}]


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

      toplevel:
        b0:
          n0 <- TOPLEVEL[l]
          n1 <- $GetIter(n0, None)
          n2 <- $Call($FuncObj(<listcomp>, dummy.<listcomp>, {}), n1, None)
          TOPLEVEL[g] <- n2
          n3 <- TOPLEVEL[l]
          n4 <- $GetIter(n3, None)
          n5 <- $Call($FuncObj(<listcomp>, dummy.<listcomp>, {}), n4, None)
          TOPLEVEL[g0] <- n5
          n6 <- TOPLEVEL[print]
          n7 <- TOPLEVEL[g]
          n8 <- $Call(n6, n7, None)
          n9 <- TOPLEVEL[print]
          n10 <- TOPLEVEL[g0]
          n11 <- $Call(n9, n10, None)
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return None


      dummy.<listcomp>:
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
          n5 <- $ListAppend((unpacked)[], n4, None)
          jmp b1

        b3:
          return (unpacked)[]


      dummy.f.<listcomp>:
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
          n5 <- $ListAppend((unpacked)[], n4, None)
          jmp b1

        b3:
          return (unpacked)[]


      dummy.f:
        b0:
          n0 <- LOCAL[l]
          n1 <- $GetIter(n0, None)
          n2 <- $Call($FuncObj(<listcomp>, dummy.f.<listcomp>, {}), n1, None)
          LOCAL[r] <- n2
          n3 <- LOCAL[l]
          n4 <- $GetIter(n3, None)
          n5 <- $Call($FuncObj(<listcomp>, dummy.f.<listcomp>, {}), n4, None)
          LOCAL[r0] <- n5
          n6 <- GLOBAL[print]
          n7 <- LOCAL[r]
          n8 <- $Call(n6, n7, None)
          n9 <- GLOBAL[print]
          n10 <- LOCAL[r0]
          n11 <- $Call(n9, n10, None)
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

      toplevel:
        b0:
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          TOPLEVEL[g] <- $FuncObj(g, dummy.g, {})
          return None


      dummy.g.<dictcomp>:
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
          n6 <- $DictSetItem((unpacked){||}, n3, n5, None)
          jmp b1

        b3:
          return (unpacked){||}


      dummy.f.<setcomp>:
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
          n5 <- $SetAdd((unpacked){}, n4, None)
          jmp b1

        b3:
          return (unpacked){}


      dummy.f:
        b0:
          n0 <- LOCAL[l]
          n1 <- $GetIter(n0, None)
          n2 <- $Call($FuncObj(<setcomp>, dummy.f.<setcomp>, {}), n1, None)
          LOCAL[r] <- n2
          n3 <- LOCAL[r]
          return n3


      dummy.g:
        b0:
          n0 <- LOCAL[l]
          n1 <- $GetIter(n0, None)
          n2 <- $Call($FuncObj(<dictcomp>, dummy.g.<dictcomp>, {}), n1, None)
          LOCAL[squared_dict] <- n2
          n3 <- GLOBAL[r]
          return n3 |xxx}]


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

      toplevel:
        b0:
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          TOPLEVEL[g] <- $FuncObj(g, dummy.g, {})
          return None


      dummy.f:
        b0:
          return true


      dummy.g:
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

      toplevel:
        b0:
          TOPLEVEL[m] <- $FuncObj(m, dummy.m, {})
          return None


      dummy.m:
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

      toplevel:
        b0:
          TOPLEVEL[m] <- $FuncObj(m, dummy.m, {})
          return None


      dummy.m:
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

      toplevel:
        b0:
          TOPLEVEL[m] <- $FuncObj(m, dummy.m, {})
          return None


      dummy.m:
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

      toplevel:
        b0:
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), "C", None)
          TOPLEVEL[C] <- n0
          n1 <- TOPLEVEL[C]
          n2 <- $Call(n1, None)
          TOPLEVEL[o] <- n2
          n3 <- TOPLEVEL[o]
          n4 <- $CallMethod[foo](n3, None)
          n5 <- TOPLEVEL[o]
          n5.foo <- $FuncObj(<lambda>, dummy.<lambda>, {})
          n6 <- TOPLEVEL[o]
          n7 <- $CallMethod[foo](n6, None)
          return None


      dummy.<lambda>:
        b0:
          n0 <- GLOBAL[print]
          n1 <- $Call(n0, "I am not foo", None)
          return n1


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          TOPLEVEL[foo] <- $FuncObj(foo, dummy.C.foo, {})
          return None


      dummy.C.foo:
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

      toplevel:
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

      toplevel:
        b0:
          TOPLEVEL[foo] <- $FuncObj(foo, dummy.foo, {})
          return None


      dummy.foo:
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
