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
          TOPLEVEL[a] <- PYCInt (10)
          n0 <- TOPLEVEL[my_fun]
          n1 <- TOPLEVEL[a]
          n2 <- n0(PYCInt (42), n1)
          TOPLEVEL[z] <- n2
          n3 <- TOPLEVEL[print]
          n4 <- TOPLEVEL[z]
          n5 <- n3(n4)
          return PYCNone


      dummy.my_fun:
        b0:
          n0 <- GLOBAL[print]
          n1 <- LOCAL[x]
          n2 <- n0(n1)
          n3 <- GLOBAL[print]
          n4 <- LOCAL[y]
          n5 <- n3(n4)
          n6 <- LOCAL[x]
          n7 <- LOCAL[y]
          n8 <- $Binary.Add(n6, n7)
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
          GLOBAL[z] <- PYCInt (0)
          n0 <- TOPLEVEL[update_global]
          n1 <- n0()
          n2 <- TOPLEVEL[print]
          n3 <- GLOBAL[z]
          n4 <- n2(n3)
          return PYCNone


      dummy.update_global:
        b0:
          n0 <- GLOBAL[z]
          n1 <- $Binary.Add(n0, PYCInt (1))
          GLOBAL[z] <- n1
          return PYCNone |}]


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
          return PYCNone


      dummy.coin:
        b0:
          return PYCBool (false)


      dummy.f:
        b0:
          n0 <- GLOBAL[coin]
          n1 <- n0()
          if n1 then jmp b1 else jmp b2

        b1:
          n2 <- LOCAL[x]
          return n2

        b2:
          n3 <- LOCAL[y]
          return n3 |}]


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
          return PYCNone


      dummy.coin:
        b0:
          return PYCBool (false)


      dummy.f:
        b0:
          LOCAL[z] <- PYCInt (0)
          n0 <- GLOBAL[coin]
          n1 <- n0()
          if n1 then jmp b1 else jmp b2

        b1:
          n2 <- LOCAL[x]
          LOCAL[z] <- n2
          jmp b3

        b2:
          n4 <- LOCAL[y]
          LOCAL[z] <- n4
          jmp b3

        b3:
          n3 <- LOCAL[z]
          return n3 |}]


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
          return PYCNone


      dummy.coin:
        b0:
          return PYCBool (false)


      dummy.f:
        b0:
          LOCAL[z] <- PYCInt (0)
          n0 <- GLOBAL[coin]
          n1 <- n0()
          if n1 then jmp b1 else jmp b5

        b1:
          n2 <- GLOBAL[coin]
          n3 <- n2()
          if n3 then jmp b2 else jmp b3

        b2:
          n4 <- LOCAL[x]
          LOCAL[z] <- n4
          jmp b4

        b3:
          return PYCInt (1664)

        b4:
          n5 <- LOCAL[z]
          n6 <- $Binary.Add(n5, PYCInt (1))
          LOCAL[z] <- n6
          jmp b8

        b5:
          n8 <- LOCAL[z]
          n9 <- $Binary.Add(n8, PYCInt (1))
          LOCAL[z] <- n9
          n10 <- GLOBAL[coin]
          n11 <- n10()
          if n11 then jmp b6 else jmp b7

        b6:
          return PYCInt (42)

        b7:
          n12 <- LOCAL[y]
          LOCAL[z] <- n12
          jmp b8

        b8:
          n7 <- LOCAL[z]
          return n7 |}]


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
          return PYCNone


      dummy.f:
        b0:
          n0 <- GLOBAL[foo]
          n1 <- LOCAL[x]
          if n1 then jmp b1(n0) else jmp b2(n0)

        b1(n2):
          jmp b3(n2, PYCInt (1))

        b2(n6):
          jmp b3(n6, PYCInt (0))

        b3(n3, n4):
          n5 <- n3(n4)
          return PYCNone


      dummy.foo:
        b0:
          return PYCNone |}]


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
          n1 <- n0(PYCInt (10))
          n2 <- $GetIter(n1)
          jmp b1(n2)

        b1(n3):
          n4 <- $NextIter(n3)
          n5 <- $HasNextIter(n3)
          if n5 then jmp b2(n3, n4) else jmp b3

        b2(n6, n7):
          TOPLEVEL[x] <- n7
          n8 <- TOPLEVEL[print]
          n9 <- TOPLEVEL[x]
          n10 <- n8(n9)
          jmp b1(n6)

        b3:
          return PYCNone |}]


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
          return PYCNone


      dummy.f:
        b0:
          n0 <- LOCAL[l]
          n1 <- $GetIter(n0)
          jmp b1(n1)

        b1(n2):
          n3 <- $NextIter(n2)
          n4 <- $HasNextIter(n2)
          if n4 then jmp b2(n2, n3) else jmp b7

        b2(n5, n6):
          LOCAL[x] <- n6
          n7 <- LOCAL[bar]
          n8 <- n7()
          n9 <- n8.__enter__()
          n10 <- LOCAL[toto]
          n11 <- n10()
          n12 <- n11.__enter__()
          LOCAL[obj] <- n12
          n13 <- LOCAL[y]
          if n13 then jmp b3(n5, CM(n8).__exit__, CM(n11).__exit__) else
          jmp b4(n5, CM(n8).__exit__, CM(n11).__exit__)

        b3(n14, n15, n16):
          n17 <- PYCNone(PYCNone, PYCNone, PYCNone)
          n18 <- PYCNone(PYCNone, PYCNone, PYCNone)
          jmp b1(n14, n15, n16)

        b4(n19, n20, n21):
          n22 <- GLOBAL[print]
          n23 <- n22(PYCString ("nop"))
          jmp b5(n19, n20, n21, PYCNone)

        b5(n24, n25, n26, n27):
          n28 <- n27(PYCNone, PYCNone, PYCNone)
          jmp b6(n24, n25, PYCNone)

        b6(n29, n30, n31):
          n32 <- n31(PYCNone, PYCNone, PYCNone)
          jmp b1(n29)

        b7:
          return PYCNone |}]


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
          n1 <- n0(PYCInt (42))
          TOPLEVEL[print] <- $FuncObj(print, dummy.print, {})
          n2 <- TOPLEVEL[print]
          n3 <- n2(PYCInt (42))
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.f:
        b0:
          n0 <- GLOBAL[print]
          n1 <- LOCAL[x]
          n2 <- n0(n1)
          return PYCNone


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
          return PYCNone


      dummy.f0:
        b0:
          return PYCNone


      dummy.f1:
        b0:
          return PYCNone |}]


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
          n4 <- n3()
          n5 <- n2(n4)
          return PYCNone


      dummy.expect_int:
        b0:
          return PYCNone


      dummy.get:
        b0:
          return PYCInt (42) |}]


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
          n4 <- n3()
          n5 <- n2(n4)
          return PYCNone


      dummy.expect:
        b0:
          return PYCNone


      dummy.get:
        b0:
          return PYCInt (42) |}]


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
          return PYCNone


      dummy.f:
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[y]
          n2 <- $Compare.eq(n0, n1)
          return n2 |}]


let%expect_test _ =
  let source = "True != False" in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- $Compare.neq(PYCBool (true), PYCBool (false))
          return PYCNone |}]


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
          return PYCNone


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
          return PYCNone


      dummy.f:
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[y]
          n2 <- $Compare.gt(n0, n1)
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
          return PYCNone


      dummy.f:
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[y]
          n2 <- $Compare.le(n0, n1)
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
          return PYCNone


      dummy.in_check:
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[l]
          n2 <- $Compare.in(n0, n1)
          return n2


      dummy.in_not_check:
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[l]
          n2 <- $Compare.not_in(n0, n1)
          return n2


      dummy.is_check:
        b0:
          n0 <- LOCAL[x]
          n1 <- $Compare.is(n0, PYCNone)
          return n1


      dummy.is_not_check:
        b0:
          n0 <- LOCAL[x]
          n1 <- $Compare.is_not(n0, PYCNone)
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
          TOPLEVEL[t] <- PYCTuple ([|PYCInt (1); PYCInt (2); PYCInt (3)|])
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.f:
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[y]
          n2 <- LOCAL[z]
          return (n0, n1, n2) |}]


let%expect_test _ =
  let source = {|
l = [1, 2, 3]
print(l)

def build_list():
          return [1, 2, 3]
|} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          TOPLEVEL[l] <- [PYCInt (1), PYCInt (2), PYCInt (3)]
          n0 <- TOPLEVEL[print]
          n1 <- TOPLEVEL[l]
          n2 <- n0(n1)
          TOPLEVEL[build_list] <- $FuncObj(build_list, dummy.build_list, {})
          return PYCNone


      dummy.build_list:
        b0:
          return [PYCInt (1), PYCInt (2), PYCInt (3)] |}]


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
          return PYCNone


      dummy.f:
        b0:
          n0 <- LOCAL[foo]
          n1 <- n0()
          n2 <- n1.__enter__()
          LOCAL[foo0] <- n2
          n3 <- LOCAL[bar]
          n4 <- n3()
          n5 <- n4.__enter__()
          LOCAL[bar0] <- n5
          n6 <- GLOBAL[print]
          n7 <- LOCAL[bar0]
          n8 <- n6(n7)
          jmp b1(CM(n1).__exit__, CM(n4).__exit__, PYCNone)

        b1(n9, n10, n11):
          n12 <- n11(PYCNone, PYCNone, PYCNone)
          n13 <- GLOBAL[print]
          n14 <- LOCAL[foo0]
          n15 <- n13(n14)
          n16 <- PYCNone(PYCNone, PYCNone, PYCNone)
          return PYCInt (42) |}]


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
          n1 <- n0()
          TOPLEVEL[a] <- n1[PYCInt (0)]
          TOPLEVEL[b] <- n1[PYCInt (1)]
          return PYCNone


      dummy.f:
        b0:
          return PYCNone |}]


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
          n1 <- n0(PYCInt (0), y= PYCInt (2), x= PYCInt (1))
          return PYCNone


      dummy.f:
        b0:
          return PYCNone |}]


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
          return PYCNone


      dummy.f:
        b0:
          n0 <- LOCAL[a]
          n1 <- LOCAL[b]
          n2 <- LOCAL[m]
          n3 <- $Compare.not_in((n0, n1), n2)
          if n3 then jmp b1 else jmp b2

        b1:
          n4 <- LOCAL[b]
          n5 <- $Inplace.Subtract(n4, PYCInt (1))
          LOCAL[b] <- n5
          jmp b0

        b2:
          n6 <- LOCAL[a]
          n7 <- LOCAL[c]
          n8 <- LOCAL[m]
          n9 <- $Compare.not_in((n6, n7), n8)
          if n9 then jmp b3 else jmp b4

        b3:
          n10 <- LOCAL[c]
          n11 <- $Inplace.Add(n10, PYCInt (1))
          LOCAL[c] <- n11
          jmp b2

        b4:
          return PYCNone |}]


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
          return PYCNone


      dummy.f:
        b0:
          n0 <- LOCAL[name]
          n1 <- $FormatFn.repr(n0)
          n2 <- $Format(n1, PYCNone)
          n3 <- LOCAL[name]
          n4 <- $FormatFn.str(n3)
          n5 <- $Format(n4, PYCNone)
          n6 <- LOCAL[name]
          n7 <- $FormatFn.ascii(n6)
          n8 <- $Format(n7, PYCNone)
          return $Concat(PYCString ("foo."), n2, n5, n8)


      dummy.test_arguments:
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[y]
          n2 <- $Binary.Multiply(n0, n1)
          n3 <- LOCAL[width]
          n4 <- $Format(n3, PYCNone)
          n5 <- $Format(n2, n4)
          return $Concat(PYCString ("x="), n5) |}]


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
          return PYCNone


      dummy.inv:
        b0:
          n0 <- LOCAL[x]
          n1 <- $Unary.Invert(n0)
          return n1


      dummy.neg:
        b0:
          n0 <- LOCAL[x]
          n1 <- $Unary.Negative(n0)
          return n1


      dummy.pos:
        b0:
          n0 <- LOCAL[x]
          n1 <- $Unary.Positive(n0)
          return n1


      dummy.test_not:
        b0:
          n0 <- LOCAL[x]
          n1 <- $Unary.Not(n0)
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
          GLOBAL[gx] <- PYCInt (100)
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          n0 <- TOPLEVEL[f]
          n1 <- n0(PYCInt (42))
          TOPLEVEL[g] <- n1
          n2 <- TOPLEVEL[print]
          n3 <- TOPLEVEL[g]
          n4 <- n3()
          n5 <- n2(n4)
          return PYCNone


      dummy.f:
        b0:
          DEREF[lx] <- PYCInt (1000)
          LOCAL[inner] <- $FuncObj(inner, dummy.f.inner, {})
          DEREF[lx] <- PYCInt (1664)
          n0 <- LOCAL[inner]
          return n0


      dummy.f.inner:
        b0:
          LOCAL[ix] <- PYCInt (20)
          n0 <- GLOBAL[print]
          n1 <- GLOBAL[gx]
          n2 <- n0(n1)
          n3 <- GLOBAL[print]
          n4 <- DEREF[ax]
          n5 <- n3(n4)
          n6 <- GLOBAL[print]
          n7 <- DEREF[lx]
          n8 <- n6(n7)
          n9 <- GLOBAL[print]
          n10 <- LOCAL[ix]
          n11 <- n9(n10)
          GLOBAL[gx] <- PYCInt (10)
          DEREF[lx] <- PYCInt (2)
          n12 <- DEREF[lx]
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

      toplevel:
        b0:
          n0 <- TOPLEVEL[l]
          n1 <- $GetIter(n0)
          n2 <- $FuncObj(<listcomp>, dummy.<listcomp>, {})(n1)
          TOPLEVEL[g] <- n2
          n3 <- TOPLEVEL[l]
          n4 <- $GetIter(n3)
          n5 <- $FuncObj(<listcomp>, dummy.<listcomp>, {})(n4)
          TOPLEVEL[g0] <- n5
          n6 <- TOPLEVEL[print]
          n7 <- TOPLEVEL[g]
          n8 <- n6(n7)
          n9 <- TOPLEVEL[print]
          n10 <- TOPLEVEL[g0]
          n11 <- n9(n10)
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.<listcomp>:
        b0:
          n0 <- LOCAL[.0]
          jmp b1([], n0)

        b1(n1, n2):
          n3 <- $NextIter(n2)
          n4 <- $HasNextIter(n2)
          if n4 then jmp b2(n1, n2, n3) else jmp b3(n1)

        b2(n5, n6, n7):
          LOCAL[x] <- n7
          n8 <- LOCAL[x]
          n9 <- $Binary.Add(n8, PYCInt (2))
          n10 <- $ListAppend(n5, n9)
          jmp b1(n5, n6)

        b3(n11):
          return n11


      dummy.f.<listcomp>:
        b0:
          n0 <- LOCAL[.0]
          jmp b1([], n0)

        b1(n1, n2):
          n3 <- $NextIter(n2)
          n4 <- $HasNextIter(n2)
          if n4 then jmp b2(n1, n2, n3) else jmp b3(n1)

        b2(n5, n6, n7):
          LOCAL[x] <- n7
          n8 <- LOCAL[x]
          n9 <- $Binary.Add(n8, PYCInt (2))
          n10 <- $ListAppend(n5, n9)
          jmp b1(n5, n6)

        b3(n11):
          return n11


      dummy.f:
        b0:
          n0 <- LOCAL[l]
          n1 <- $GetIter(n0)
          n2 <- $FuncObj(<listcomp>, dummy.f.<listcomp>, {})(n1)
          LOCAL[r] <- n2
          n3 <- LOCAL[l]
          n4 <- $GetIter(n3)
          n5 <- $FuncObj(<listcomp>, dummy.f.<listcomp>, {})(n4)
          LOCAL[r0] <- n5
          n6 <- GLOBAL[print]
          n7 <- LOCAL[r]
          n8 <- n6(n7)
          n9 <- GLOBAL[print]
          n10 <- LOCAL[r0]
          n11 <- n9(n10)
          return PYCNone |}]


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
          return PYCNone


      dummy.g.<dictcomp>:
        b0:
          n0 <- LOCAL[.0]
          jmp b1({||}, n0)

        b1(n1, n2):
          n3 <- $NextIter(n2)
          n4 <- $HasNextIter(n2)
          if n4 then jmp b2(n1, n2, n3) else jmp b3(n1)

        b2(n5, n6, n7):
          LOCAL[num] <- n7
          n8 <- LOCAL[num]
          n9 <- LOCAL[num]
          n10 <- $Binary.Power(n9, PYCInt (2))
          n11 <- $DictSetItem(n5, n8, n10)
          jmp b1(n5, n6)

        b3(n12):
          return n12


      dummy.f.<setcomp>:
        b0:
          n0 <- LOCAL[.0]
          jmp b1({}, n0)

        b1(n1, n2):
          n3 <- $NextIter(n2)
          n4 <- $HasNextIter(n2)
          if n4 then jmp b2(n1, n2, n3) else jmp b3(n1)

        b2(n5, n6, n7):
          LOCAL[x] <- n7
          n8 <- LOCAL[x]
          n9 <- $Binary.Add(n8, PYCInt (1))
          n10 <- $SetAdd(n5, n9)
          jmp b1(n5, n6)

        b3(n11):
          return n11


      dummy.f:
        b0:
          n0 <- LOCAL[l]
          n1 <- $GetIter(n0)
          n2 <- $FuncObj(<setcomp>, dummy.f.<setcomp>, {})(n1)
          LOCAL[r] <- n2
          n3 <- LOCAL[r]
          return n3


      dummy.g:
        b0:
          n0 <- LOCAL[l]
          n1 <- $GetIter(n0)
          n2 <- $FuncObj(<dictcomp>, dummy.g.<dictcomp>, {})(n1)
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
          return PYCNone


      dummy.f:
        b0:
          return PYCBool (true)


      dummy.g:
        b0:
          n0 <- GLOBAL[f]
          n1 <- n0()
          n2 <- $GetAwaitable(n1)
          n3 <- $YieldFrom(n2, PYCNone)
          if n2 then jmp b1 else jmp b2

        b1:
          n4 <- GLOBAL[print]
          n5 <- n4(PYCInt (0))
          jmp b3

        b2:
          n6 <- GLOBAL[print]
          n7 <- n6(PYCInt (1))
          jmp b3

        b3:
          return PYCNone |}]


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
          return PYCNone


      dummy.m:
        b0:
          n0 <- GLOBAL[foo]
          n1 <- LOCAL[self]
          n2 <- LOCAL[test]
          if n2 then jmp b1(n0, n1) else jmp b2(n0, n1)

        b1(n3, n4):
          n5 <- LOCAL[x]
          jmp b3(n3, n4, n5)

        b2(n10, n11):
          n12 <- LOCAL[y]
          jmp b3(n10, n11, n12)

        b3(n6, n7, n8):
          n9 <- n6(n7, n8)
          return n9 |}]


let%expect_test _ =
  let source = {|
def m(self, x, y, test):
    return self.foo(x if test else y)
|} in
  PyIR.test source ;
  [%expect {|
    IR error: LOAD_METHOD_EXPECTED: expected a LOAD_METHOD result but got n4 |}]


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
          return PYCNone


      dummy.m:
        b0:
          n0 <- LOCAL[test]
          if n0 then jmp b1 else jmp b2

        b1:
          n1 <- LOCAL[x]
          jmp b3(n1)

        b2:
          n4 <- LOCAL[y]
          jmp b3(n4)

        b3(n2):
          n3 <- n2.foo()
          return n3 |}]


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
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"))
          TOPLEVEL[C] <- n0
          n1 <- TOPLEVEL[C]
          n2 <- n1()
          TOPLEVEL[o] <- n2
          n3 <- TOPLEVEL[o]
          n4 <- n3.foo()
          n5 <- TOPLEVEL[o]
          n5.foo <- $FuncObj(<lambda>, dummy.<lambda>, {})
          n6 <- TOPLEVEL[o]
          n7 <- n6.foo()
          return PYCNone


      dummy.<lambda>:
        b0:
          n0 <- GLOBAL[print]
          n1 <- n0(PYCString ("I am not foo"))
          return n1


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- PYCString ("C")
          TOPLEVEL[foo] <- $FuncObj(foo, dummy.C.foo, {})
          return PYCNone


      dummy.C.foo:
        b0:
          n0 <- GLOBAL[print]
          n1 <- n0(PYCString ("I am foo"))
          return PYCNone |}]
