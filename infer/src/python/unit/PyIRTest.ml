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
          my_fun(PyIR.Name) <- $FuncObj(my_fun, dummy.my_fun, {})
          a(PyIR.Name) <- PYCInt (10)
          n0 <- my_fun(PyIR.Name)(PYCInt (42), a(PyIR.Name))
          z(PyIR.Name) <- n0
          n1 <- print(PyIR.Name)(z(PyIR.Name))
          return PYCNone


      dummy.my_fun:
        b0:
          n0 <- print(PyIR.Global)(x(PyIR.Fast))
          n1 <- print(PyIR.Global)(y(PyIR.Fast))
          n2 <- $Binary.Add(x(PyIR.Fast), y(PyIR.Fast))
          z(PyIR.Fast) <- n2
          return z(PyIR.Fast) |}]


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
          update_global(PyIR.Name) <- $FuncObj(update_global, dummy.update_global, {})
          z(PyIR.Global) <- PYCInt (0)
          n0 <- update_global(PyIR.Name)()
          n1 <- print(PyIR.Name)(z(PyIR.Global))
          return PYCNone


      dummy.update_global:
        b0:
          n0 <- $Binary.Add(z(PyIR.Global), PYCInt (1))
          z(PyIR.Global) <- n0
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
          coin(PyIR.Name) <- $FuncObj(coin, dummy.coin, {})
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.coin:
        b0:
          return PYCBool (false)


      dummy.f:
        b0:
          n0 <- coin(PyIR.Global)()
          if n0 then jmp b1 else jmp b2

        b1:
          return x(PyIR.Fast)

        b2:
          return y(PyIR.Fast) |}]


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
          coin(PyIR.Name) <- $FuncObj(coin, dummy.coin, {})
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.coin:
        b0:
          return PYCBool (false)


      dummy.f:
        b0:
          z(PyIR.Fast) <- PYCInt (0)
          n0 <- coin(PyIR.Global)()
          if n0 then jmp b1 else jmp b2

        b1:
          z(PyIR.Fast) <- x(PyIR.Fast)
          jmp b3

        b2:
          z(PyIR.Fast) <- y(PyIR.Fast)
          jmp b3

        b3:
          return z(PyIR.Fast) |}]


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
          coin(PyIR.Name) <- $FuncObj(coin, dummy.coin, {})
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.coin:
        b0:
          return PYCBool (false)


      dummy.f:
        b0:
          z(PyIR.Fast) <- PYCInt (0)
          n0 <- coin(PyIR.Global)()
          if n0 then jmp b1 else jmp b2

        b1:
          n1 <- coin(PyIR.Global)()
          if n1 then jmp b3 else jmp b4

        b2:
          n3 <- $Binary.Add(z(PyIR.Fast), PYCInt (1))
          z(PyIR.Fast) <- n3
          n4 <- coin(PyIR.Global)()
          if n4 then jmp b7 else jmp b8

        b3:
          z(PyIR.Fast) <- x(PyIR.Fast)
          jmp b5

        b4:
          return PYCInt (1664)

        b5:
          n2 <- $Binary.Add(z(PyIR.Fast), PYCInt (1))
          z(PyIR.Fast) <- n2
          jmp b6

        b6:
          return z(PyIR.Fast)

        b7:
          return PYCInt (42)

        b8:
          z(PyIR.Fast) <- y(PyIR.Fast)
          jmp b6 |}]


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
          foo(PyIR.Name) <- $FuncObj(foo, dummy.foo, {})
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.f:
        b0:
          if x(PyIR.Fast) then jmp b1(foo(PyIR.Global)) else jmp b2(foo(PyIR.Global))

        b1:
          jmp b3(PYCInt (1), n0)

        b2:
          jmp b3(PYCInt (0), n1)

        b3:
          n4 <- n2(n3)
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
          n0 <- range(PyIR.Name)(PYCInt (10))
          n1 <- $GetIter(n0)
          jmp b1(n1)

        b1:
          n3 <- $NextIter(n2)
          n4 <- $HasNextIter(n3)
          if n4 then jmp b2 else jmp b3

        b2:
          n5 <- $IterData(n3)
          x(PyIR.Name) <- n5
          n6 <- print(PyIR.Name)(x(PyIR.Name))
          jmp b1(n2)

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
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.f:
        b0:
          n0 <- $GetIter(l(PyIR.Fast))
          jmp b1(n0)

        b1:
          n2 <- $NextIter(n1)
          n3 <- $HasNextIter(n2)
          if n3 then jmp b2 else jmp b3

        b2:
          n4 <- $IterData(n2)
          x(PyIR.Fast) <- n4
          n5 <- bar(PyIR.Fast)()
          n6 <- $LoadMethod(n5, __enter__)()
          n9 <- toto(PyIR.Fast)()
          n10 <- $LoadMethod(n9, __enter__)()
          obj(PyIR.Fast) <- n10
          if y(PyIR.Fast) then jmp b6(CM(n9).__exit__, CM(n5).__exit__, n1) else
          jmp b7(CM(n9).__exit__, CM(n5).__exit__, n1)

        b3:
          return PYCNone

        b4:
          n35 <- n8(PYCNone, PYCNone, PYCNone)
          jmp b1(n7)

        b5:
          n32 <- n13(PYCNone, PYCNone, PYCNone)
          jmp b4(n12, n11)

        b6:
          jmp b8(n16, n15, n14)

        b7:
          n28 <- print(PyIR.Global)(PYCString ("nop"))
          jmp b5(n19, n18, n17)

        b8:
          n23 <- n22(PYCNone, PYCNone, PYCNone)
          jmp b9(n21, n20)

        b9:
          n26 <- n25(PYCNone, PYCNone, PYCNone)
          jmp b1(n24) |}]


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
          n0 <- print(PyIR.Name)(PYCInt (42))
          print(PyIR.Name) <- $FuncObj(print, dummy.print, {})
          n1 <- print(PyIR.Name)(PYCInt (42))
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.f:
        b0:
          n0 <- print(PyIR.Global)(x(PyIR.Fast))
          return PYCNone


      dummy.print:
        b0:
          return x(PyIR.Fast) |}]


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
          f0(PyIR.Name) <- $FuncObj(f0, dummy.f0, {})
          f1(PyIR.Name) <- $FuncObj(f1, dummy.f1, {})
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
          expect_int(PyIR.Name) <- $FuncObj(expect_int, dummy.expect_int, {})
          get(PyIR.Name) <- $FuncObj(get, dummy.get, {})
          n0 <- get(PyIR.Name)()
          n1 <- expect_int(PyIR.Name)(n0)
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
          expect(PyIR.Name) <- $FuncObj(expect, dummy.expect, {})
          get(PyIR.Name) <- $FuncObj(get, dummy.get, {})
          n0 <- get(PyIR.Name)()
          n1 <- expect(PyIR.Name)(n0)
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
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.f:
        b0:
          n0 <- $Compare.eq(x(PyIR.Fast), y(PyIR.Fast))
          return n0 |}]


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
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.f:
        b0:
          if x(PyIR.Fast) then jmp b1 else jmp b2

        b1:
          if $Not(y(PyIR.Fast)) then jmp b2 else jmp b3(y(PyIR.Fast))

        b2:
          if z(PyIR.Fast) then jmp b4 else jmp b3(z(PyIR.Fast))

        b3:
          return n1

        b4:
          jmp b3(t(PyIR.Fast)) |}]


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
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.f:
        b0:
          n0 <- $Compare.gt(x(PyIR.Fast), y(PyIR.Fast))
          return n0 |}]


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
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.f:
        b0:
          n0 <- $Compare.le(x(PyIR.Fast), y(PyIR.Fast))
          return n0 |}]


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
          is_check(PyIR.Name) <- $FuncObj(is_check, dummy.is_check, {})
          is_not_check(PyIR.Name) <- $FuncObj(is_not_check, dummy.is_not_check, {})
          in_check(PyIR.Name) <- $FuncObj(in_check, dummy.in_check, {})
          in_not_check(PyIR.Name) <- $FuncObj(in_not_check, dummy.in_not_check, {})
          return PYCNone


      dummy.in_check:
        b0:
          n0 <- $Compare.in(x(PyIR.Fast), l(PyIR.Fast))
          return n0


      dummy.in_not_check:
        b0:
          n0 <- $Compare.not_in(x(PyIR.Fast), l(PyIR.Fast))
          return n0


      dummy.is_check:
        b0:
          n0 <- $Compare.is(x(PyIR.Fast), PYCNone)
          return n0


      dummy.is_not_check:
        b0:
          n0 <- $Compare.is_not(x(PyIR.Fast), PYCNone)
          return n0 |}]


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
          t(PyIR.Name) <- PYCTuple ([|PYCInt (1); PYCInt (2); PYCInt (3)|])
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.f:
        b0:
          return (x(PyIR.Fast), y(PyIR.Fast), z(PyIR.Fast)) |}]


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
          l(PyIR.Name) <- [PYCInt (1), PYCInt (2), PYCInt (3)]
          n0 <- print(PyIR.Name)(l(PyIR.Name))
          build_list(PyIR.Name) <- $FuncObj(build_list, dummy.build_list, {})
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
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.f:
        b0:
          n0 <- foo(PyIR.Fast)()
          n1 <- $LoadMethod(n0, __enter__)()
          foo0(PyIR.Fast) <- n1
          n3 <- bar(PyIR.Fast)()
          n4 <- $LoadMethod(n3, __enter__)()
          bar0(PyIR.Fast) <- n4
          n7 <- print(PyIR.Global)(bar0(PyIR.Fast))
          jmp b2(CM(n3).__exit__, CM(n0).__exit__)

        b1:
          n14 <- n2(PYCNone, PYCNone, PYCNone)
          return PYCNone

        b2:
          n10 <- n6(PYCNone, PYCNone, PYCNone)
          n11 <- print(PyIR.Global)(foo0(PyIR.Fast))
          jmp b3(n5)

        b3:
          n13 <- n12(PYCNone, PYCNone, PYCNone)
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
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          n0 <- f(PyIR.Name)()
          a(PyIR.Name) <- n0[PYCInt (0)]
          b(PyIR.Name) <- n0[PYCInt (1)]
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
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          n0 <- f(PyIR.Name)(PYCInt (0), y= PYCInt (2), x= PYCInt (1))
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
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.f:
        b0:
          n0 <- $Compare.not_in((a(PyIR.Fast), b(PyIR.Fast)), m(PyIR.Fast))
          if n0 then jmp b1 else jmp b2

        b1:
          n1 <- $Inplace.Subtract(b(PyIR.Fast), PYCInt (1))
          b(PyIR.Fast) <- n1
          jmp b0

        b2:
          n2 <- $Compare.not_in((a(PyIR.Fast), c(PyIR.Fast)), m(PyIR.Fast))
          if n2 then jmp b3 else jmp b4

        b3:
          n3 <- $Inplace.Add(c(PyIR.Fast), PYCInt (1))
          c(PyIR.Fast) <- n3
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
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          test_arguments(PyIR.Name) <- $FuncObj(test_arguments, dummy.test_arguments, {})
          return PYCNone


      dummy.f:
        b0:
          n0 <- $FormatFn.repr(name(PyIR.Fast))
          n1 <- $Format(n0, PYCNone)
          n2 <- $FormatFn.str(name(PyIR.Fast))
          n3 <- $Format(n2, PYCNone)
          n4 <- $FormatFn.ascii(name(PyIR.Fast))
          n5 <- $Format(n4, PYCNone)
          return $Concat(PYCString ("foo."), n1, n3, n5)


      dummy.test_arguments:
        b0:
          n0 <- $Binary.Multiply(x(PyIR.Fast), y(PyIR.Fast))
          n1 <- $Format(width(PyIR.Fast), PYCNone)
          n2 <- $Format(n0, n1)
          return $Concat(PYCString ("x="), n2) |}]


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
          pos(PyIR.Name) <- $FuncObj(pos, dummy.pos, {})
          neg(PyIR.Name) <- $FuncObj(neg, dummy.neg, {})
          test_not(PyIR.Name) <- $FuncObj(test_not, dummy.test_not, {})
          inv(PyIR.Name) <- $FuncObj(inv, dummy.inv, {})
          return PYCNone


      dummy.inv:
        b0:
          n0 <- $Unary.Invert(x(PyIR.Fast))
          return n0


      dummy.neg:
        b0:
          n0 <- $Unary.Negative(x(PyIR.Fast))
          return n0


      dummy.pos:
        b0:
          n0 <- $Unary.Positive(x(PyIR.Fast))
          return n0


      dummy.test_not:
        b0:
          n0 <- $Unary.Not(x(PyIR.Fast))
          return n0 |}]


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
          gx(PyIR.Global) <- PYCInt (100)
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          n0 <- f(PyIR.Name)(PYCInt (42))
          g(PyIR.Name) <- n0
          n1 <- g(PyIR.Name)()
          n2 <- print(PyIR.Name)(n1)
          return PYCNone


      dummy.f:
        b0:
          lx(PyIR.Deref) <- PYCInt (1000)
          inner(PyIR.Fast) <- $FuncObj(inner, dummy.f.inner, {})
          lx(PyIR.Deref) <- PYCInt (1664)
          return inner(PyIR.Fast)


      dummy.f.inner:
        b0:
          ix(PyIR.Fast) <- PYCInt (20)
          n0 <- print(PyIR.Global)(gx(PyIR.Global))
          n1 <- print(PyIR.Global)(ax(PyIR.Deref))
          n2 <- print(PyIR.Global)(lx(PyIR.Deref))
          n3 <- print(PyIR.Global)(ix(PyIR.Fast))
          gx(PyIR.Global) <- PYCInt (10)
          lx(PyIR.Deref) <- PYCInt (2)
          return lx(PyIR.Deref) |}]


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
          n0 <- $GetIter(l(PyIR.Name))
          n1 <- $FuncObj(<listcomp>, dummy.<listcomp>, {})(n0)
          g(PyIR.Name) <- n1
          n2 <- $GetIter(l(PyIR.Name))
          n3 <- $FuncObj(<listcomp>, dummy.<listcomp>, {})(n2)
          g0(PyIR.Name) <- n3
          n4 <- print(PyIR.Name)(g(PyIR.Name))
          n5 <- print(PyIR.Name)(g0(PyIR.Name))
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.<listcomp>:
        b0:
          jmp b1(.0(PyIR.Fast), [])

        b1:
          n2 <- $NextIter(n1)
          n3 <- $HasNextIter(n2)
          if n3 then jmp b2(n0) else jmp b3(n0)

        b2:
          n6 <- $IterData(n2)
          x(PyIR.Fast) <- n6
          n7 <- $Binary.Add(x(PyIR.Fast), PYCInt (2))
          n8 <- $ListAppend(n4, n7)
          jmp b1(n1, n4)

        b3:
          return n5


      dummy.f.<listcomp>:
        b0:
          jmp b1(.0(PyIR.Fast), [])

        b1:
          n2 <- $NextIter(n1)
          n3 <- $HasNextIter(n2)
          if n3 then jmp b2(n0) else jmp b3(n0)

        b2:
          n6 <- $IterData(n2)
          x(PyIR.Fast) <- n6
          n7 <- $Binary.Add(x(PyIR.Fast), PYCInt (2))
          n8 <- $ListAppend(n4, n7)
          jmp b1(n1, n4)

        b3:
          return n5


      dummy.f:
        b0:
          n0 <- $GetIter(l(PyIR.Fast))
          n1 <- $FuncObj(<listcomp>, dummy.f.<listcomp>, {})(n0)
          r(PyIR.Fast) <- n1
          n2 <- $GetIter(l(PyIR.Fast))
          n3 <- $FuncObj(<listcomp>, dummy.f.<listcomp>, {})(n2)
          r0(PyIR.Fast) <- n3
          n4 <- print(PyIR.Global)(r(PyIR.Fast))
          n5 <- print(PyIR.Global)(r0(PyIR.Fast))
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
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          g(PyIR.Name) <- $FuncObj(g, dummy.g, {})
          return PYCNone


      dummy.g.<dictcomp>:
        b0:
          jmp b1(.0(PyIR.Fast), {||})

        b1:
          n2 <- $NextIter(n1)
          n3 <- $HasNextIter(n2)
          if n3 then jmp b2(n0) else jmp b3(n0)

        b2:
          n6 <- $IterData(n2)
          num(PyIR.Fast) <- n6
          n7 <- $Binary.Power(num(PyIR.Fast), PYCInt (2))
          n8 <- $DictSetItem(n4, num(PyIR.Fast), n7)
          jmp b1(n1, n4)

        b3:
          return n5


      dummy.f.<setcomp>:
        b0:
          jmp b1(.0(PyIR.Fast), {})

        b1:
          n2 <- $NextIter(n1)
          n3 <- $HasNextIter(n2)
          if n3 then jmp b2(n0) else jmp b3(n0)

        b2:
          n6 <- $IterData(n2)
          x(PyIR.Fast) <- n6
          n7 <- $Binary.Add(x(PyIR.Fast), PYCInt (1))
          n8 <- $SetAdd(n4, n7)
          jmp b1(n1, n4)

        b3:
          return n5


      dummy.f:
        b0:
          n0 <- $GetIter(l(PyIR.Fast))
          n1 <- $FuncObj(<setcomp>, dummy.f.<setcomp>, {})(n0)
          r(PyIR.Fast) <- n1
          return r(PyIR.Fast)


      dummy.g:
        b0:
          n0 <- $GetIter(l(PyIR.Fast))
          n1 <- $FuncObj(<dictcomp>, dummy.g.<dictcomp>, {})(n0)
          squared_dict(PyIR.Fast) <- n1
          return r(PyIR.Global) |xxx}]


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
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          g(PyIR.Name) <- $FuncObj(g, dummy.g, {})
          return PYCNone


      dummy.f:
        b0:
          return PYCBool (true)


      dummy.g:
        b0:
          n0 <- f(PyIR.Global)()
          n1 <- $GetAwaitable(n0)
          n2 <- $YieldFrom(n1, PYCNone)
          if n1 then jmp b1 else jmp b2

        b1:
          n3 <- print(PyIR.Global)(PYCInt (0))
          jmp b3

        b2:
          n4 <- print(PyIR.Global)(PYCInt (1))
          jmp b3

        b3:
          return PYCNone |}]
