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
          n0 <- TOPLEVEL[my_fun](PYCInt (42), TOPLEVEL[a])
          TOPLEVEL[z] <- n0
          n1 <- TOPLEVEL[print](TOPLEVEL[z])
          return PYCNone


      dummy.my_fun:
        b0:
          n0 <- GLOBAL[print](LOCAL[x])
          n1 <- GLOBAL[print](LOCAL[y])
          n2 <- $Binary.Add(LOCAL[x], LOCAL[y])
          LOCAL[z] <- n2
          return LOCAL[z] |}]


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
          n0 <- TOPLEVEL[update_global]()
          n1 <- TOPLEVEL[print](GLOBAL[z])
          return PYCNone


      dummy.update_global:
        b0:
          n0 <- $Binary.Add(GLOBAL[z], PYCInt (1))
          GLOBAL[z] <- n0
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
          n0 <- GLOBAL[coin]()
          if n0 then jmp b1 else jmp b2

        b1:
          return LOCAL[x]

        b2:
          return LOCAL[y] |}]


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
          n0 <- GLOBAL[coin]()
          if n0 then jmp b1 else jmp b2

        b1:
          LOCAL[z] <- LOCAL[x]
          jmp b3

        b2:
          LOCAL[z] <- LOCAL[y]
          jmp b3

        b3:
          return LOCAL[z] |}]


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
          n0 <- GLOBAL[coin]()
          if n0 then jmp b1 else jmp b2

        b1:
          n1 <- GLOBAL[coin]()
          if n1 then jmp b3 else jmp b4

        b2:
          n3 <- $Binary.Add(LOCAL[z], PYCInt (1))
          LOCAL[z] <- n3
          n4 <- GLOBAL[coin]()
          if n4 then jmp b7 else jmp b8

        b3:
          LOCAL[z] <- LOCAL[x]
          jmp b5

        b4:
          return PYCInt (1664)

        b5:
          n2 <- $Binary.Add(LOCAL[z], PYCInt (1))
          LOCAL[z] <- n2
          jmp b6

        b6:
          return LOCAL[z]

        b7:
          return PYCInt (42)

        b8:
          LOCAL[z] <- LOCAL[y]
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
          TOPLEVEL[foo] <- $FuncObj(foo, dummy.foo, {})
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.f:
        b0:
          if LOCAL[x] then jmp b1(GLOBAL[foo]) else jmp b2(GLOBAL[foo])

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
          n0 <- TOPLEVEL[range](PYCInt (10))
          n1 <- $GetIter(n0)
          jmp b1(n1)

        b1:
          n3 <- $NextIter(n2)
          n4 <- $HasNextIter(n3)
          if n4 then jmp b2 else jmp b3

        b2:
          n5 <- $IterData(n3)
          TOPLEVEL[x] <- n5
          n6 <- TOPLEVEL[print](TOPLEVEL[x])
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
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.f:
        b0:
          n0 <- $GetIter(LOCAL[l])
          jmp b1(n0)

        b1:
          n2 <- $NextIter(n1)
          n3 <- $HasNextIter(n2)
          if n3 then jmp b2 else jmp b3

        b2:
          n4 <- $IterData(n2)
          LOCAL[x] <- n4
          n5 <- LOCAL[bar]()
          n6 <- $LoadMethod(n5, __enter__)()
          n9 <- LOCAL[toto]()
          n10 <- $LoadMethod(n9, __enter__)()
          LOCAL[obj] <- n10
          if LOCAL[y] then jmp b6(CM(n9).__exit__, CM(n5).__exit__, n1) else
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
          n28 <- GLOBAL[print](PYCString ("nop"))
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
          n0 <- TOPLEVEL[print](PYCInt (42))
          TOPLEVEL[print] <- $FuncObj(print, dummy.print, {})
          n1 <- TOPLEVEL[print](PYCInt (42))
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.f:
        b0:
          n0 <- GLOBAL[print](LOCAL[x])
          return PYCNone


      dummy.print:
        b0:
          return LOCAL[x] |}]


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
          TOPLEVEL[f0] <- $FuncObj(f0, dummy.f0, {})
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
          TOPLEVEL[expect_int] <- $FuncObj(expect_int, dummy.expect_int, {})
          TOPLEVEL[get] <- $FuncObj(get, dummy.get, {})
          n0 <- TOPLEVEL[get]()
          n1 <- TOPLEVEL[expect_int](n0)
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
          TOPLEVEL[expect] <- $FuncObj(expect, dummy.expect, {})
          TOPLEVEL[get] <- $FuncObj(get, dummy.get, {})
          n0 <- TOPLEVEL[get]()
          n1 <- TOPLEVEL[expect](n0)
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
          n0 <- $Compare.eq(LOCAL[x], LOCAL[y])
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
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.f:
        b0:
          if LOCAL[x] then jmp b1 else jmp b2

        b1:
          if $Not(LOCAL[y]) then jmp b2 else jmp b3(LOCAL[y])

        b2:
          if LOCAL[z] then jmp b4 else jmp b3(LOCAL[z])

        b3:
          return n1

        b4:
          jmp b3(LOCAL[t]) |}]


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
          n0 <- $Compare.gt(LOCAL[x], LOCAL[y])
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
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.f:
        b0:
          n0 <- $Compare.le(LOCAL[x], LOCAL[y])
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
          TOPLEVEL[is_check] <- $FuncObj(is_check, dummy.is_check, {})
          TOPLEVEL[is_not_check] <- $FuncObj(is_not_check, dummy.is_not_check, {})
          TOPLEVEL[in_check] <- $FuncObj(in_check, dummy.in_check, {})
          TOPLEVEL[in_not_check] <- $FuncObj(in_not_check, dummy.in_not_check, {})
          return PYCNone


      dummy.in_check:
        b0:
          n0 <- $Compare.in(LOCAL[x], LOCAL[l])
          return n0


      dummy.in_not_check:
        b0:
          n0 <- $Compare.not_in(LOCAL[x], LOCAL[l])
          return n0


      dummy.is_check:
        b0:
          n0 <- $Compare.is(LOCAL[x], PYCNone)
          return n0


      dummy.is_not_check:
        b0:
          n0 <- $Compare.is_not(LOCAL[x], PYCNone)
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
          TOPLEVEL[t] <- PYCTuple ([|PYCInt (1); PYCInt (2); PYCInt (3)|])
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.f:
        b0:
          return (LOCAL[x], LOCAL[y], LOCAL[z]) |}]


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
          n0 <- TOPLEVEL[print](TOPLEVEL[l])
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
          n0 <- LOCAL[foo]()
          n1 <- $LoadMethod(n0, __enter__)()
          LOCAL[foo0] <- n1
          n3 <- LOCAL[bar]()
          n4 <- $LoadMethod(n3, __enter__)()
          LOCAL[bar0] <- n4
          n7 <- GLOBAL[print](LOCAL[bar0])
          jmp b2(CM(n3).__exit__, CM(n0).__exit__)

        b1:
          n14 <- n2(PYCNone, PYCNone, PYCNone)
          return PYCNone

        b2:
          n10 <- n6(PYCNone, PYCNone, PYCNone)
          n11 <- GLOBAL[print](LOCAL[foo0])
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
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          n0 <- TOPLEVEL[f]()
          TOPLEVEL[a] <- n0[PYCInt (0)]
          TOPLEVEL[b] <- n0[PYCInt (1)]
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
          n0 <- TOPLEVEL[f](PYCInt (0), y= PYCInt (2), x= PYCInt (1))
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
          n0 <- $Compare.not_in((LOCAL[a], LOCAL[b]), LOCAL[m])
          if n0 then jmp b1 else jmp b2

        b1:
          n1 <- $Inplace.Subtract(LOCAL[b], PYCInt (1))
          LOCAL[b] <- n1
          jmp b0

        b2:
          n2 <- $Compare.not_in((LOCAL[a], LOCAL[c]), LOCAL[m])
          if n2 then jmp b3 else jmp b4

        b3:
          n3 <- $Inplace.Add(LOCAL[c], PYCInt (1))
          LOCAL[c] <- n3
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
          n0 <- $FormatFn.repr(LOCAL[name])
          n1 <- $Format(n0, PYCNone)
          n2 <- $FormatFn.str(LOCAL[name])
          n3 <- $Format(n2, PYCNone)
          n4 <- $FormatFn.ascii(LOCAL[name])
          n5 <- $Format(n4, PYCNone)
          return $Concat(PYCString ("foo."), n1, n3, n5)


      dummy.test_arguments:
        b0:
          n0 <- $Binary.Multiply(LOCAL[x], LOCAL[y])
          n1 <- $Format(LOCAL[width], PYCNone)
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
          TOPLEVEL[pos] <- $FuncObj(pos, dummy.pos, {})
          TOPLEVEL[neg] <- $FuncObj(neg, dummy.neg, {})
          TOPLEVEL[test_not] <- $FuncObj(test_not, dummy.test_not, {})
          TOPLEVEL[inv] <- $FuncObj(inv, dummy.inv, {})
          return PYCNone


      dummy.inv:
        b0:
          n0 <- $Unary.Invert(LOCAL[x])
          return n0


      dummy.neg:
        b0:
          n0 <- $Unary.Negative(LOCAL[x])
          return n0


      dummy.pos:
        b0:
          n0 <- $Unary.Positive(LOCAL[x])
          return n0


      dummy.test_not:
        b0:
          n0 <- $Unary.Not(LOCAL[x])
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
          GLOBAL[gx] <- PYCInt (100)
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          n0 <- TOPLEVEL[f](PYCInt (42))
          TOPLEVEL[g] <- n0
          n1 <- TOPLEVEL[g]()
          n2 <- TOPLEVEL[print](n1)
          return PYCNone


      dummy.f:
        b0:
          DEREF[lx] <- PYCInt (1000)
          LOCAL[inner] <- $FuncObj(inner, dummy.f.inner, {})
          DEREF[lx] <- PYCInt (1664)
          return LOCAL[inner]


      dummy.f.inner:
        b0:
          LOCAL[ix] <- PYCInt (20)
          n0 <- GLOBAL[print](GLOBAL[gx])
          n1 <- GLOBAL[print](DEREF[ax])
          n2 <- GLOBAL[print](DEREF[lx])
          n3 <- GLOBAL[print](LOCAL[ix])
          GLOBAL[gx] <- PYCInt (10)
          DEREF[lx] <- PYCInt (2)
          return DEREF[lx] |}]


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
          n0 <- $GetIter(TOPLEVEL[l])
          n1 <- $FuncObj(<listcomp>, dummy.<listcomp>, {})(n0)
          TOPLEVEL[g] <- n1
          n2 <- $GetIter(TOPLEVEL[l])
          n3 <- $FuncObj(<listcomp>, dummy.<listcomp>, {})(n2)
          TOPLEVEL[g0] <- n3
          n4 <- TOPLEVEL[print](TOPLEVEL[g])
          n5 <- TOPLEVEL[print](TOPLEVEL[g0])
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.<listcomp>:
        b0:
          jmp b1(LOCAL[.0], [])

        b1:
          n2 <- $NextIter(n1)
          n3 <- $HasNextIter(n2)
          if n3 then jmp b2(n0) else jmp b3(n0)

        b2:
          n6 <- $IterData(n2)
          LOCAL[x] <- n6
          n7 <- $Binary.Add(LOCAL[x], PYCInt (2))
          n8 <- $ListAppend(n4, n7)
          jmp b1(n1, n4)

        b3:
          return n5


      dummy.f.<listcomp>:
        b0:
          jmp b1(LOCAL[.0], [])

        b1:
          n2 <- $NextIter(n1)
          n3 <- $HasNextIter(n2)
          if n3 then jmp b2(n0) else jmp b3(n0)

        b2:
          n6 <- $IterData(n2)
          LOCAL[x] <- n6
          n7 <- $Binary.Add(LOCAL[x], PYCInt (2))
          n8 <- $ListAppend(n4, n7)
          jmp b1(n1, n4)

        b3:
          return n5


      dummy.f:
        b0:
          n0 <- $GetIter(LOCAL[l])
          n1 <- $FuncObj(<listcomp>, dummy.f.<listcomp>, {})(n0)
          LOCAL[r] <- n1
          n2 <- $GetIter(LOCAL[l])
          n3 <- $FuncObj(<listcomp>, dummy.f.<listcomp>, {})(n2)
          LOCAL[r0] <- n3
          n4 <- GLOBAL[print](LOCAL[r])
          n5 <- GLOBAL[print](LOCAL[r0])
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
          jmp b1(LOCAL[.0], {||})

        b1:
          n2 <- $NextIter(n1)
          n3 <- $HasNextIter(n2)
          if n3 then jmp b2(n0) else jmp b3(n0)

        b2:
          n6 <- $IterData(n2)
          LOCAL[num] <- n6
          n7 <- $Binary.Power(LOCAL[num], PYCInt (2))
          n8 <- $DictSetItem(n4, LOCAL[num], n7)
          jmp b1(n1, n4)

        b3:
          return n5


      dummy.f.<setcomp>:
        b0:
          jmp b1(LOCAL[.0], {})

        b1:
          n2 <- $NextIter(n1)
          n3 <- $HasNextIter(n2)
          if n3 then jmp b2(n0) else jmp b3(n0)

        b2:
          n6 <- $IterData(n2)
          LOCAL[x] <- n6
          n7 <- $Binary.Add(LOCAL[x], PYCInt (1))
          n8 <- $SetAdd(n4, n7)
          jmp b1(n1, n4)

        b3:
          return n5


      dummy.f:
        b0:
          n0 <- $GetIter(LOCAL[l])
          n1 <- $FuncObj(<setcomp>, dummy.f.<setcomp>, {})(n0)
          LOCAL[r] <- n1
          return LOCAL[r]


      dummy.g:
        b0:
          n0 <- $GetIter(LOCAL[l])
          n1 <- $FuncObj(<dictcomp>, dummy.g.<dictcomp>, {})(n0)
          LOCAL[squared_dict] <- n1
          return GLOBAL[r] |xxx}]


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
          n0 <- GLOBAL[f]()
          n1 <- $GetAwaitable(n0)
          n2 <- $YieldFrom(n1, PYCNone)
          if n1 then jmp b1 else jmp b2

        b1:
          n3 <- GLOBAL[print](PYCInt (0))
          jmp b3

        b2:
          n4 <- GLOBAL[print](PYCInt (1))
          jmp b3

        b3:
          return PYCNone |}]
