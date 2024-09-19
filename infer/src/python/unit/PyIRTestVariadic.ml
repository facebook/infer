(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

(* Tests with variadic functions *)

let%expect_test _ =
  let source =
    {|
def f(**kwargs):
        for (k, v) in kwargs.items():
            print(k, v)
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
          n0 <- $CallMethod($LoadMethod(kwargs(PyIR.Fast), items), )
          n1 <- $GetIter(n0)
          jmp b1(n1)

        b1:
          n3 <- $NextIter(n2)
          n4 <- $HasNextIter(n3)
          if n4 then jmp b2 else jmp b3

        b2:
          n5 <- $IterData(n3)
          k(PyIR.Fast) <- n5[PYCInt (0)]
          v(PyIR.Fast) <- n5[PYCInt (1)]
          n6 <- print(PyIR.Global)(k(PyIR.Fast), v(PyIR.Fast))
          jmp b1(n2)

        b3:
          return PYCNone |}]


let%expect_test _ =
  let source =
    {|
def f(dummy, dummy2, dummy3, dummy4, **dummyA):
    print("dummy = ", dummy)
    print("dummy2= ", dummy2)
    print("dummy3= ", dummy3)
    print("dummy4= ", dummy4)
    for (k, v) in dummyA.items():
        print("{} = {}".format(k, v))

def g(dummy, dummy2, dummy3, dummy4):
    print("dummy = ", dummy)
    print("dummy2= ", dummy2)
    print("dummy3= ", dummy3)
    print("dummy4= ", dummy4)

def start():
    x = (3, 4)
    f(*(1, 2), *x, **{"test": 42})
    f(*(1, 2), 'a', 'b', **{"test": 42})
    g(*(1, 2), *x)

start()
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
          start(PyIR.Name) <- $FuncObj(start, dummy.start, {})
          n0 <- start(PyIR.Name)()
          return PYCNone


      dummy.f:
        b0:
          n0 <- print(PyIR.Global)(PYCString ("dummy = "), dummy(PyIR.Fast))
          n1 <- print(PyIR.Global)(PYCString ("dummy2= "), dummy2(PyIR.Fast))
          n2 <- print(PyIR.Global)(PYCString ("dummy3= "), dummy3(PyIR.Fast))
          n3 <- print(PyIR.Global)(PYCString ("dummy4= "), dummy4(PyIR.Fast))
          n4 <- $CallMethod($LoadMethod(dummyA(PyIR.Fast), items), )
          n5 <- $GetIter(n4)
          jmp b1(n5)

        b1:
          n7 <- $NextIter(n6)
          n8 <- $HasNextIter(n7)
          if n8 then jmp b2 else jmp b3

        b2:
          n9 <- $IterData(n7)
          k(PyIR.Fast) <- n9[PYCInt (0)]
          v(PyIR.Fast) <- n9[PYCInt (1)]
          n10 <- $CallMethod($LoadMethod(PYCString ("{} = {}"), format), k(PyIR.Fast), v(PyIR.Fast))
          n11 <- print(PyIR.Global)(n10)
          jmp b1(n6)

        b3:
          return PYCNone


      dummy.g:
        b0:
          n0 <- print(PyIR.Global)(PYCString ("dummy = "), dummy(PyIR.Fast))
          n1 <- print(PyIR.Global)(PYCString ("dummy2= "), dummy2(PyIR.Fast))
          n2 <- print(PyIR.Global)(PYCString ("dummy3= "), dummy3(PyIR.Fast))
          n3 <- print(PyIR.Global)(PYCString ("dummy4= "), dummy4(PyIR.Fast))
          return PYCNone


      dummy.start:
        b0:
          x(PyIR.Fast) <- PYCTuple ([|PYCInt (3); PYCInt (4)|])
          n0 <- f(PyIR.Global)($Packed((packed)($Packed(PYCTuple ([|PYCInt (1); PYCInt (2)|])), $Packed(x(PyIR.Fast)))), $PackedMap({|
                               PYCString ("test"), PYCInt (42)|})) !packed
          n1 <- f(PyIR.Global)($Packed((packed)($Packed(PYCTuple ([|PYCInt (1); PYCInt (2)|])), $Packed(
                                                PYCTuple ([|PYCString ("a"); PYCString ("b")|])))), $PackedMap({|
                               PYCString ("test"), PYCInt (42)|})) !packed
          n2 <- g(PyIR.Global)($Packed((packed)($Packed(PYCTuple ([|PYCInt (1); PYCInt (2)|])), $Packed(x(PyIR.Fast))))) !packed
          return PYCNone |xxx}]


let%expect_test _ =
  let source =
    {|
def f(foo, a, b, c):
    foo.f(a)

    foo.f(*b)
    foo.f(a, *b)

    foo.f(**c)
    foo.f(*b, **c)
    foo.f(a, **c)
    foo.f(a, *b, **c)
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
          n0 <- $CallMethod($LoadMethod(foo(PyIR.Fast), f), a(PyIR.Fast))
          n1 <- foo(PyIR.Fast).f($Packed(b(PyIR.Fast))) !packed
          n2 <- foo(PyIR.Fast).f($Packed((packed)($Packed((a(PyIR.Fast))), $Packed(b(PyIR.Fast))))) !packed
          n3 <- foo(PyIR.Fast).f($Packed(()), $PackedMap(c(PyIR.Fast))) !packed
          n4 <- foo(PyIR.Fast).f($Packed(b(PyIR.Fast)), $PackedMap(c(PyIR.Fast))) !packed
          n5 <- foo(PyIR.Fast).f($Packed((a(PyIR.Fast))), $PackedMap(c(PyIR.Fast))) !packed
          n6 <- foo(PyIR.Fast).f($Packed((packed)($Packed((a(PyIR.Fast))), $Packed(b(PyIR.Fast)))), $PackedMap(c(PyIR.Fast))) !packed
          return PYCNone |}]


let%expect_test _ =
  let source =
    {|
d0 = {0: 0, 1:1}
d1 = {'a': 0, 'b': 1}
x = {**d0, **d1}
print(x)

def f(x, **kwargs):
    print(x)
    for (k, v) in kwargs.items():
        print(k, v)

d1 = {'a': 0, 'b': 1}
f(**d1, x=42)
          |}
  in
  PyIR.test source ;
  [%expect
    {xxx|
    module dummy:

      toplevel:
        b0:
          d0(PyIR.Name) <- {PYCInt (0): PYCInt (0), PYCInt (1): PYCInt (1), }
          d1(PyIR.Name) <- {PYCString ("a"): PYCInt (0), PYCString ("b"): PYCInt (1), }
          x(PyIR.Name) <- (packed){|$Packed(d0(PyIR.Name)), $Packed(d1(PyIR.Name))|}
          n0 <- print(PyIR.Name)(x(PyIR.Name))
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          d1(PyIR.Name) <- {PYCString ("a"): PYCInt (0), PYCString ("b"): PYCInt (1), }
          n1 <- f(PyIR.Name)($Packed(()), $PackedMap((packed){|$Packed(d1(PyIR.Name)), $Packed({|
                                                               PYCString ("x"),
                                                               PYCInt (42)|})|})) !packed
          return PYCNone


      dummy.f:
        b0:
          n0 <- print(PyIR.Global)(x(PyIR.Fast))
          n1 <- $CallMethod($LoadMethod(kwargs(PyIR.Fast), items), )
          n2 <- $GetIter(n1)
          jmp b1(n2)

        b1:
          n4 <- $NextIter(n3)
          n5 <- $HasNextIter(n4)
          if n5 then jmp b2 else jmp b3

        b2:
          n6 <- $IterData(n4)
          k(PyIR.Fast) <- n6[PYCInt (0)]
          v(PyIR.Fast) <- n6[PYCInt (1)]
          n7 <- print(PyIR.Global)(k(PyIR.Fast), v(PyIR.Fast))
          jmp b1(n3)

        b3:
          return PYCNone |xxx}]


let%expect_test _ =
  let source =
    {|
def f():
          return range(10)

(a, b, *lst, x, y, z) = f()
print(lst) # [2, 3, 4, 5, 6]
          |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          n0 <- f(PyIR.Name)()
          n1 <- $UnpackEx(PYCInt (2), PYCInt (3), n0)
          a(PyIR.Name) <- n1[PYCInt (0)]
          b(PyIR.Name) <- n1[PYCInt (1)]
          lst(PyIR.Name) <- n1[PYCInt (2)]
          x(PyIR.Name) <- n1[PYCInt (3)]
          y(PyIR.Name) <- n1[PYCInt (4)]
          z(PyIR.Name) <- n1[PYCInt (5)]
          n2 <- print(PyIR.Name)(lst(PyIR.Name))
          return PYCNone


      dummy.f:
        b0:
          n0 <- range(PyIR.Global)(PYCInt (10))
          return n0 |}]
