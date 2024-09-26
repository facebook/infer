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
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.f:
        b0:
          n0 <- $CallMethod($LoadMethod(LOCAL[kwargs], items), )
          n1 <- $GetIter(n0)
          jmp b1(n1)

        b1:
          n3 <- $NextIter(n2)
          n4 <- $HasNextIter(n3)
          if n4 then jmp b2 else jmp b3

        b2:
          n5 <- $IterData(n3)
          LOCAL[k] <- n5[PYCInt (0)]
          LOCAL[v] <- n5[PYCInt (1)]
          n6 <- GLOBAL[print](LOCAL[k], LOCAL[v])
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
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          TOPLEVEL[g] <- $FuncObj(g, dummy.g, {})
          TOPLEVEL[start] <- $FuncObj(start, dummy.start, {})
          n0 <- TOPLEVEL[start]()
          return PYCNone


      dummy.f:
        b0:
          n0 <- GLOBAL[print](PYCString ("dummy = "), LOCAL[dummy])
          n1 <- GLOBAL[print](PYCString ("dummy2= "), LOCAL[dummy2])
          n2 <- GLOBAL[print](PYCString ("dummy3= "), LOCAL[dummy3])
          n3 <- GLOBAL[print](PYCString ("dummy4= "), LOCAL[dummy4])
          n4 <- $CallMethod($LoadMethod(LOCAL[dummyA], items), )
          n5 <- $GetIter(n4)
          jmp b1(n5)

        b1:
          n7 <- $NextIter(n6)
          n8 <- $HasNextIter(n7)
          if n8 then jmp b2 else jmp b3

        b2:
          n9 <- $IterData(n7)
          LOCAL[k] <- n9[PYCInt (0)]
          LOCAL[v] <- n9[PYCInt (1)]
          n10 <- $CallMethod($LoadMethod(PYCString ("{} = {}"), format), LOCAL[k], LOCAL[v])
          n11 <- GLOBAL[print](n10)
          jmp b1(n6)

        b3:
          return PYCNone


      dummy.g:
        b0:
          n0 <- GLOBAL[print](PYCString ("dummy = "), LOCAL[dummy])
          n1 <- GLOBAL[print](PYCString ("dummy2= "), LOCAL[dummy2])
          n2 <- GLOBAL[print](PYCString ("dummy3= "), LOCAL[dummy3])
          n3 <- GLOBAL[print](PYCString ("dummy4= "), LOCAL[dummy4])
          return PYCNone


      dummy.start:
        b0:
          LOCAL[x] <- PYCTuple ([|PYCInt (3); PYCInt (4)|])
          n0 <- GLOBAL[f]($Packed((packed)($Packed(PYCTuple ([|PYCInt (1); PYCInt (2)|])), $Packed(LOCAL[x]))), $PackedMap({|
                          PYCString ("test"), PYCInt (42)|})) !packed
          n1 <- GLOBAL[f]($Packed((packed)($Packed(PYCTuple ([|PYCInt (1); PYCInt (2)|])), $Packed(
                                           PYCTuple ([|PYCString ("a"); PYCString ("b")|])))), $PackedMap({|
                          PYCString ("test"), PYCInt (42)|})) !packed
          n2 <- GLOBAL[g]($Packed((packed)($Packed(PYCTuple ([|PYCInt (1); PYCInt (2)|])), $Packed(LOCAL[x])))) !packed
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
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.f:
        b0:
          n0 <- $CallMethod($LoadMethod(LOCAL[foo], f), LOCAL[a])
          n1 <- LOCAL[foo].f($Packed(LOCAL[b])) !packed
          n2 <- LOCAL[foo].f($Packed((packed)($Packed((LOCAL[a])), $Packed(LOCAL[b])))) !packed
          n3 <- LOCAL[foo].f($Packed(()), $PackedMap(LOCAL[c])) !packed
          n4 <- LOCAL[foo].f($Packed(LOCAL[b]), $PackedMap(LOCAL[c])) !packed
          n5 <- LOCAL[foo].f($Packed((LOCAL[a])), $PackedMap(LOCAL[c])) !packed
          n6 <- LOCAL[foo].f($Packed((packed)($Packed((LOCAL[a])), $Packed(LOCAL[b]))), $PackedMap(LOCAL[c])) !packed
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
          TOPLEVEL[d0] <- {PYCInt (0): PYCInt (0), PYCInt (1): PYCInt (1), }
          TOPLEVEL[d1] <- {PYCString ("a"): PYCInt (0), PYCString ("b"): PYCInt (1), }
          TOPLEVEL[x] <- (packed){|$Packed(TOPLEVEL[d0]), $Packed(TOPLEVEL[d1])|}
          n0 <- TOPLEVEL[print](TOPLEVEL[x])
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          TOPLEVEL[d1] <- {PYCString ("a"): PYCInt (0), PYCString ("b"): PYCInt (1), }
          n1 <- TOPLEVEL[f]($Packed(()), $PackedMap((packed){|$Packed(TOPLEVEL[d1]), $Packed({|
                                                              PYCString ("x"),
                                                              PYCInt (42)|})|})) !packed
          return PYCNone


      dummy.f:
        b0:
          n0 <- GLOBAL[print](LOCAL[x])
          n1 <- $CallMethod($LoadMethod(LOCAL[kwargs], items), )
          n2 <- $GetIter(n1)
          jmp b1(n2)

        b1:
          n4 <- $NextIter(n3)
          n5 <- $HasNextIter(n4)
          if n5 then jmp b2 else jmp b3

        b2:
          n6 <- $IterData(n4)
          LOCAL[k] <- n6[PYCInt (0)]
          LOCAL[v] <- n6[PYCInt (1)]
          n7 <- GLOBAL[print](LOCAL[k], LOCAL[v])
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
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          n0 <- TOPLEVEL[f]()
          n1 <- $UnpackEx(PYCInt (2), PYCInt (3), n0)
          TOPLEVEL[a] <- n1[PYCInt (0)]
          TOPLEVEL[b] <- n1[PYCInt (1)]
          TOPLEVEL[lst] <- n1[PYCInt (2)]
          TOPLEVEL[x] <- n1[PYCInt (3)]
          TOPLEVEL[y] <- n1[PYCInt (4)]
          TOPLEVEL[z] <- n1[PYCInt (5)]
          n2 <- TOPLEVEL[print](TOPLEVEL[lst])
          return PYCNone


      dummy.f:
        b0:
          n0 <- GLOBAL[range](PYCInt (10))
          return n0 |}]
