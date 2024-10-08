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
          return None


      dummy.f:
        b0:
          n0 <- LOCAL[kwargs]
          n1 <- n0.items()
          n2 <- $GetIter(n1)
          jmp b1(n2)

        b1(n3):
          n4 <- $NextIter(n3)
          n5 <- $HasNextIter(n3)
          if n5 then jmp b2 else jmp b3

        b2:
          LOCAL[k] <- n4[0]
          LOCAL[v] <- n4[1]
          n6 <- GLOBAL[print]
          n7 <- LOCAL[k]
          n8 <- LOCAL[v]
          n9 <- n6(n7, n8)
          jmp b1(n3)

        b3:
          return None |}]


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
          n0 <- TOPLEVEL[start]
          n1 <- n0()
          return None


      dummy.f:
        b0:
          n0 <- GLOBAL[print]
          n1 <- LOCAL[dummy]
          n2 <- n0("dummy = ", n1)
          n3 <- GLOBAL[print]
          n4 <- LOCAL[dummy2]
          n5 <- n3("dummy2= ", n4)
          n6 <- GLOBAL[print]
          n7 <- LOCAL[dummy3]
          n8 <- n6("dummy3= ", n7)
          n9 <- GLOBAL[print]
          n10 <- LOCAL[dummy4]
          n11 <- n9("dummy4= ", n10)
          n12 <- LOCAL[dummyA]
          n13 <- n12.items()
          n14 <- $GetIter(n13)
          jmp b1(n14)

        b1(n15):
          n16 <- $NextIter(n15)
          n17 <- $HasNextIter(n15)
          if n17 then jmp b2 else jmp b3

        b2:
          LOCAL[k] <- n16[0]
          LOCAL[v] <- n16[1]
          n18 <- GLOBAL[print]
          n19 <- LOCAL[k]
          n20 <- LOCAL[v]
          n21 <- "{} = {}".format(n19, n20)
          n22 <- n18(n21)
          jmp b1(n15)

        b3:
          return None


      dummy.g:
        b0:
          n0 <- GLOBAL[print]
          n1 <- LOCAL[dummy]
          n2 <- n0("dummy = ", n1)
          n3 <- GLOBAL[print]
          n4 <- LOCAL[dummy2]
          n5 <- n3("dummy2= ", n4)
          n6 <- GLOBAL[print]
          n7 <- LOCAL[dummy3]
          n8 <- n6("dummy3= ", n7)
          n9 <- GLOBAL[print]
          n10 <- LOCAL[dummy4]
          n11 <- n9("dummy4= ", n10)
          return None


      dummy.start:
        b0:
          LOCAL[x] <- (3,4)
          n0 <- GLOBAL[f]
          n1 <- LOCAL[x]
          n2 <- n0($Packed((packed)($Packed((1,2)), $Packed(n1))), $PackedMap({|"test", 42|})) !packed
          n3 <- GLOBAL[f]
          n4 <- n3($Packed((packed)($Packed((1,2)), $Packed(("a","b")))), $PackedMap({|"test", 42|})) !packed
          n5 <- GLOBAL[g]
          n6 <- LOCAL[x]
          n7 <- n5($Packed((packed)($Packed((1,2)), $Packed(n6)))) !packed
          return None |xxx}]


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
          return None


      dummy.f:
        b0:
          n0 <- LOCAL[foo]
          n1 <- LOCAL[a]
          n2 <- n0.f(n1)
          n3 <- LOCAL[foo]
          n4 <- n3.f
          n5 <- LOCAL[b]
          n6 <- n4($Packed(n5)) !packed
          n7 <- LOCAL[foo]
          n8 <- n7.f
          n9 <- LOCAL[a]
          n10 <- LOCAL[b]
          n11 <- n8($Packed((packed)($Packed((n9)), $Packed(n10)))) !packed
          n12 <- LOCAL[foo]
          n13 <- n12.f
          n14 <- LOCAL[c]
          n15 <- n13($Packed(()), $PackedMap(n14)) !packed
          n16 <- LOCAL[foo]
          n17 <- n16.f
          n18 <- LOCAL[b]
          n19 <- LOCAL[c]
          n20 <- n17($Packed(n18), $PackedMap(n19)) !packed
          n21 <- LOCAL[foo]
          n22 <- n21.f
          n23 <- LOCAL[a]
          n24 <- LOCAL[c]
          n25 <- n22($Packed((n23)), $PackedMap(n24)) !packed
          n26 <- LOCAL[foo]
          n27 <- n26.f
          n28 <- LOCAL[a]
          n29 <- LOCAL[b]
          n30 <- LOCAL[c]
          n31 <- n27($Packed((packed)($Packed((n28)), $Packed(n29))), $PackedMap(n30)) !packed
          return None |}]


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
          TOPLEVEL[d0] <- {0: 0, 1: 1, }
          TOPLEVEL[d1] <- {"a": 0, "b": 1, }
          n0 <- TOPLEVEL[d0]
          n1 <- TOPLEVEL[d1]
          TOPLEVEL[x] <- (packed){|$Packed(n0), $Packed(n1)|}
          n2 <- TOPLEVEL[print]
          n3 <- TOPLEVEL[x]
          n4 <- n2(n3)
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          TOPLEVEL[d1] <- {"a": 0, "b": 1, }
          n5 <- TOPLEVEL[f]
          n6 <- TOPLEVEL[d1]
          n7 <- n5($Packed(()), $PackedMap((packed){|$Packed(n6), $Packed({|"x", 42|})|})) !packed
          return None


      dummy.f:
        b0:
          n0 <- GLOBAL[print]
          n1 <- LOCAL[x]
          n2 <- n0(n1)
          n3 <- LOCAL[kwargs]
          n4 <- n3.items()
          n5 <- $GetIter(n4)
          jmp b1(n5)

        b1(n6):
          n7 <- $NextIter(n6)
          n8 <- $HasNextIter(n6)
          if n8 then jmp b2 else jmp b3

        b2:
          LOCAL[k] <- n7[0]
          LOCAL[v] <- n7[1]
          n9 <- GLOBAL[print]
          n10 <- LOCAL[k]
          n11 <- LOCAL[v]
          n12 <- n9(n10, n11)
          jmp b1(n6)

        b3:
          return None |xxx}]


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
          n0 <- TOPLEVEL[f]
          n1 <- n0()
          n2 <- $UnpackEx(2, 3, n1)
          TOPLEVEL[a] <- n2[0]
          TOPLEVEL[b] <- n2[1]
          TOPLEVEL[lst] <- n2[2]
          TOPLEVEL[x] <- n2[3]
          TOPLEVEL[y] <- n2[4]
          TOPLEVEL[z] <- n2[5]
          n3 <- TOPLEVEL[print]
          n4 <- TOPLEVEL[lst]
          n5 <- n3(n4)
          return None


      dummy.f:
        b0:
          n0 <- GLOBAL[range]
          n1 <- n0(10)
          return n1 |}]
