(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

(* Tests with loops *)

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
          n5 <- $HasNextIter(n4)
          if n5 then jmp b2(n6, n3) else jmp b3

        b2(n7, n8):
          TOPLEVEL[x] <- n8
          n9 <- TOPLEVEL[print]
          n10 <- TOPLEVEL[x]
          n11 <- n9(n10)
          jmp b1(n7)

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
          n4 <- $HasNextIter(n3)
          if n4 then jmp b2(n5, n2) else jmp b7

        b2(n6, n7):
          LOCAL[x] <- n7
          n8 <- LOCAL[bar]
          n9 <- n8()
          n10 <- n9.__enter__()
          n11 <- LOCAL[toto]
          n12 <- n11()
          n13 <- n12.__enter__()
          LOCAL[obj] <- n13
          n14 <- LOCAL[y]
          if n14 then jmp b3(CM(n12).__exit__, CM(n9).__exit__, n6) else
          jmp b4(CM(n12).__exit__, CM(n9).__exit__, n6)

        b3(n15, n16, n17):
          n18 <- PYCNone(PYCNone, PYCNone, PYCNone)
          n19 <- PYCNone(PYCNone, PYCNone, PYCNone)
          jmp b1(n17, n16, n15)

        b4(n20, n21, n22):
          n23 <- GLOBAL[print]
          n24 <- n23(PYCString ("nop"))
          jmp b5(PYCNone, n22, n21, n20)

        b5(n25, n26, n27, n28):
          n29 <- n28(PYCNone, PYCNone, PYCNone)
          jmp b6(PYCNone, n26, n25)

        b6(n30, n31, n32):
          n33 <- n32(PYCNone, PYCNone, PYCNone)
          jmp b1(n30)

        b7:
          return PYCNone |}]


let%expect_test _ =
  let source =
    {|
def f(match, it, n):
    for item in match:
        if not it[n]==item: raise AssertionError
        n+=1
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
          n0 <- LOCAL[match]
          n1 <- $GetIter(n0)
          jmp b1(n1)

        b1(n2):
          n3 <- $NextIter(n2)
          n4 <- $HasNextIter(n3)
          if n4 then jmp b2(n5, n2) else jmp b5

        b2(n6, n7):
          LOCAL[item] <- n7
          n8 <- LOCAL[it]
          n9 <- LOCAL[n]
          n10 <- n8[n9]
          n11 <- LOCAL[item]
          n12 <- $Compare.eq(n10, n11)
          if $Not(n12) then jmp b3(n6) else jmp b4(n6)

        b3(n13):
          n14 <- GLOBAL[AssertionError]
          throw n14

        b4(n15):
          n16 <- LOCAL[n]
          n17 <- $Inplace.Add(n16, PYCInt (1))
          LOCAL[n] <- n17
          jmp b1(n15)

        b5:
          return PYCNone |}]


let%expect_test _ =
  let source =
    {|
def f(foo):
    for path in foo:
        if path:
                return
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
          n1 <- $GetIter(n0)
          jmp b1(n1)

        b1(n2):
          n3 <- $NextIter(n2)
          n4 <- $HasNextIter(n3)
          if n4 then jmp b2(n5, n2) else jmp b5

        b2(n6, n7):
          LOCAL[path] <- n7
          n8 <- LOCAL[path]
          if n8 then jmp b3(n6) else jmp b1(n6)

        b3(n9):
          return PYCNone

        b5:
          return PYCNone |}]
