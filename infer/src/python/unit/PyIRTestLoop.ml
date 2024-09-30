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

        b1:
          n4 <- $NextIter(n3)
          n5 <- $HasNextIter(n4)
          if n5 then jmp b2 else jmp b3

        b2:
          n6 <- $IterData(n4)
          TOPLEVEL[x] <- n6
          n7 <- TOPLEVEL[print]
          n8 <- TOPLEVEL[x]
          n9 <- n7(n8)
          jmp b1(n3)

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

        b1:
          n3 <- $NextIter(n2)
          n4 <- $HasNextIter(n3)
          if n4 then jmp b2 else jmp b3

        b2:
          n5 <- $IterData(n3)
          LOCAL[x] <- n5
          n6 <- LOCAL[bar]
          n7 <- n6()
          n8 <- n7.__enter__()
          n11 <- LOCAL[toto]
          n12 <- n11()
          n13 <- n12.__enter__()
          LOCAL[obj] <- n13
          n17 <- LOCAL[y]
          if n17 then jmp b6(CM(n12).__exit__, CM(n7).__exit__, n2) else
          jmp b7(CM(n12).__exit__, CM(n7).__exit__, n2)

        b3:
          return PYCNone

        b4:
          n40 <- n10(PYCNone, PYCNone, PYCNone)
          jmp b1(n9)

        b5:
          n37 <- n16(PYCNone, PYCNone, PYCNone)
          jmp b4(n15, n14)

        b6:
          jmp b8(n20, n19, n18)

        b7:
          n32 <- GLOBAL[print]
          n33 <- n32(PYCString ("nop"))
          jmp b5(n23, n22, n21)

        b8:
          n27 <- n26(PYCNone, PYCNone, PYCNone)
          jmp b9(n25, n24)

        b9:
          n30 <- n29(PYCNone, PYCNone, PYCNone)
          jmp b1(n28) |}]


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

        b1:
          n3 <- $NextIter(n2)
          n4 <- $HasNextIter(n3)
          if n4 then jmp b2 else jmp b3

        b2:
          n5 <- $IterData(n3)
          LOCAL[item] <- n5
          n6 <- LOCAL[it]
          n7 <- LOCAL[n]
          n8 <- n6[n7]
          n9 <- LOCAL[item]
          n10 <- $Compare.eq(n8, n9)
          if $Not(n10) then jmp b4(n2) else jmp b5(n2)

        b3:
          return PYCNone

        b4:
          n13 <- GLOBAL[AssertionError]
          throw n13

        b5:
          n14 <- LOCAL[n]
          n15 <- $Inplace.Add(n14, PYCInt (1))
          LOCAL[n] <- n15
          jmp b1(n12) |}]


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

        b1:
          n3 <- $NextIter(n2)
          n4 <- $HasNextIter(n3)
          if n4 then jmp b2 else jmp b3

        b2:
          n5 <- $IterData(n3)
          LOCAL[path] <- n5
          n6 <- LOCAL[path]
          if n6 then jmp b4(n2) else jmp b1(n2)

        b3:
          return PYCNone

        b4:
          return PYCNone |}]
