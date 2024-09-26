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
          n0 <- $GetIter(LOCAL[match])
          jmp b1(n0)

        b1:
          n2 <- $NextIter(n1)
          n3 <- $HasNextIter(n2)
          if n3 then jmp b2 else jmp b3

        b2:
          n4 <- $IterData(n2)
          LOCAL[item] <- n4
          n5 <- $Compare.eq(LOCAL[it][LOCAL[n]], LOCAL[item])
          if $Not(n5) then jmp b4(n1) else jmp b5(n1)

        b3:
          return PYCNone

        b4:
          throw GLOBAL[AssertionError]

        b5:
          n8 <- $Inplace.Add(LOCAL[n], PYCInt (1))
          LOCAL[n] <- n8
          jmp b1(n7) |}]


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
          n0 <- $GetIter(LOCAL[foo])
          jmp b1(n0)

        b1:
          n2 <- $NextIter(n1)
          n3 <- $HasNextIter(n2)
          if n3 then jmp b2 else jmp b3

        b2:
          n4 <- $IterData(n2)
          LOCAL[path] <- n4
          if LOCAL[path] then jmp b4(n1) else jmp b1(n1)

        b3:
          return PYCNone

        b4:
          return PYCNone |}]
