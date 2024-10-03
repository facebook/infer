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

        b3(n28, n29, n30):
          n31 <- PYCNone(PYCNone, PYCNone, PYCNone)
          n32 <- PYCNone(PYCNone, PYCNone, PYCNone)
          jmp b1(n28, n29, n30)

        b4(n14, n15, n16):
          n17 <- GLOBAL[print]
          n18 <- n17(PYCString ("nop"))
          jmp b5(n14, n15, n16, PYCNone)

        b5(n19, n20, n21, n22):
          n23 <- n22(PYCNone, PYCNone, PYCNone)
          jmp b6(n19, n20, PYCNone)

        b6(n24, n25, n26):
          n27 <- n26(PYCNone, PYCNone, PYCNone)
          jmp b1(n24)

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
          n4 <- $HasNextIter(n2)
          if n4 then jmp b2(n2, n3) else jmp b5

        b2(n5, n6):
          LOCAL[item] <- n6
          n7 <- LOCAL[it]
          n8 <- LOCAL[n]
          n9 <- n7[n8]
          n10 <- LOCAL[item]
          n11 <- $Compare.eq(n9, n10)
          if $Not(n11) then jmp b3(n5) else jmp b4(n5)

        b3(n15):
          n16 <- GLOBAL[AssertionError]
          throw n16

        b4(n12):
          n13 <- LOCAL[n]
          n14 <- $Inplace.Add(n13, PYCInt (1))
          LOCAL[n] <- n14
          jmp b1(n12)

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
          n4 <- $HasNextIter(n2)
          if n4 then jmp b2(n2, n3) else jmp b5

        b2(n5, n6):
          LOCAL[path] <- n6
          n7 <- LOCAL[path]
          if n7 then jmp b3(n5) else jmp b1(n5)

        b3(n8):
          return PYCNone

        b5:
          return PYCNone |}]


let%expect_test _ =
  let source = {|
async def async_loop1():
    async for doc in get_docs():
        foo(doc)
|} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          TOPLEVEL[async_loop1] <- $FuncObj(async_loop1, dummy.async_loop1, {})
          return PYCNone


      dummy.async_loop1:
        b0:
          n0 <- GLOBAL[get_docs]
          n1 <- n0()
          n2 <- n1.__aiter__()
          jmp b1(n2)

        b1(n3):
          n4 <- n3.__anext__()
          n5 <- $GetAwaitable(n4)
          n6 <- $YieldFrom(n5, PYCNone)
          LOCAL[doc] <- n5
          n7 <- GLOBAL[foo]
          n8 <- LOCAL[doc]
          n9 <- n7(n8)
          jmp b1(n3) |}]


let%expect_test _ =
  let source = {|
async def async_loop2():
    [ x async for x in read() ]
|} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          TOPLEVEL[async_loop2] <- $FuncObj(async_loop2, dummy.async_loop2, {})
          return PYCNone


      dummy.async_loop2.<listcomp>:
        b0:
          n0 <- LOCAL[.0]
          jmp b1([], n0)

        b1(n1, n2):
          n3 <- n2.__anext__()
          n4 <- $GetAwaitable(n3)
          n5 <- $YieldFrom(n4, PYCNone)
          LOCAL[x] <- n4
          n6 <- LOCAL[x]
          n7 <- $ListAppend(n1, n6)
          jmp b1(n1, n2)


      dummy.async_loop2:
        b0:
          n0 <- GLOBAL[read]
          n1 <- n0()
          n2 <- n1.__aiter__()
          n3 <- $FuncObj(<listcomp>, dummy.async_loop2.<listcomp>, {})(n2)
          n4 <- $GetAwaitable(n3)
          n5 <- $YieldFrom(n4, PYCNone)
          return PYCNone |}]
