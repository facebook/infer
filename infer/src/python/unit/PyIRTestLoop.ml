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
          n1 <- n0(10)
          n2 <- $GetIter(n1)
          jmp b1(n2)

        b1(n3):
          n4 <- $NextIter(n3)
          n5 <- $HasNextIter(n3)
          if n5 then jmp b2 else jmp b3

        b2:
          TOPLEVEL[x] <- n4
          n6 <- TOPLEVEL[print]
          n7 <- TOPLEVEL[x]
          n8 <- n6(n7)
          jmp b1(n3)

        b3:
          return None |}]


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
          return None


      dummy.f:
        b0:
          n0 <- LOCAL[l]
          n1 <- $GetIter(n0)
          jmp b1(n1)

        b1(n2):
          n3 <- $NextIter(n2)
          n4 <- $HasNextIter(n2)
          if n4 then jmp b2 else jmp b7

        b2:
          LOCAL[x] <- n3
          n5 <- LOCAL[bar]
          n6 <- n5()
          n7 <- n6.__enter__()
          n8 <- LOCAL[toto]
          n9 <- n8()
          n10 <- n9.__enter__()
          LOCAL[obj] <- n10
          n11 <- LOCAL[y]
          if n11 then jmp b3 else jmp b4

        b3:
          n16 <- n9.__enter__(None, None, None)
          n17 <- n6.__enter__(None, None, None)
          jmp b1(n2)

        b4:
          n12 <- GLOBAL[print]
          n13 <- n12("nop")
          jmp b5

        b5:
          n14 <- n9.__enter__(None, None, None)
          jmp b6

        b6:
          n15 <- n6.__enter__(None, None, None)
          jmp b1(n2)

        b7:
          return None |}]


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
          return None


      dummy.f:
        b0:
          n0 <- LOCAL[match]
          n1 <- $GetIter(n0)
          jmp b1(n1)

        b1(n2):
          n3 <- $NextIter(n2)
          n4 <- $HasNextIter(n2)
          if n4 then jmp b2 else jmp b5

        b2:
          LOCAL[item] <- n3
          n5 <- LOCAL[it]
          n6 <- LOCAL[n]
          n7 <- n5[n6]
          n8 <- LOCAL[item]
          n9 <- $Compare.eq(n7, n8)
          if $Not(n9) then jmp b3 else jmp b4

        b3:
          n12 <- GLOBAL[AssertionError]
          throw n12

        b4:
          n10 <- LOCAL[n]
          n11 <- $Inplace.Add(n10, 1)
          LOCAL[n] <- n11
          jmp b1(n2)

        b5:
          return None |}]


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
          return None


      dummy.f:
        b0:
          n0 <- LOCAL[foo]
          n1 <- $GetIter(n0)
          jmp b1(n1)

        b1(n2):
          n3 <- $NextIter(n2)
          n4 <- $HasNextIter(n2)
          if n4 then jmp b2 else jmp b5

        b2:
          LOCAL[path] <- n3
          n5 <- LOCAL[path]
          if n5 then jmp b3 else jmp b1(n2)

        b3:
          return None

        b5:
          return None |}]


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
          return None


      dummy.async_loop1:
        b0:
          n0 <- GLOBAL[get_docs]
          n1 <- n0()
          n2 <- n1.__aiter__()
          jmp b1(n2)

        b1(n3):
          n4 <- n3.__anext__()
          n5 <- $GetAwaitable(n4)
          n6 <- $YieldFrom(n5, None)
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
          return None


      dummy.async_loop2.<listcomp>:
        b0:
          n0 <- LOCAL[.0]
          jmp b1([], n0)

        b1(n1, n2):
          n3 <- n2.__anext__()
          n4 <- $GetAwaitable(n3)
          n5 <- $YieldFrom(n4, None)
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
          n5 <- $YieldFrom(n4, None)
          return None |}]
