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
          jmp b1

        b1:
          n3 <- $NextIter(n2)
          n4 <- $HasNextIter(n2)
          if n4 then jmp b2 else jmp b3

        b2:
          TOPLEVEL[x] <- n3
          n5 <- TOPLEVEL[print]
          n6 <- TOPLEVEL[x]
          n7 <- n5(n6)
          jmp b1

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
          jmp b1

        b1:
          n2 <- $NextIter(n1)
          n3 <- $HasNextIter(n1)
          if n3 then jmp b2 else jmp b7

        b2:
          LOCAL[x] <- n2
          n4 <- LOCAL[bar]
          n5 <- n4()
          n6 <- n5.__enter__()
          n7 <- LOCAL[toto]
          n8 <- n7()
          n9 <- n8.__enter__()
          LOCAL[obj] <- n9
          n10 <- LOCAL[y]
          if n10 then jmp b3 else jmp b4

        b3:
          n15 <- n8.__enter__(None, None, None)
          n16 <- n5.__enter__(None, None, None)
          jmp b1

        b4:
          n11 <- GLOBAL[print]
          n12 <- n11("nop")
          jmp b5

        b5:
          n13 <- n8.__enter__(None, None, None)
          jmp b6

        b6:
          n14 <- n5.__enter__(None, None, None)
          jmp b1

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
          jmp b1

        b1:
          n2 <- $NextIter(n1)
          n3 <- $HasNextIter(n1)
          if n3 then jmp b2 else jmp b5

        b2:
          LOCAL[item] <- n2
          n4 <- LOCAL[it]
          n5 <- LOCAL[n]
          n6 <- n4[n5]
          n7 <- LOCAL[item]
          n8 <- $Compare.eq(n6, n7)
          if $Not(n8) then jmp b3 else jmp b4

        b3:
          n11 <- GLOBAL[AssertionError]
          throw n11

        b4:
          n9 <- LOCAL[n]
          n10 <- $Inplace.Add(n9, 1)
          LOCAL[n] <- n10
          jmp b1

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
          jmp b1

        b1:
          n2 <- $NextIter(n1)
          n3 <- $HasNextIter(n1)
          if n3 then jmp b2 else jmp b5

        b2:
          LOCAL[path] <- n2
          n4 <- LOCAL[path]
          if n4 then jmp b3 else jmp b1

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
          jmp b1

        b1:
          n3 <- n2.__anext__()
          n4 <- $GetAwaitable(n3)
          n5 <- $YieldFrom(n4, None)
          LOCAL[doc] <- n4
          n6 <- GLOBAL[foo]
          n7 <- LOCAL[doc]
          n8 <- n6(n7)
          jmp b1 |}]


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
          jmp b1

        b1:
          n1 <- n0.__anext__()
          n2 <- $GetAwaitable(n1)
          n3 <- $YieldFrom(n2, None)
          LOCAL[x] <- n2
          n4 <- LOCAL[x]
          n5 <- $ListAppend([], n4)
          jmp b1


      dummy.async_loop2:
        b0:
          n0 <- GLOBAL[read]
          n1 <- n0()
          n2 <- n1.__aiter__()
          n3 <- $FuncObj(<listcomp>, dummy.async_loop2.<listcomp>, {})(n2)
          n4 <- $GetAwaitable(n3)
          n5 <- $YieldFrom(n4, None)
          return None |}]
