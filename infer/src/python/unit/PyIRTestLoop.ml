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

      function toplevel():
        b0:
          n0 <- TOPLEVEL[range]
          n1 <- $Call(n0, 10, None)
          n2 <- $GetIter(n1, None)
          jmp b1

        b1:
          n3 <- $NextIter(n2, None)
          n4 <- $HasNextIter(n2, None)
          if n4 then jmp b2 else jmp b3

        b2:
          TOPLEVEL[x] <- n3
          n5 <- TOPLEVEL[print]
          n6 <- TOPLEVEL[x]
          n7 <- $Call(n5, n6, None)
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

      function toplevel():
        b0:
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          return None


      function dummy.f(x, y, l, bar, toto):
        b0:
          n0 <- LOCAL[l]
          n1 <- $GetIter(n0, None)
          jmp b1

        b1:
          n2 <- $NextIter(n1, None)
          n3 <- $HasNextIter(n1, None)
          if n3 then jmp b2 else jmp b13

        b12:
          jmp b1

        b13:
          return None

        b2:
          LOCAL[x] <- n2
          n4 <- LOCAL[bar]
          n5 <- $Call(n4, None)
          n6 <- $CallMethod[__enter__](n5, None)
          n7 <- LOCAL[toto]
          n8 <- $Call(n7, None)
          n9 <- $CallMethod[__enter__](n8, None)
          LOCAL[obj] <- n9
          n10 <- LOCAL[y]
          if n10 then jmp b3 else jmp b4

        b3:
          n15 <- $CallMethod[__exit__](n8, None)
          n16 <- $CallMethod[__exit__](n5, None)
          jmp b1

        b4:
          n11 <- GLOBAL[print]
          n12 <- $Call(n11, "nop", None)
          n13 <- $CallMethod[__exit__](n8, None)
          jmp b8

        b8:
          n14 <- $CallMethod[__exit__](n5, None)
          jmp b12 |}]


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

      function toplevel():
        b0:
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          return None


      function dummy.f(match, it, n):
        b0:
          n0 <- LOCAL[match]
          n1 <- $GetIter(n0, None)
          jmp b1

        b1:
          n2 <- $NextIter(n1, None)
          n3 <- $HasNextIter(n1, None)
          if n3 then jmp b2 else jmp b5

        b2:
          LOCAL[item] <- n2
          n4 <- LOCAL[it]
          n5 <- LOCAL[n]
          n6 <- n4[n5]
          n7 <- LOCAL[item]
          n8 <- $Compare.eq(n6, n7, None)
          if n8 then jmp b4 else jmp b3

        b3:
          n11 <- GLOBAL[AssertionError]
          throw n11

        b4:
          n9 <- LOCAL[n]
          n10 <- $Inplace.Add(n9, 1, None)
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

      function toplevel():
        b0:
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          return None


      function dummy.f(foo):
        b0:
          n0 <- LOCAL[foo]
          n1 <- $GetIter(n0, None)
          jmp b1

        b1:
          n2 <- $NextIter(n1, None)
          n3 <- $HasNextIter(n1, None)
          if n3 then jmp b2 else jmp b5

        b2:
          LOCAL[path] <- n2
          n4 <- LOCAL[path]
          if n4 then jmp b3 else jmp b4

        b3:
          return None

        b4:
          jmp b1

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

      function toplevel():
        b0:
          n0 <- $MakeFunction["async_loop1", "dummy.async_loop1", None, None, None, None]
          TOPLEVEL[async_loop1] <- n0
          return None


      function dummy.async_loop1(doc):
        b0:
          $GenStartCoroutine()
          n0 <- GLOBAL[get_docs]
          n1 <- $Call(n0, None)
          n2 <- $CallMethod[__aiter__](n1, None)
          jmp b1

        b1:
          n3 <- $CallMethod[__anext__](n2, None)
          n4 <- $GetAwaitable(n3, None)
          n5 <- $YieldFrom(n4, None, None)
          LOCAL[doc] <- n4
          n6 <- GLOBAL[foo]
          n7 <- LOCAL[doc]
          n8 <- $Call(n6, n7, None)
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

      function toplevel():
        b0:
          n0 <- $MakeFunction["async_loop2", "dummy.async_loop2", None, None, None, None]
          TOPLEVEL[async_loop2] <- n0
          return None


      function dummy.async_loop2._$listcomp(.0):
        b0:
          $GenStartCoroutine()
          n0 <- LOCAL[.0]
          jmp b1

        b1:
          n1 <- $CallMethod[__anext__](n0, None)
          n2 <- $GetAwaitable(n1, None)
          n3 <- $YieldFrom(n2, None, None)
          LOCAL[x] <- n2
          n4 <- LOCAL[x]
          n5 <- $ListAppend($BuildList(), n4, None)
          jmp b1


      function dummy.async_loop2():
        b0:
          $GenStartCoroutine()
          n0 <- $MakeFunction["_$listcomp", "dummy.async_loop2._$listcomp", None, None, None, None]
          n1 <- GLOBAL[read]
          n2 <- $Call(n1, None)
          n3 <- $CallMethod[__aiter__](n2, None)
          n4 <- $Call(n0, n3, None)
          n5 <- $GetAwaitable(n4, None)
          n6 <- $YieldFrom(n5, None, None)
          return None |}]


let%expect_test _ =
  let source =
    {|
def main():
    if test():
        for i in r():
            action()
        while test():
            action()
|}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $MakeFunction["main", "dummy.main", None, None, None, None]
          TOPLEVEL[main] <- n0
          return None


      function dummy.main(i):
        b0:
          n0 <- GLOBAL[test]
          n1 <- $Call(n0, None)
          if n1 then jmp b1 else jmp b7

        b1:
          n2 <- GLOBAL[r]
          n3 <- $Call(n2, None)
          n4 <- $GetIter(n3, None)
          jmp b2

        b2:
          n5 <- $NextIter(n4, None)
          n6 <- $HasNextIter(n4, None)
          if n6 then jmp b3 else jmp b4

        b3:
          LOCAL[i] <- n5
          n13 <- GLOBAL[action]
          n14 <- $Call(n13, None)
          jmp b2

        b4:
          n7 <- GLOBAL[test]
          n8 <- $Call(n7, None)
          if n8 then jmp b5 else jmp b8

        b5:
          n9 <- GLOBAL[action]
          n10 <- $Call(n9, None)
          n11 <- GLOBAL[test]
          n12 <- $Call(n11, None)
          if n12 then jmp b5 else jmp b6

        b6:
          return None

        b7:
          return None

        b8:
          return None |}]


let%expect_test _ =
  let source =
    {|
def main():
    for _ in loop():
        if test():
            pass
        action()
|}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $MakeFunction["main", "dummy.main", None, None, None, None]
          TOPLEVEL[main] <- n0
          return None


      function dummy.main(_):
        b0:
          n0 <- GLOBAL[loop]
          n1 <- $Call(n0, None)
          n2 <- $GetIter(n1, None)
          jmp b1

        b1:
          n3 <- $NextIter(n2, None)
          n4 <- $HasNextIter(n2, None)
          if n4 then jmp b2 else jmp b5

        b2:
          LOCAL[_] <- n3
          n5 <- GLOBAL[test]
          n6 <- $Call(n5, None)
          if n6 then jmp b3 else jmp b4

        b3:
          jmp b4

        b4:
          n7 <- GLOBAL[action]
          n8 <- $Call(n7, None)
          jmp b1

        b5:
          return None |}]
