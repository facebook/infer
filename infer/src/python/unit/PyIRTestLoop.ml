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
          n0 <- None
          n3 <- TOPLEVEL[range]
          n4 <- $Call(n3, 10, n0)
          n5 <- $GetIter(n4, n0)
          jmp b1

        b1:
          n6 <- $NextIter(n5, n0)
          n7 <- $HasNextIter(n5, n0)
          if n7 then jmp b2 else jmp b3

        b2:
          TOPLEVEL[x] <- n6
          n8 <- TOPLEVEL[print]
          n9 <- TOPLEVEL[x]
          n10 <- $Call(n8, n9, n0)
          jmp b1

        b3:
          return n0 |}]


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
          n0 <- None
          n3 <- $MakeFunction["f", "dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n3
          return n0


      function dummy.f(x, y, l, bar, toto):
        b0:
          n0 <- None
          n3 <- LOCAL[l]
          n4 <- $GetIter(n3, n0)
          jmp b1

        b1:
          n5 <- $NextIter(n4, n0)
          n6 <- $HasNextIter(n4, n0)
          if n6 then jmp b2 else jmp b13

        b12:
          jmp b1

        b13:
          return n0

        b2:
          LOCAL[x] <- n5
          n7 <- LOCAL[bar]
          n8 <- $Call(n7, n0)
          n9 <- $CallMethod[__enter__](n8, n0)
          n10 <- LOCAL[toto]
          n11 <- $Call(n10, n0)
          n12 <- $CallMethod[__enter__](n11, n0)
          LOCAL[obj] <- n12
          n13 <- LOCAL[y]
          if n13 then jmp b3 else jmp b4

        b3:
          n18 <- $CallMethod[__exit__](n11, n0)
          n19 <- $CallMethod[__exit__](n8, n0)
          jmp b1

        b4:
          n14 <- GLOBAL[print]
          n15 <- $Call(n14, "nop", n0)
          n16 <- $CallMethod[__exit__](n11, n0)
          jmp b8

        b8:
          n17 <- $CallMethod[__exit__](n8, n0)
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
          n0 <- None
          n3 <- $MakeFunction["f", "dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n3
          return n0


      function dummy.f(match, it, n):
        b0:
          n0 <- None
          n3 <- LOCAL[match]
          n4 <- $GetIter(n3, n0)
          jmp b1

        b1:
          n5 <- $NextIter(n4, n0)
          n6 <- $HasNextIter(n4, n0)
          if n6 then jmp b2 else jmp b5

        b2:
          LOCAL[item] <- n5
          n7 <- LOCAL[it]
          n8 <- LOCAL[n]
          n9 <- n7[n8]
          n10 <- LOCAL[item]
          n11 <- $Compare.eq(n9, n10, n0)
          if n11 then jmp b4 else jmp b3

        b3:
          n14 <- GLOBAL[AssertionError]
          throw n14

        b4:
          n12 <- LOCAL[n]
          n13 <- $Inplace.Add(n12, 1, n0)
          LOCAL[n] <- n13
          jmp b1

        b5:
          return n0 |}]


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
          n0 <- None
          n3 <- $MakeFunction["f", "dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n3
          return n0


      function dummy.f(foo):
        b0:
          n0 <- None
          n3 <- LOCAL[foo]
          n4 <- $GetIter(n3, n0)
          jmp b1

        b1:
          n5 <- $NextIter(n4, n0)
          n6 <- $HasNextIter(n4, n0)
          if n6 then jmp b2 else jmp b5

        b2:
          LOCAL[path] <- n5
          n7 <- LOCAL[path]
          if n7 then jmp b3 else jmp b4

        b3:
          return n0

        b4:
          jmp b1

        b5:
          return n0 |}]


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
          n0 <- None
          n3 <- $MakeFunction["async_loop1", "dummy.async_loop1", n0, n0, n0, n0]
          TOPLEVEL[async_loop1] <- n3
          return n0


      async function dummy.async_loop1(doc):
        b0:
          n0 <- None
          $GenStartCoroutine()
          n3 <- GLOBAL[get_docs]
          n4 <- $Call(n3, n0)
          n5 <- $CallMethod[__aiter__](n4, n0)
          jmp b1

        b1:
          n6 <- $CallMethod[__anext__](n5, n0)
          n7 <- $GetAwaitable(n6, n0)
          n8 <- $YieldFrom(n7, n0, n0)
          LOCAL[doc] <- n7
          n9 <- GLOBAL[foo]
          n10 <- LOCAL[doc]
          n11 <- $Call(n9, n10, n0)
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
          n0 <- None
          n3 <- $MakeFunction["async_loop2", "dummy.async_loop2", n0, n0, n0, n0]
          TOPLEVEL[async_loop2] <- n3
          return n0


      async function dummy.async_loop2():
        b0:
          n0 <- None
          $GenStartCoroutine()
          n3 <- $MakeFunction["_$listcomp", "dummy.async_loop2._$listcomp", n0, n0, n0, n0]
          n4 <- GLOBAL[read]
          n5 <- $Call(n4, n0)
          n6 <- $CallMethod[__aiter__](n5, n0)
          n7 <- $Call(n3, n6, n0)
          n8 <- $GetAwaitable(n7, n0)
          n9 <- $YieldFrom(n8, n0, n0)
          return n0


      async function dummy.async_loop2._$listcomp(.0):
        b0:
          n0 <- None
          $GenStartCoroutine()
          n3 <- LOCAL[.0]
          jmp b1

        b1:
          n4 <- $CallMethod[__anext__](n3, n0)
          n5 <- $GetAwaitable(n4, n0)
          n6 <- $YieldFrom(n5, n0, n0)
          LOCAL[x] <- n5
          n7 <- LOCAL[x]
          n8 <- $ListAppend($BuildList(), n7, n0)
          jmp b1 |}]


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
          n0 <- None
          n3 <- $MakeFunction["main", "dummy.main", n0, n0, n0, n0]
          TOPLEVEL[main] <- n3
          return n0


      function dummy.main(i):
        b0:
          n0 <- None
          n3 <- GLOBAL[test]
          n4 <- $Call(n3, n0)
          if n4 then jmp b1 else jmp b7

        b1:
          n5 <- GLOBAL[r]
          n6 <- $Call(n5, n0)
          n7 <- $GetIter(n6, n0)
          jmp b2

        b2:
          n8 <- $NextIter(n7, n0)
          n9 <- $HasNextIter(n7, n0)
          if n9 then jmp b3 else jmp b4

        b3:
          LOCAL[i] <- n8
          n16 <- GLOBAL[action]
          n17 <- $Call(n16, n0)
          jmp b2

        b4:
          n10 <- GLOBAL[test]
          n11 <- $Call(n10, n0)
          if n11 then jmp b5 else jmp b8

        b5:
          n12 <- GLOBAL[action]
          n13 <- $Call(n12, n0)
          n14 <- GLOBAL[test]
          n15 <- $Call(n14, n0)
          if n15 then jmp b5 else jmp b6

        b6:
          return n0

        b7:
          return n0

        b8:
          return n0 |}]


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
          n0 <- None
          n3 <- $MakeFunction["main", "dummy.main", n0, n0, n0, n0]
          TOPLEVEL[main] <- n3
          return n0


      function dummy.main(_):
        b0:
          n0 <- None
          n3 <- GLOBAL[loop]
          n4 <- $Call(n3, n0)
          n5 <- $GetIter(n4, n0)
          jmp b1

        b1:
          n6 <- $NextIter(n5, n0)
          n7 <- $HasNextIter(n5, n0)
          if n7 then jmp b2 else jmp b5

        b2:
          LOCAL[_] <- n6
          n8 <- GLOBAL[test]
          n9 <- $Call(n8, n0)
          if n9 then jmp b3 else jmp b4

        b3:
          jmp b4

        b4:
          n10 <- GLOBAL[action]
          n11 <- $Call(n10, n0)
          jmp b1

        b5:
          return n0 |}]
