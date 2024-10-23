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

      function toplevel():
        b0:
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          return None


      function dummy.f(kwargs, k, v):
        b0:
          n0 <- LOCAL[kwargs]
          n1 <- $CallMethod[items](n0, None)
          n2 <- $GetIter(n1, None)
          jmp b1

        b1:
          n3 <- $NextIter(n2, None)
          n4 <- $HasNextIter(n2, None)
          if n4 then jmp b2 else jmp b3

        b2:
          LOCAL[k] <- n3[0]
          LOCAL[v] <- n3[1]
          n5 <- GLOBAL[print]
          n6 <- LOCAL[k]
          n7 <- LOCAL[v]
          n8 <- $Call(n5, n6, n7, None)
          jmp b1

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

      function toplevel():
        b0:
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          n1 <- $MakeFunction["g", "dummy.g", None, None, None, None]
          TOPLEVEL[g] <- n1
          n2 <- $MakeFunction["start", "dummy.start", None, None, None, None]
          TOPLEVEL[start] <- n2
          n3 <- TOPLEVEL[start]
          n4 <- $Call(n3, None)
          return None


      function dummy.f(dummy, dummy2, dummy3, dummy4):
        b0:
          n0 <- GLOBAL[print]
          n1 <- LOCAL[dummy]
          n2 <- $Call(n0, "dummy = ", n1, None)
          n3 <- GLOBAL[print]
          n4 <- LOCAL[dummy2]
          n5 <- $Call(n3, "dummy2= ", n4, None)
          n6 <- GLOBAL[print]
          n7 <- LOCAL[dummy3]
          n8 <- $Call(n6, "dummy3= ", n7, None)
          n9 <- GLOBAL[print]
          n10 <- LOCAL[dummy4]
          n11 <- $Call(n9, "dummy4= ", n10, None)
          n12 <- LOCAL[dummyA]
          n13 <- $CallMethod[items](n12, None)
          n14 <- $GetIter(n13, None)
          jmp b1

        b1:
          n15 <- $NextIter(n14, None)
          n16 <- $HasNextIter(n14, None)
          if n16 then jmp b2 else jmp b3

        b2:
          LOCAL[k] <- n15[0]
          LOCAL[v] <- n15[1]
          n17 <- GLOBAL[print]
          n18 <- LOCAL[k]
          n19 <- LOCAL[v]
          n20 <- $CallMethod[format]("{} = {}", n18, n19, None)
          n21 <- $Call(n17, n20, None)
          jmp b1

        b3:
          return None


      function dummy.g(dummy, dummy2, dummy3, dummy4):
        b0:
          n0 <- GLOBAL[print]
          n1 <- LOCAL[dummy]
          n2 <- $Call(n0, "dummy = ", n1, None)
          n3 <- GLOBAL[print]
          n4 <- LOCAL[dummy2]
          n5 <- $Call(n3, "dummy2= ", n4, None)
          n6 <- GLOBAL[print]
          n7 <- LOCAL[dummy3]
          n8 <- $Call(n6, "dummy3= ", n7, None)
          n9 <- GLOBAL[print]
          n10 <- LOCAL[dummy4]
          n11 <- $Call(n9, "dummy4= ", n10, None)
          return None


      function dummy.start(x):
        b0:
          LOCAL[x] <- $BuildTuple(3, 4)
          n0 <- GLOBAL[f]
          n1 <- LOCAL[x]
          n2 <- $CallFunctionEx(n0, $BuildTupleUnpack($BuildTuple(1, 2), n1), $BuildMap("test", 42), None)
          n3 <- GLOBAL[f]
          n4 <- $CallFunctionEx(n3, $BuildTupleUnpack($BuildTuple(1, 2), $BuildTuple("a", "b")), $BuildMap("test", 42), None)
          n5 <- GLOBAL[g]
          n6 <- LOCAL[x]
          n7 <- $CallFunctionEx(n5, $BuildTupleUnpack($BuildTuple(1, 2), n6), None, None)
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

      function toplevel():
        b0:
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          return None


      function dummy.f(foo, a, b, c):
        b0:
          n0 <- LOCAL[foo]
          n1 <- LOCAL[a]
          n2 <- $CallMethod[f](n0, n1, None)
          n3 <- LOCAL[foo]
          n4 <- n3.f
          n5 <- LOCAL[b]
          n6 <- $CallFunctionEx(n4, n5, None, None)
          n7 <- LOCAL[foo]
          n8 <- n7.f
          n9 <- LOCAL[a]
          n10 <- LOCAL[b]
          n11 <- $CallFunctionEx(n8, $BuildTupleUnpack($BuildTuple(n9), n10), None, None)
          n12 <- LOCAL[foo]
          n13 <- n12.f
          n14 <- LOCAL[c]
          n15 <- $CallFunctionEx(n13, $BuildTuple(), n14, None)
          n16 <- LOCAL[foo]
          n17 <- n16.f
          n18 <- LOCAL[b]
          n19 <- LOCAL[c]
          n20 <- $CallFunctionEx(n17, n18, n19, None)
          n21 <- LOCAL[foo]
          n22 <- n21.f
          n23 <- LOCAL[a]
          n24 <- LOCAL[c]
          n25 <- $CallFunctionEx(n22, $BuildTuple(n23), n24, None)
          n26 <- LOCAL[foo]
          n27 <- n26.f
          n28 <- LOCAL[a]
          n29 <- LOCAL[b]
          n30 <- LOCAL[c]
          n31 <- $CallFunctionEx(n27, $BuildTupleUnpack($BuildTuple(n28), n29), n30, None)
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

      function toplevel():
        b0:
          n0 <- $BuildConstKeyMap($BuildTuple(0, 1), 0, 1, None)
          TOPLEVEL[d0] <- n0
          n1 <- $BuildConstKeyMap($BuildTuple("a", "b"), 0, 1, None)
          TOPLEVEL[d1] <- n1
          n2 <- TOPLEVEL[d0]
          n3 <- TOPLEVEL[d1]
          TOPLEVEL[x] <- $BuildMapUnpack(n2, n3)
          n4 <- TOPLEVEL[print]
          n5 <- TOPLEVEL[x]
          n6 <- $Call(n4, n5, None)
          n7 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n7
          n8 <- $BuildConstKeyMap($BuildTuple("a", "b"), 0, 1, None)
          TOPLEVEL[d1] <- n8
          n9 <- TOPLEVEL[f]
          n10 <- TOPLEVEL[d1]
          n11 <- $CallFunctionEx(n9, $BuildTuple(), $BuildMapUnpack(n10, $BuildMap("x", 42)), None)
          return None


      function dummy.f(x):
        b0:
          n0 <- GLOBAL[print]
          n1 <- LOCAL[x]
          n2 <- $Call(n0, n1, None)
          n3 <- LOCAL[kwargs]
          n4 <- $CallMethod[items](n3, None)
          n5 <- $GetIter(n4, None)
          jmp b1

        b1:
          n6 <- $NextIter(n5, None)
          n7 <- $HasNextIter(n5, None)
          if n7 then jmp b2 else jmp b3

        b2:
          LOCAL[k] <- n6[0]
          LOCAL[v] <- n6[1]
          n8 <- GLOBAL[print]
          n9 <- LOCAL[k]
          n10 <- LOCAL[v]
          n11 <- $Call(n8, n9, n10, None)
          jmp b1

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

      function toplevel():
        b0:
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          n1 <- TOPLEVEL[f]
          n2 <- $Call(n1, None)
          n3 <- $UnpackEx(2, 3, n2, None)
          TOPLEVEL[a] <- n3[0]
          TOPLEVEL[b] <- n3[1]
          TOPLEVEL[lst] <- n3[2]
          TOPLEVEL[x] <- n3[3]
          TOPLEVEL[y] <- n3[4]
          TOPLEVEL[z] <- n3[5]
          n4 <- TOPLEVEL[print]
          n5 <- TOPLEVEL[lst]
          n6 <- $Call(n4, n5, None)
          return None


      function dummy.f():
        b0:
          n0 <- GLOBAL[range]
          n1 <- $Call(n0, 10, None)
          return n1 |}]
