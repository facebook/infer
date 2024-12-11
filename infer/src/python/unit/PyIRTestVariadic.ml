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
          n0 <- None
          n3 <- $MakeFunction["f", "dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n3
          return n0


      function dummy.f(kwargs, k, v):
        b0:
          n0 <- None
          n3 <- LOCAL[kwargs]
          n4 <- $CallMethod[items](n3, n0)
          n5 <- $GetIter(n4, n0)
          jmp b1

        b1:
          n6 <- $NextIter(n5, n0)
          n7 <- $HasNextIter(n5, n0)
          if n7 then jmp b2 else jmp b3

        b2:
          LOCAL[k] <- n6[0]
          LOCAL[v] <- n6[1]
          n8 <- GLOBAL[print]
          n9 <- LOCAL[k]
          n10 <- LOCAL[v]
          n11 <- $Call(n8, n9, n10, n0)
          jmp b1

        b3:
          return n0 |}]


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
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["f", "dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n3
          n4 <- $MakeFunction["g", "dummy.g", n0, n0, n0, n0]
          TOPLEVEL[g] <- n4
          n5 <- $MakeFunction["start", "dummy.start", n0, n0, n0, n0]
          TOPLEVEL[start] <- n5
          n6 <- TOPLEVEL[start]
          n7 <- $Call(n6, n0)
          return n0


      function dummy.f(dummy, dummy2, dummy3, dummy4):
        b0:
          n0 <- None
          n3 <- GLOBAL[print]
          n4 <- LOCAL[dummy]
          n5 <- $Call(n3, "dummy = ", n4, n0)
          n6 <- GLOBAL[print]
          n7 <- LOCAL[dummy2]
          n8 <- $Call(n6, "dummy2= ", n7, n0)
          n9 <- GLOBAL[print]
          n10 <- LOCAL[dummy3]
          n11 <- $Call(n9, "dummy3= ", n10, n0)
          n12 <- GLOBAL[print]
          n13 <- LOCAL[dummy4]
          n14 <- $Call(n12, "dummy4= ", n13, n0)
          n15 <- LOCAL[dummyA]
          n16 <- $CallMethod[items](n15, n0)
          n17 <- $GetIter(n16, n0)
          jmp b1

        b1:
          n18 <- $NextIter(n17, n0)
          n19 <- $HasNextIter(n17, n0)
          if n19 then jmp b2 else jmp b3

        b2:
          LOCAL[k] <- n18[0]
          LOCAL[v] <- n18[1]
          n20 <- GLOBAL[print]
          n21 <- LOCAL[k]
          n22 <- LOCAL[v]
          n23 <- $CallMethod[format]("{} = {}", n21, n22, n0)
          n24 <- $Call(n20, n23, n0)
          jmp b1

        b3:
          return n0


      function dummy.g(dummy, dummy2, dummy3, dummy4):
        b0:
          n0 <- None
          n3 <- GLOBAL[print]
          n4 <- LOCAL[dummy]
          n5 <- $Call(n3, "dummy = ", n4, n0)
          n6 <- GLOBAL[print]
          n7 <- LOCAL[dummy2]
          n8 <- $Call(n6, "dummy2= ", n7, n0)
          n9 <- GLOBAL[print]
          n10 <- LOCAL[dummy3]
          n11 <- $Call(n9, "dummy3= ", n10, n0)
          n12 <- GLOBAL[print]
          n13 <- LOCAL[dummy4]
          n14 <- $Call(n12, "dummy4= ", n13, n0)
          return n0


      function dummy.start(x):
        b0:
          n0 <- None
          LOCAL[x] <- $BuildTuple(3, 4)
          n3 <- GLOBAL[f]
          n4 <- $ListExtend($BuildList(), $BuildTuple(1, 2), n0)
          n5 <- LOCAL[x]
          n6 <- $ListExtend($BuildList(), n5, n0)
          n7 <- $ListToTuple($BuildList(), n0)
          n8 <- $DictMerge($BuildMap(), $BuildMap("test", 42), n0)
          n9 <- $CallFunctionEx(n3, n7, $BuildMap(), n0)
          n10 <- GLOBAL[f]
          n11 <- $ListExtend($BuildList(), $BuildTuple(1, 2), n0)
          n12 <- $ListAppend($BuildList(), "a", n0)
          n13 <- $ListAppend($BuildList(), "b", n0)
          n14 <- $ListToTuple($BuildList(), n0)
          n15 <- $DictMerge($BuildMap(), $BuildMap("test", 42), n0)
          n16 <- $CallFunctionEx(n10, n14, $BuildMap(), n0)
          n17 <- GLOBAL[g]
          n18 <- $ListExtend($BuildList(), $BuildTuple(1, 2), n0)
          n19 <- LOCAL[x]
          n20 <- $ListExtend($BuildList(), n19, n0)
          n21 <- $ListToTuple($BuildList(), n0)
          n22 <- $CallFunctionEx(n17, n21, n0, n0)
          return n0 |}]


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
          n0 <- None
          n3 <- $MakeFunction["f", "dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n3
          return n0


      function dummy.f(foo, a, b, c):
        b0:
          n0 <- None
          n3 <- LOCAL[foo]
          n4 <- LOCAL[a]
          n5 <- $CallMethod[f](n3, n4, n0)
          n6 <- LOCAL[foo]
          n7 <- n6.f
          n8 <- LOCAL[b]
          n9 <- $CallFunctionEx(n7, n8, n0, n0)
          n10 <- LOCAL[foo]
          n11 <- n10.f
          n12 <- LOCAL[a]
          n13 <- LOCAL[b]
          n14 <- $ListExtend($BuildList(n12), n13, n0)
          n15 <- $ListToTuple($BuildList(n12), n0)
          n16 <- $CallFunctionEx(n11, n15, n0, n0)
          n17 <- LOCAL[foo]
          n18 <- n17.f
          n19 <- LOCAL[c]
          n20 <- $DictMerge($BuildMap(), n19, n0)
          n21 <- $CallFunctionEx(n18, $BuildTuple(), $BuildMap(), n0)
          n22 <- LOCAL[foo]
          n23 <- n22.f
          n24 <- LOCAL[b]
          n25 <- LOCAL[c]
          n26 <- $DictMerge($BuildMap(), n25, n0)
          n27 <- $CallFunctionEx(n23, n24, $BuildMap(), n0)
          n28 <- LOCAL[foo]
          n29 <- n28.f
          n30 <- LOCAL[a]
          n31 <- LOCAL[c]
          n32 <- $DictMerge($BuildMap(), n31, n0)
          n33 <- $CallFunctionEx(n29, $BuildTuple(n30), $BuildMap(), n0)
          n34 <- LOCAL[foo]
          n35 <- n34.f
          n36 <- LOCAL[a]
          n37 <- LOCAL[b]
          n38 <- $ListExtend($BuildList(n36), n37, n0)
          n39 <- $ListToTuple($BuildList(n36), n0)
          n40 <- LOCAL[c]
          n41 <- $DictMerge($BuildMap(), n40, n0)
          n42 <- $CallFunctionEx(n35, n39, $BuildMap(), n0)
          return n0 |}]


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
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $BuildConstKeyMap($BuildTuple(0, 1), 0, 1, n0)
          TOPLEVEL[d0] <- n3
          n4 <- $BuildConstKeyMap($BuildTuple("a", "b"), 0, 1, n0)
          TOPLEVEL[d1] <- n4
          n5 <- TOPLEVEL[d0]
          n6 <- $DictUpdate($BuildMap(), n5, n0)
          n7 <- TOPLEVEL[d1]
          n8 <- $DictUpdate($BuildMap(), n7, n0)
          TOPLEVEL[x] <- $BuildMap()
          n9 <- TOPLEVEL[print]
          n10 <- TOPLEVEL[x]
          n11 <- $Call(n9, n10, n0)
          n12 <- $MakeFunction["f", "dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n12
          n13 <- $BuildConstKeyMap($BuildTuple("a", "b"), 0, 1, n0)
          TOPLEVEL[d1] <- n13
          n14 <- TOPLEVEL[f]
          n15 <- TOPLEVEL[d1]
          n16 <- $DictMerge($BuildMap(), n15, n0)
          n17 <- $DictMerge($BuildMap(), $BuildMap("x", 42), n0)
          n18 <- $CallFunctionEx(n14, $BuildTuple(), $BuildMap(), n0)
          return n0


      function dummy.f(x):
        b0:
          n0 <- None
          n3 <- GLOBAL[print]
          n4 <- LOCAL[x]
          n5 <- $Call(n3, n4, n0)
          n6 <- LOCAL[kwargs]
          n7 <- $CallMethod[items](n6, n0)
          n8 <- $GetIter(n7, n0)
          jmp b1

        b1:
          n9 <- $NextIter(n8, n0)
          n10 <- $HasNextIter(n8, n0)
          if n10 then jmp b2 else jmp b3

        b2:
          LOCAL[k] <- n9[0]
          LOCAL[v] <- n9[1]
          n11 <- GLOBAL[print]
          n12 <- LOCAL[k]
          n13 <- LOCAL[v]
          n14 <- $Call(n11, n12, n13, n0)
          jmp b1

        b3:
          return n0 |}]


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
          n0 <- None
          n3 <- $MakeFunction["f", "dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n3
          n4 <- TOPLEVEL[f]
          n5 <- $Call(n4, n0)
          n6 <- $UnpackEx(2, 3, n5, n0)
          TOPLEVEL[a] <- n6[0]
          TOPLEVEL[b] <- n6[1]
          TOPLEVEL[lst] <- n6[2]
          TOPLEVEL[x] <- n6[3]
          TOPLEVEL[y] <- n6[4]
          TOPLEVEL[z] <- n6[5]
          n7 <- TOPLEVEL[print]
          n8 <- TOPLEVEL[lst]
          n9 <- $Call(n7, n8, n0)
          return n0


      function dummy.f():
        b0:
          n0 <- None
          n3 <- GLOBAL[range]
          n4 <- $Call(n3, 10, n0)
          return n4 |}]
