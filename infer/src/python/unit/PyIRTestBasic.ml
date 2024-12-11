(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

(* basic tests without functions, classes or import *)

let%expect_test _ =
  let source = "x = 42" in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          TOPLEVEL[x] <- 42
          return n0 |}]


let%expect_test _ =
  let source = {|
x = 42
print(x)
      |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          TOPLEVEL[x] <- 42
          n3 <- TOPLEVEL[print]
          n4 <- TOPLEVEL[x]
          n5 <- $Call(n3, n4, n0)
          return n0 |}]


let%expect_test _ =
  let source = {|
x = 42
y = 10
print(x + y)
      |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          TOPLEVEL[x] <- 42
          TOPLEVEL[y] <- 10
          n3 <- TOPLEVEL[print]
          n4 <- TOPLEVEL[x]
          n5 <- TOPLEVEL[y]
          n6 <- $Binary.Add(n4, n5, n0)
          n7 <- $Call(n3, n6, n0)
          return n0 |}]


let%expect_test _ =
  let source = {|
x = 42
y = 10
print(x - y)
      |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          TOPLEVEL[x] <- 42
          TOPLEVEL[y] <- 10
          n3 <- TOPLEVEL[print]
          n4 <- TOPLEVEL[x]
          n5 <- TOPLEVEL[y]
          n6 <- $Binary.Subtract(n4, n5, n0)
          n7 <- $Call(n3, n6, n0)
          return n0 |}]


let%expect_test _ =
  let source = {|
x = 42
x += 10
print(x)
      |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          TOPLEVEL[x] <- 42
          n3 <- TOPLEVEL[x]
          n4 <- $Inplace.Add(n3, 10, n0)
          TOPLEVEL[x] <- n4
          n5 <- TOPLEVEL[print]
          n6 <- TOPLEVEL[x]
          n7 <- $Call(n5, n6, n0)
          return n0 |}]


let%expect_test _ =
  let source = {|
x = 42
x -= 10
print(x)
      |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          TOPLEVEL[x] <- 42
          n3 <- TOPLEVEL[x]
          n4 <- $Inplace.Subtract(n3, 10, n0)
          TOPLEVEL[x] <- n4
          n5 <- TOPLEVEL[print]
          n6 <- TOPLEVEL[x]
          n7 <- $Call(n5, n6, n0)
          return n0 |}]


let%expect_test _ =
  let source = {|
pi = 3.14
      |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          TOPLEVEL[pi] <- 3.14
          return n0 |}]


let%expect_test _ =
  let source = {|
byte_data = b'\x48\x65\x6C\x6C\x6F'  # Equivalent to b'Hello'
      |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          TOPLEVEL[byte_data] <- "Hello"
          return n0 |}]


let%expect_test _ =
  let source = {|
l = [0, 1, 2, 3, 4, 5]
l[0:2]
l[0:2:1]
          |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $ListExtend($BuildList(), $BuildTuple(0, 1, 2, 3, 4, 5), n0)
          TOPLEVEL[l] <- $BuildList()
          n4 <- TOPLEVEL[l]
          n5 <- n4[$BuildSlice(0, 2)]
          n6 <- TOPLEVEL[l]
          n7 <- n6[$BuildSlice(0, 2, 1)]
          return n0 |}]


let%expect_test _ =
  let source = "True != False" in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $Compare.neq(true, false, n0)
          return n0 |}]


let%expect_test _ =
  let source = {|
l = [1, 2, 3]
print(l[0])
|} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $ListExtend($BuildList(), $BuildTuple(1, 2, 3), n0)
          TOPLEVEL[l] <- $BuildList()
          n4 <- TOPLEVEL[print]
          n5 <- TOPLEVEL[l]
          n6 <- n5[0]
          n7 <- $Call(n4, n6, n0)
          return n0 |}]


let%expect_test _ =
  let source = {|
l = [1, 2, 3]
x = 0
l[x] = 10
|} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $ListExtend($BuildList(), $BuildTuple(1, 2, 3), n0)
          TOPLEVEL[l] <- $BuildList()
          TOPLEVEL[x] <- 0
          n4 <- TOPLEVEL[l]
          n5 <- TOPLEVEL[x]
          n4[n5] <- 10
          return n0 |}]


let%expect_test _ =
  let source = {|
s = {1, 2, 3}
|} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $SetUpdate($BuildSet(), $BuildFrozenSet(1, 2, 3), n0)
          TOPLEVEL[s] <- $BuildSet()
          return n0 |}]


let%expect_test _ =
  let source =
    {|
x = "1"
s = {x : 1, "2": 2}
print(s)

s = {"a": 42, "b": 1664}
print(s["1"])

# from cinder
d = { 0x78: "abc", # 1-n decoding mapping
      b"abc": 0x0078,# 1-n encoding mapping
      0x01: None,   # decoding mapping to <undefined>
      0x79: "",    # decoding mapping to <remove character>
      }
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          TOPLEVEL[x] <- "1"
          n3 <- TOPLEVEL[x]
          TOPLEVEL[s] <- $BuildMap(n3, 1, "2", 2)
          n4 <- TOPLEVEL[print]
          n5 <- TOPLEVEL[s]
          n6 <- $Call(n4, n5, n0)
          n7 <- $BuildConstKeyMap($BuildTuple("a", "b"), 42, 1664, n0)
          TOPLEVEL[s] <- n7
          n8 <- TOPLEVEL[print]
          n9 <- TOPLEVEL[s]
          n10 <- n9["1"]
          n11 <- $Call(n8, n10, n0)
          n12 <- $BuildConstKeyMap($BuildTuple(120, "abc", 1, 121), "abc", 120, n0, "", n0)
          TOPLEVEL[d] <- n12
          return n0 |}]


let%expect_test _ =
  let source = {|
fp = open("foo.txt", "wt")
fp.write("yolo")
          |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[open]
          n4 <- $Call(n3, "foo.txt", "wt", n0)
          TOPLEVEL[fp] <- n4
          n5 <- TOPLEVEL[fp]
          n6 <- $CallMethod[write](n5, "yolo", n0)
          return n0 |}]


let%expect_test _ =
  let source = {|
with open("foo.txt", "wt") as fp:
    fp.write("yolo")
          |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[open]
          n4 <- $Call(n3, "foo.txt", "wt", n0)
          n5 <- $CallMethod[__enter__](n4, n0)
          TOPLEVEL[fp] <- n5
          n6 <- TOPLEVEL[fp]
          n7 <- $CallMethod[write](n6, "yolo", n0)
          n8 <- $CallMethod[__exit__](n4, n0)
          return n0 |}]


let%expect_test _ =
  let source =
    {|
values = [1, 2, [3, 4] , 5]
values2 = ('a', 'b')

result = (*[10, 100], *values, *values2)

print(result) # (10, 100, 1, 2, [3, 4], 5, 'a', 'b')

result = [*values, *values2] # [10, 100, 1, 2, [3, 4], 5, 'a', 'b']
print(result)
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          TOPLEVEL[values] <- $BuildList(1, 2, $BuildList(3, 4), 5)
          TOPLEVEL[values2] <- $BuildTuple("a", "b")
          n3 <- $ListExtend($BuildList(), $BuildList(10, 100), n0)
          n4 <- TOPLEVEL[values]
          n5 <- $ListExtend($BuildList(), n4, n0)
          n6 <- TOPLEVEL[values2]
          n7 <- $ListExtend($BuildList(), n6, n0)
          n8 <- $ListToTuple($BuildList(), n0)
          TOPLEVEL[result] <- n8
          n9 <- TOPLEVEL[print]
          n10 <- TOPLEVEL[result]
          n11 <- $Call(n9, n10, n0)
          n12 <- TOPLEVEL[values]
          n13 <- $ListExtend($BuildList(), n12, n0)
          n14 <- TOPLEVEL[values2]
          n15 <- $ListExtend($BuildList(), n14, n0)
          TOPLEVEL[result] <- $BuildList()
          n16 <- TOPLEVEL[print]
          n17 <- TOPLEVEL[result]
          n18 <- $Call(n16, n17, n0)
          return n0 |}]


let%expect_test _ =
  let source = {|
x = 1
x = x + (x := 0)
print(x) # will print 1
          |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          TOPLEVEL[x] <- 1
          n3 <- TOPLEVEL[x]
          TOPLEVEL[x] <- 0
          n4 <- $Binary.Add(n3, 0, n0)
          TOPLEVEL[x] <- n4
          n5 <- TOPLEVEL[print]
          n6 <- TOPLEVEL[x]
          n7 <- $Call(n5, n6, n0)
          return n0 |}]


let%expect_test _ =
  let source =
    {|
x = f(0, 1)
x = f(0, b=1)
x = f(a=0, b=1)
x = f(0, *args)
x = f(0, **d)
x = f(0, *args, **d)
x = f(0, *args1, *args2, **d1, **d2)
x = o.f(0, 1)
# Python3.8 compile all other method calls without CALL_METHOD!
x = o.f(0, b=1)
x = o.f(a=0, b=1)
x = o.f(0, *args)
x = o.f(0, **d)
x = o.f(0, *args, **d)
x = o.f(0, *args1, *args2, **d1, **d2)
|}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[f]
          n4 <- $Call(n3, 0, 1, n0)
          TOPLEVEL[x] <- n4
          n5 <- TOPLEVEL[f]
          n6 <- $Call(n5, 0, 1, $BuildTuple("b"))
          TOPLEVEL[x] <- n6
          n7 <- TOPLEVEL[f]
          n8 <- $Call(n7, 0, 1, $BuildTuple("a", "b"))
          TOPLEVEL[x] <- n8
          n9 <- TOPLEVEL[f]
          n10 <- TOPLEVEL[args]
          n11 <- $ListExtend($BuildList(0), n10, n0)
          n12 <- $ListToTuple($BuildList(0), n0)
          n13 <- $CallFunctionEx(n9, n12, n0, n0)
          TOPLEVEL[x] <- n13
          n14 <- TOPLEVEL[f]
          n15 <- TOPLEVEL[d]
          n16 <- $DictMerge($BuildMap(), n15, n0)
          n17 <- $CallFunctionEx(n14, $BuildTuple(0), $BuildMap(), n0)
          TOPLEVEL[x] <- n17
          n18 <- TOPLEVEL[f]
          n19 <- TOPLEVEL[args]
          n20 <- $ListExtend($BuildList(0), n19, n0)
          n21 <- $ListToTuple($BuildList(0), n0)
          n22 <- TOPLEVEL[d]
          n23 <- $DictMerge($BuildMap(), n22, n0)
          n24 <- $CallFunctionEx(n18, n21, $BuildMap(), n0)
          TOPLEVEL[x] <- n24
          n25 <- TOPLEVEL[f]
          n26 <- TOPLEVEL[args1]
          n27 <- $ListExtend($BuildList(0), n26, n0)
          n28 <- TOPLEVEL[args2]
          n29 <- $ListExtend($BuildList(0), n28, n0)
          n30 <- $ListToTuple($BuildList(0), n0)
          n31 <- TOPLEVEL[d1]
          n32 <- $DictMerge($BuildMap(), n31, n0)
          n33 <- TOPLEVEL[d2]
          n34 <- $DictMerge($BuildMap(), n33, n0)
          n35 <- $CallFunctionEx(n25, n30, $BuildMap(), n0)
          TOPLEVEL[x] <- n35
          n36 <- TOPLEVEL[o]
          n37 <- $CallMethod[f](n36, 0, 1, n0)
          TOPLEVEL[x] <- n37
          n38 <- TOPLEVEL[o]
          n39 <- n38.f
          n40 <- $Call(n39, 0, 1, $BuildTuple("b"))
          TOPLEVEL[x] <- n40
          n41 <- TOPLEVEL[o]
          n42 <- n41.f
          n43 <- $Call(n42, 0, 1, $BuildTuple("a", "b"))
          TOPLEVEL[x] <- n43
          n44 <- TOPLEVEL[o]
          n45 <- n44.f
          n46 <- TOPLEVEL[args]
          n47 <- $ListExtend($BuildList(0), n46, n0)
          n48 <- $ListToTuple($BuildList(0), n0)
          n49 <- $CallFunctionEx(n45, n48, n0, n0)
          TOPLEVEL[x] <- n49
          n50 <- TOPLEVEL[o]
          n51 <- n50.f
          n52 <- TOPLEVEL[d]
          n53 <- $DictMerge($BuildMap(), n52, n0)
          n54 <- $CallFunctionEx(n51, $BuildTuple(0), $BuildMap(), n0)
          TOPLEVEL[x] <- n54
          n55 <- TOPLEVEL[o]
          n56 <- n55.f
          n57 <- TOPLEVEL[args]
          n58 <- $ListExtend($BuildList(0), n57, n0)
          n59 <- $ListToTuple($BuildList(0), n0)
          n60 <- TOPLEVEL[d]
          n61 <- $DictMerge($BuildMap(), n60, n0)
          n62 <- $CallFunctionEx(n56, n59, $BuildMap(), n0)
          TOPLEVEL[x] <- n62
          n63 <- TOPLEVEL[o]
          n64 <- n63.f
          n65 <- TOPLEVEL[args1]
          n66 <- $ListExtend($BuildList(0), n65, n0)
          n67 <- TOPLEVEL[args2]
          n68 <- $ListExtend($BuildList(0), n67, n0)
          n69 <- $ListToTuple($BuildList(0), n0)
          n70 <- TOPLEVEL[d1]
          n71 <- $DictMerge($BuildMap(), n70, n0)
          n72 <- TOPLEVEL[d2]
          n73 <- $DictMerge($BuildMap(), n72, n0)
          n74 <- $CallFunctionEx(n64, n69, $BuildMap(), n0)
          TOPLEVEL[x] <- n74
          return n0 |}]


let%expect_test _ =
  let source =
    {|
def main(arg):
    def f(x: int, y: str = "ok", z: float = 0.0, *l, key=None):
        return arg

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


      function dummy.main(arg):
        b0:
          n0 <- None
          n3 <- $BuildConstKeyMap($BuildTuple("key"), n0, n0)
          n4 <- GLOBAL[int]
          n5 <- GLOBAL[str]
          n6 <- GLOBAL[float]
          n7 <- $LoadClosure(0,"arg")
          n8 <- $MakeFunction["f", "dummy.main.f", $BuildTuple("ok", 0.), n3, $BuildTuple("x", n4, "y", n5, "z", n6), $BuildTuple(n7)]
          LOCAL[f] <- n8
          return n0


      function dummy.main.f(x, y, z):
        b0:
          n0 <- None
          n3 <- $LoadDeref(0,"arg")
          return n3 |}]


let%expect_test _ =
  let source = {|
def foo(n):
    o.update({ "key": "*" * n})
|} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["foo", "dummy.foo", n0, n0, n0, n0]
          TOPLEVEL[foo] <- n3
          return n0


      function dummy.foo(n):
        b0:
          n0 <- None
          n3 <- GLOBAL[o]
          n4 <- LOCAL[n]
          n5 <- $Binary.Multiply("*", n4, n0)
          n6 <- $CallMethod[update](n3, $BuildMap("key", n5), n0)
          return n0 |}]


let%expect_test _ =
  let source = {|
def foo(n):
    assert(n>0)
|} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["foo", "dummy.foo", n0, n0, n0, n0]
          TOPLEVEL[foo] <- n3
          return n0


      function dummy.foo(n):
        b0:
          n0 <- None
          n3 <- LOCAL[n]
          n4 <- $Compare.gt(n3, 0, n0)
          if n4 then jmp b2 else jmp b1

        b1:
          throw $AssertionError

        b2:
          return n0 |}]
