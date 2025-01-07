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
          n3 <- $BuildList()
          n4 <- $ListExtend(n3, $BuildTuple(0, 1, 2, 3, 4, 5), n0)
          TOPLEVEL[l] <- n3
          n5 <- TOPLEVEL[l]
          n6 <- $BuildSlice(0, 2)
          n7 <- n5[n6]
          n8 <- TOPLEVEL[l]
          n9 <- $BuildSlice(0, 2, 1)
          n10 <- n8[n9]
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
          n3 <- $BuildList()
          n4 <- $ListExtend(n3, $BuildTuple(1, 2, 3), n0)
          TOPLEVEL[l] <- n3
          n5 <- TOPLEVEL[print]
          n6 <- TOPLEVEL[l]
          n7 <- n6[0]
          n8 <- $Call(n5, n7, n0)
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
          n3 <- $BuildList()
          n4 <- $ListExtend(n3, $BuildTuple(1, 2, 3), n0)
          TOPLEVEL[l] <- n3
          TOPLEVEL[x] <- 0
          n5 <- TOPLEVEL[l]
          n6 <- TOPLEVEL[x]
          n5[n6] <- 10
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
          n3 <- $BuildSet()
          n4 <- $SetUpdate(n3, $BuildFrozenSet(1, 2, 3), n0)
          TOPLEVEL[s] <- n3
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
          n4 <- $BuildMap(n3, 1, "2", 2)
          TOPLEVEL[s] <- n4
          n5 <- TOPLEVEL[print]
          n6 <- TOPLEVEL[s]
          n7 <- $Call(n5, n6, n0)
          n8 <- $BuildConstKeyMap($BuildTuple("a", "b"), 42, 1664, n0)
          TOPLEVEL[s] <- n8
          n9 <- TOPLEVEL[print]
          n10 <- TOPLEVEL[s]
          n11 <- n10["1"]
          n12 <- $Call(n9, n11, n0)
          n13 <- $BuildConstKeyMap($BuildTuple(120, "abc", 1, 121), "abc", 120, n0, "", n0)
          TOPLEVEL[d] <- n13
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
          n3 <- $BuildList(3, 4)
          n4 <- $BuildList(1, 2, n3, 5)
          TOPLEVEL[values] <- n4
          TOPLEVEL[values2] <- $BuildTuple("a", "b")
          n5 <- $BuildList()
          n6 <- $BuildList(10, 100)
          n7 <- $ListExtend(n5, n6, n0)
          n8 <- TOPLEVEL[values]
          n9 <- $ListExtend(n5, n8, n0)
          n10 <- TOPLEVEL[values2]
          n11 <- $ListExtend(n5, n10, n0)
          n12 <- $ListToTuple(n5, n0)
          TOPLEVEL[result] <- n12
          n13 <- TOPLEVEL[print]
          n14 <- TOPLEVEL[result]
          n15 <- $Call(n13, n14, n0)
          n16 <- $BuildList()
          n17 <- TOPLEVEL[values]
          n18 <- $ListExtend(n16, n17, n0)
          n19 <- TOPLEVEL[values2]
          n20 <- $ListExtend(n16, n19, n0)
          TOPLEVEL[result] <- n16
          n21 <- TOPLEVEL[print]
          n22 <- TOPLEVEL[result]
          n23 <- $Call(n21, n22, n0)
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
          n10 <- $BuildList(0)
          n11 <- TOPLEVEL[args]
          n12 <- $ListExtend(n10, n11, n0)
          n13 <- $ListToTuple(n10, n0)
          n14 <- $CallFunctionEx(n9, n13, n0, n0)
          TOPLEVEL[x] <- n14
          n15 <- TOPLEVEL[f]
          n16 <- $BuildMap()
          n17 <- TOPLEVEL[d]
          n18 <- $DictMerge(n16, n17, n0)
          n19 <- $CallFunctionEx(n15, $BuildTuple(0), n16, n0)
          TOPLEVEL[x] <- n19
          n20 <- TOPLEVEL[f]
          n21 <- $BuildList(0)
          n22 <- TOPLEVEL[args]
          n23 <- $ListExtend(n21, n22, n0)
          n24 <- $ListToTuple(n21, n0)
          n25 <- $BuildMap()
          n26 <- TOPLEVEL[d]
          n27 <- $DictMerge(n25, n26, n0)
          n28 <- $CallFunctionEx(n20, n24, n25, n0)
          TOPLEVEL[x] <- n28
          n29 <- TOPLEVEL[f]
          n30 <- $BuildList(0)
          n31 <- TOPLEVEL[args1]
          n32 <- $ListExtend(n30, n31, n0)
          n33 <- TOPLEVEL[args2]
          n34 <- $ListExtend(n30, n33, n0)
          n35 <- $ListToTuple(n30, n0)
          n36 <- $BuildMap()
          n37 <- TOPLEVEL[d1]
          n38 <- $DictMerge(n36, n37, n0)
          n39 <- TOPLEVEL[d2]
          n40 <- $DictMerge(n36, n39, n0)
          n41 <- $CallFunctionEx(n29, n35, n36, n0)
          TOPLEVEL[x] <- n41
          n42 <- TOPLEVEL[o]
          n43 <- $CallMethod[f](n42, 0, 1, n0)
          TOPLEVEL[x] <- n43
          n44 <- TOPLEVEL[o]
          n45 <- n44.f
          n46 <- $Call(n45, 0, 1, $BuildTuple("b"))
          TOPLEVEL[x] <- n46
          n47 <- TOPLEVEL[o]
          n48 <- n47.f
          n49 <- $Call(n48, 0, 1, $BuildTuple("a", "b"))
          TOPLEVEL[x] <- n49
          n50 <- TOPLEVEL[o]
          n51 <- n50.f
          n52 <- $BuildList(0)
          n53 <- TOPLEVEL[args]
          n54 <- $ListExtend(n52, n53, n0)
          n55 <- $ListToTuple(n52, n0)
          n56 <- $CallFunctionEx(n51, n55, n0, n0)
          TOPLEVEL[x] <- n56
          n57 <- TOPLEVEL[o]
          n58 <- n57.f
          n59 <- $BuildMap()
          n60 <- TOPLEVEL[d]
          n61 <- $DictMerge(n59, n60, n0)
          n62 <- $CallFunctionEx(n58, $BuildTuple(0), n59, n0)
          TOPLEVEL[x] <- n62
          n63 <- TOPLEVEL[o]
          n64 <- n63.f
          n65 <- $BuildList(0)
          n66 <- TOPLEVEL[args]
          n67 <- $ListExtend(n65, n66, n0)
          n68 <- $ListToTuple(n65, n0)
          n69 <- $BuildMap()
          n70 <- TOPLEVEL[d]
          n71 <- $DictMerge(n69, n70, n0)
          n72 <- $CallFunctionEx(n64, n68, n69, n0)
          TOPLEVEL[x] <- n72
          n73 <- TOPLEVEL[o]
          n74 <- n73.f
          n75 <- $BuildList(0)
          n76 <- TOPLEVEL[args1]
          n77 <- $ListExtend(n75, n76, n0)
          n78 <- TOPLEVEL[args2]
          n79 <- $ListExtend(n75, n78, n0)
          n80 <- $ListToTuple(n75, n0)
          n81 <- $BuildMap()
          n82 <- TOPLEVEL[d1]
          n83 <- $DictMerge(n81, n82, n0)
          n84 <- TOPLEVEL[d2]
          n85 <- $DictMerge(n81, n84, n0)
          n86 <- $CallFunctionEx(n74, n80, n81, n0)
          TOPLEVEL[x] <- n86
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
          n3 <- $MakeFunction["dummy.main", n0, n0, n0, n0]
          TOPLEVEL[main] <- n3
          return n0


      function dummy.main(arg):
        b0:
          n0 <- None
          n3 <- $BuildConstKeyMap($BuildTuple("key"), n0, n0)
          n4 <- GLOBAL[int]
          n5 <- GLOBAL[str]
          n6 <- GLOBAL[float]
          n7 <- $BuildTuple("x", n4, "y", n5, "z", n6)
          n8 <- $LoadClosure(0,"arg")
          n9 <- $BuildTuple(n8)
          n10 <- $MakeFunction["dummy.main.f", $BuildTuple("ok", 0.), n3, n7, n9]
          LOCAL[f] <- n10
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
          n3 <- $MakeFunction["dummy.foo", n0, n0, n0, n0]
          TOPLEVEL[foo] <- n3
          return n0


      function dummy.foo(n):
        b0:
          n0 <- None
          n3 <- GLOBAL[o]
          n4 <- LOCAL[n]
          n5 <- $Binary.Multiply("*", n4, n0)
          n6 <- $BuildMap("key", n5)
          n7 <- $CallMethod[update](n3, n6, n0)
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
          n3 <- $MakeFunction["dummy.foo", n0, n0, n0, n0]
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
