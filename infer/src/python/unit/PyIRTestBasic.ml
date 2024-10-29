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
          TOPLEVEL[x] <- 42
          return None |}]


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
          TOPLEVEL[x] <- 42
          n0 <- TOPLEVEL[print]
          n1 <- TOPLEVEL[x]
          n2 <- $Call(n0, n1, None)
          return None |}]


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
          TOPLEVEL[x] <- 42
          TOPLEVEL[y] <- 10
          n0 <- TOPLEVEL[print]
          n1 <- TOPLEVEL[x]
          n2 <- TOPLEVEL[y]
          n3 <- $Binary.Add(n1, n2, None)
          n4 <- $Call(n0, n3, None)
          return None |}]


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
          TOPLEVEL[x] <- 42
          TOPLEVEL[y] <- 10
          n0 <- TOPLEVEL[print]
          n1 <- TOPLEVEL[x]
          n2 <- TOPLEVEL[y]
          n3 <- $Binary.Subtract(n1, n2, None)
          n4 <- $Call(n0, n3, None)
          return None |}]


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
          TOPLEVEL[x] <- 42
          n0 <- TOPLEVEL[x]
          n1 <- $Inplace.Add(n0, 10, None)
          TOPLEVEL[x] <- n1
          n2 <- TOPLEVEL[print]
          n3 <- TOPLEVEL[x]
          n4 <- $Call(n2, n3, None)
          return None |}]


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
          TOPLEVEL[x] <- 42
          n0 <- TOPLEVEL[x]
          n1 <- $Inplace.Subtract(n0, 10, None)
          TOPLEVEL[x] <- n1
          n2 <- TOPLEVEL[print]
          n3 <- TOPLEVEL[x]
          n4 <- $Call(n2, n3, None)
          return None |}]


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
          TOPLEVEL[pi] <- 3.14
          return None |}]


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
          TOPLEVEL[byte_data] <- "Hello"
          return None |}]


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
          n0 <- $ListExtend($BuildList(), $BuildTuple(0, 1, 2, 3, 4, 5), None)
          TOPLEVEL[l] <- $BuildList()
          n1 <- TOPLEVEL[l]
          n2 <- n1[$BuildSlice(0, 2)]
          n3 <- TOPLEVEL[l]
          n4 <- n3[$BuildSlice(0, 2, 1)]
          return None |}]


let%expect_test _ =
  let source = "True != False" in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $Compare.neq(true, false, None)
          return None |}]


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
          n0 <- $ListExtend($BuildList(), $BuildTuple(1, 2, 3), None)
          TOPLEVEL[l] <- $BuildList()
          n1 <- TOPLEVEL[print]
          n2 <- TOPLEVEL[l]
          n3 <- n2[0]
          n4 <- $Call(n1, n3, None)
          return None |}]


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
          n0 <- $ListExtend($BuildList(), $BuildTuple(1, 2, 3), None)
          TOPLEVEL[l] <- $BuildList()
          TOPLEVEL[x] <- 0
          n1 <- TOPLEVEL[l]
          n2 <- TOPLEVEL[x]
          n1[n2] <- 10
          return None |}]


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
          n0 <- $SetUpdate($BuildSet(), $BuildFrozenSet(1, 2, 3), None)
          TOPLEVEL[s] <- $BuildSet()
          return None |}]


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
    {xxx|
    module dummy:

      function toplevel():
        b0:
          TOPLEVEL[x] <- "1"
          n0 <- TOPLEVEL[x]
          TOPLEVEL[s] <- $BuildMap(n0, 1, "2", 2)
          n1 <- TOPLEVEL[print]
          n2 <- TOPLEVEL[s]
          n3 <- $Call(n1, n2, None)
          n4 <- $BuildConstKeyMap($BuildTuple("a", "b"), 42, 1664, None)
          TOPLEVEL[s] <- n4
          n5 <- TOPLEVEL[print]
          n6 <- TOPLEVEL[s]
          n7 <- n6["1"]
          n8 <- $Call(n5, n7, None)
          n9 <- $BuildConstKeyMap($BuildTuple(120, "abc", 1, 121), "abc", 120, None, "", None)
          TOPLEVEL[d] <- n9
          return None |xxx}]


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
          n0 <- TOPLEVEL[open]
          n1 <- $Call(n0, "foo.txt", "wt", None)
          TOPLEVEL[fp] <- n1
          n2 <- TOPLEVEL[fp]
          n3 <- $CallMethod[write](n2, "yolo", None)
          return None |}]


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
          n0 <- TOPLEVEL[open]
          n1 <- $Call(n0, "foo.txt", "wt", None)
          n2 <- $CallMethod[__enter__](n1, None)
          TOPLEVEL[fp] <- n2
          n3 <- TOPLEVEL[fp]
          n4 <- $CallMethod[write](n3, "yolo", None)
          n5 <- $CallMethod[__exit__](n1, None)
          return None |}]


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
          TOPLEVEL[values] <- $BuildList(1, 2, $BuildList(3, 4), 5)
          TOPLEVEL[values2] <- $BuildTuple("a", "b")
          n0 <- $ListExtend($BuildList(), $BuildList(10, 100), None)
          n1 <- TOPLEVEL[values]
          n2 <- $ListExtend($BuildList(), n1, None)
          n3 <- TOPLEVEL[values2]
          n4 <- $ListExtend($BuildList(), n3, None)
          n5 <- $ListToTuple($BuildList(), None)
          TOPLEVEL[result] <- n5
          n6 <- TOPLEVEL[print]
          n7 <- TOPLEVEL[result]
          n8 <- $Call(n6, n7, None)
          n9 <- TOPLEVEL[values]
          n10 <- $ListExtend($BuildList(), n9, None)
          n11 <- TOPLEVEL[values2]
          n12 <- $ListExtend($BuildList(), n11, None)
          TOPLEVEL[result] <- $BuildList()
          n13 <- TOPLEVEL[print]
          n14 <- TOPLEVEL[result]
          n15 <- $Call(n13, n14, None)
          return None |}]


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
          TOPLEVEL[x] <- 1
          n0 <- TOPLEVEL[x]
          TOPLEVEL[x] <- 0
          n1 <- $Binary.Add(n0, 0, None)
          TOPLEVEL[x] <- n1
          n2 <- TOPLEVEL[print]
          n3 <- TOPLEVEL[x]
          n4 <- $Call(n2, n3, None)
          return None |}]


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
    {xxx|
    module dummy:

      function toplevel():
        b0:
          n0 <- TOPLEVEL[f]
          n1 <- $Call(n0, 0, 1, None)
          TOPLEVEL[x] <- n1
          n2 <- TOPLEVEL[f]
          n3 <- $Call(n2, 0, 1, $BuildTuple("b"))
          TOPLEVEL[x] <- n3
          n4 <- TOPLEVEL[f]
          n5 <- $Call(n4, 0, 1, $BuildTuple("a", "b"))
          TOPLEVEL[x] <- n5
          n6 <- TOPLEVEL[f]
          n7 <- TOPLEVEL[args]
          n8 <- $ListExtend($BuildList(0), n7, None)
          n9 <- $ListToTuple($BuildList(0), None)
          n10 <- $CallFunctionEx(n6, n9, None, None)
          TOPLEVEL[x] <- n10
          n11 <- TOPLEVEL[f]
          n12 <- TOPLEVEL[d]
          n13 <- $DictMerge($BuildMap(), n12, None)
          n14 <- $CallFunctionEx(n11, $BuildTuple(0), $BuildMap(), None)
          TOPLEVEL[x] <- n14
          n15 <- TOPLEVEL[f]
          n16 <- TOPLEVEL[args]
          n17 <- $ListExtend($BuildList(0), n16, None)
          n18 <- $ListToTuple($BuildList(0), None)
          n19 <- TOPLEVEL[d]
          n20 <- $DictMerge($BuildMap(), n19, None)
          n21 <- $CallFunctionEx(n15, n18, $BuildMap(), None)
          TOPLEVEL[x] <- n21
          n22 <- TOPLEVEL[f]
          n23 <- TOPLEVEL[args1]
          n24 <- $ListExtend($BuildList(0), n23, None)
          n25 <- TOPLEVEL[args2]
          n26 <- $ListExtend($BuildList(0), n25, None)
          n27 <- $ListToTuple($BuildList(0), None)
          n28 <- TOPLEVEL[d1]
          n29 <- $DictMerge($BuildMap(), n28, None)
          n30 <- TOPLEVEL[d2]
          n31 <- $DictMerge($BuildMap(), n30, None)
          n32 <- $CallFunctionEx(n22, n27, $BuildMap(), None)
          TOPLEVEL[x] <- n32
          n33 <- TOPLEVEL[o]
          n34 <- $CallMethod[f](n33, 0, 1, None)
          TOPLEVEL[x] <- n34
          n35 <- TOPLEVEL[o]
          n36 <- n35.f
          n37 <- $Call(n36, 0, 1, $BuildTuple("b"))
          TOPLEVEL[x] <- n37
          n38 <- TOPLEVEL[o]
          n39 <- n38.f
          n40 <- $Call(n39, 0, 1, $BuildTuple("a", "b"))
          TOPLEVEL[x] <- n40
          n41 <- TOPLEVEL[o]
          n42 <- n41.f
          n43 <- TOPLEVEL[args]
          n44 <- $ListExtend($BuildList(0), n43, None)
          n45 <- $ListToTuple($BuildList(0), None)
          n46 <- $CallFunctionEx(n42, n45, None, None)
          TOPLEVEL[x] <- n46
          n47 <- TOPLEVEL[o]
          n48 <- n47.f
          n49 <- TOPLEVEL[d]
          n50 <- $DictMerge($BuildMap(), n49, None)
          n51 <- $CallFunctionEx(n48, $BuildTuple(0), $BuildMap(), None)
          TOPLEVEL[x] <- n51
          n52 <- TOPLEVEL[o]
          n53 <- n52.f
          n54 <- TOPLEVEL[args]
          n55 <- $ListExtend($BuildList(0), n54, None)
          n56 <- $ListToTuple($BuildList(0), None)
          n57 <- TOPLEVEL[d]
          n58 <- $DictMerge($BuildMap(), n57, None)
          n59 <- $CallFunctionEx(n53, n56, $BuildMap(), None)
          TOPLEVEL[x] <- n59
          n60 <- TOPLEVEL[o]
          n61 <- n60.f
          n62 <- TOPLEVEL[args1]
          n63 <- $ListExtend($BuildList(0), n62, None)
          n64 <- TOPLEVEL[args2]
          n65 <- $ListExtend($BuildList(0), n64, None)
          n66 <- $ListToTuple($BuildList(0), None)
          n67 <- TOPLEVEL[d1]
          n68 <- $DictMerge($BuildMap(), n67, None)
          n69 <- TOPLEVEL[d2]
          n70 <- $DictMerge($BuildMap(), n69, None)
          n71 <- $CallFunctionEx(n61, n66, $BuildMap(), None)
          TOPLEVEL[x] <- n71
          return None
|xxx}]


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
          n0 <- $MakeFunction["main", "dummy.main", None, None, None, None]
          TOPLEVEL[main] <- n0
          return None


      function dummy.main.f(x, y, z):
        b0:
          n0 <- $LoadDeref(0,"arg")
          return n0


      function dummy.main(arg):
        b0:
          n0 <- $BuildConstKeyMap($BuildTuple("key"), None, None)
          n1 <- GLOBAL[int]
          n2 <- GLOBAL[str]
          n3 <- GLOBAL[float]
          n4 <- $LoadClosure(0,"arg")
          n5 <- $MakeFunction["f", "dummy.main.f", $BuildTuple("ok", 0.), n0, $BuildTuple("x", n1, "y", n2, "z", n3), $BuildTuple(n4)]
          LOCAL[f] <- n5
          return None |}]


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
          n0 <- $MakeFunction["foo", "dummy.foo", None, None, None, None]
          TOPLEVEL[foo] <- n0
          return None


      function dummy.foo(n):
        b0:
          n0 <- GLOBAL[o]
          n1 <- LOCAL[n]
          n2 <- $Binary.Multiply("*", n1, None)
          n3 <- $CallMethod[update](n0, $BuildMap("key", n2), None)
          return None |}]


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
          n0 <- $MakeFunction["foo", "dummy.foo", None, None, None, None]
          TOPLEVEL[foo] <- n0
          return None


      function dummy.foo(n):
        b0:
          n0 <- LOCAL[n]
          n1 <- $Compare.gt(n0, 0, None)
          if n1 then jmp b2 else jmp b1

        b1:
          throw $AssertionError

        b2:
          return None |}]
