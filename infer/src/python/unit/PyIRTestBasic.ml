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
  [%expect {| |}]


let%expect_test _ =
  let source =
    {|
async def str_key_access_bad():
    d = {"ABC": asyncio.sleep(), 123: 456, "DEF": await asyncio.sleep(1)}
    return d["DEF"]
      |}
  in
  PyIR.test ~show:true source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0: @2
          n0 <- None @2
          n3 <- $MakeFunction["dummy.str_key_access_bad", n0, n0, n0, n0] @2
          TOPLEVEL[str_key_access_bad] <- n3 @2
          return n0
           @2

      async function dummy.str_key_access_bad(d):
        b0: @?
          n0 <- None @?
          $GenStartCoroutine() @?
          jmp b1
           @?
        b1: @3
          n3 <- GLOBAL[asyncio] @3
          n4 <- $CallMethod[sleep](n3, n0) @3
          n5 <- GLOBAL[asyncio] @3
          n6 <- $CallMethod[sleep](n5, 1, n0) @3
          n7 <- $GetAwaitable(n6, n0) @3
          n8 <- $YieldFrom(n7, n0, n0) @3
          n9 <- $BuildMap("ABC", n4, 123, 456, "DEF", n7) @3
          LOCAL[d] <- n9 @3
          jmp b2
           @3
        b2: @4
          n10 <- LOCAL[d] @4
          n11 <- n10["DEF"] @4
          return n11
           @4 |}]


let%expect_test _ =
  let source = {|
x = 42
y = 10
print(x + y)
      |} in
  PyIR.test source ;
  [%expect {| |}]


let%expect_test _ =
  let source = {|
x = 42
y = 10
print(x - y)
      |} in
  PyIR.test source ;
  [%expect {| |}]


let%expect_test _ =
  let source = {|
x = 42
x += 10
print(x)
      |} in
  PyIR.test source ;
  [%expect {| |}]


let%expect_test _ =
  let source = {|
x = 42
x -= 10
print(x)
      |} in
  PyIR.test source ;
  [%expect {| |}]


let%expect_test _ =
  let source = {|
pi = 3.14
      |} in
  PyIR.test source ;
  [%expect {| |}]


let%expect_test _ =
  let source = {|
byte_data = b'\x48\x65\x6C\x6C\x6F'  # Equivalent to b'Hello'
      |} in
  PyIR.test source ;
  [%expect {| |}]


let%expect_test _ =
  let source = {|
l = [0, 1, 2, 3, 4, 5]
l[0:2]
l[0:2:1]
          |} in
  PyIR.test source ;
  [%expect {| |}]


let%expect_test _ =
  let source = "True != False" in
  PyIR.test source ;
  [%expect {| |}]


let%expect_test _ =
  let source = {|
l = [1, 2, 3]
print(l[0])
|} in
  PyIR.test source ;
  [%expect {| |}]


let%expect_test _ =
  let source = {|
l = [1, 2, 3]
x = 0
l[x] = 10
|} in
  PyIR.test source ;
  [%expect {| |}]


let%expect_test _ =
  let source = {|
s = {1, 2, 3}
|} in
  PyIR.test source ;
  [%expect {| |}]


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
  [%expect {| |}]


let%expect_test _ =
  let source = {|
fp = open("foo.txt", "wt")
fp.write("yolo")
          |} in
  PyIR.test source ;
  [%expect {| |}]


let%expect_test _ =
  let source = {|
with open("foo.txt", "wt") as fp:
    fp.write("yolo")
          |} in
  PyIR.test source ;
  [%expect {| |}]


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
  [%expect {| |}]


let%expect_test _ =
  let source = {|
x = 1
x = x + (x := 0)
print(x) # will print 1
          |} in
  PyIR.test source ;
  [%expect {| |}]


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
  [%expect {| |}]


let%expect_test _ =
  let source =
    {|
def main(arg):
    def f(x: int, y: str = "ok", z: float = 0.0, *l, key=None):
        return arg

|}
  in
  PyIR.test source ;
  [%expect {| |}]


let%expect_test _ =
  let source = {|
def foo(n):
    o.update({ "key": "*" * n})
|} in
  PyIR.test source ;
  [%expect {| |}]


let%expect_test _ =
  let source = {|
@deco
def foo(n):
    assert(n>0)
|} in
  PyIR.test source ;
  [%expect {| |}]
