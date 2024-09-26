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

      toplevel:
        b0:
          TOPLEVEL[x] <- PYCInt (42)
          return PYCNone |}]


let%expect_test _ =
  let source = {|
x = 42
print(x)
      |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          TOPLEVEL[x] <- PYCInt (42)
          n0 <- TOPLEVEL[print]
          n1 <- TOPLEVEL[x]
          n2 <- n0(n1)
          return PYCNone |}]


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

      toplevel:
        b0:
          TOPLEVEL[x] <- PYCInt (42)
          TOPLEVEL[y] <- PYCInt (10)
          n0 <- TOPLEVEL[print]
          n1 <- TOPLEVEL[x]
          n2 <- TOPLEVEL[y]
          n3 <- $Binary.Add(n1, n2)
          n4 <- n0(n3)
          return PYCNone |}]


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

      toplevel:
        b0:
          TOPLEVEL[x] <- PYCInt (42)
          TOPLEVEL[y] <- PYCInt (10)
          n0 <- TOPLEVEL[print]
          n1 <- TOPLEVEL[x]
          n2 <- TOPLEVEL[y]
          n3 <- $Binary.Subtract(n1, n2)
          n4 <- n0(n3)
          return PYCNone |}]


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

      toplevel:
        b0:
          TOPLEVEL[x] <- PYCInt (42)
          n0 <- TOPLEVEL[x]
          n1 <- $Inplace.Add(n0, PYCInt (10))
          TOPLEVEL[x] <- n1
          n2 <- TOPLEVEL[print]
          n3 <- TOPLEVEL[x]
          n4 <- n2(n3)
          return PYCNone |}]


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

      toplevel:
        b0:
          TOPLEVEL[x] <- PYCInt (42)
          n0 <- TOPLEVEL[x]
          n1 <- $Inplace.Subtract(n0, PYCInt (10))
          TOPLEVEL[x] <- n1
          n2 <- TOPLEVEL[print]
          n3 <- TOPLEVEL[x]
          n4 <- n2(n3)
          return PYCNone |}]


let%expect_test _ =
  let source = {|
pi = 3.14
      |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          TOPLEVEL[pi] <- PYCFloat (3.14)
          return PYCNone |}]


let%expect_test _ =
  let source = {|
byte_data = b'\x48\x65\x6C\x6C\x6F'  # Equivalent to b'Hello'
      |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          TOPLEVEL[byte_data] <- PYCBytes ("Hello")
          return PYCNone |}]


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

      toplevel:
        b0:
          TOPLEVEL[l] <- [PYCInt (0), PYCInt (1), PYCInt (2), PYCInt (3), PYCInt (4), PYCInt (5)]
          n0 <- TOPLEVEL[l]
          n1 <- n0[[PYCInt (0):PYCInt (2)]]
          n2 <- TOPLEVEL[l]
          n3 <- n2[[PYCInt (0):PYCInt (2):PYCInt (1)]]
          return PYCNone |}]


let%expect_test _ =
  let source = "True != False" in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- $Compare.neq(PYCBool (true), PYCBool (false))
          return PYCNone |}]


let%expect_test _ =
  let source = {|
l = [1, 2, 3]
print(l[0])
|} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          TOPLEVEL[l] <- [PYCInt (1), PYCInt (2), PYCInt (3)]
          n0 <- TOPLEVEL[print]
          n1 <- TOPLEVEL[l]
          n2 <- n1[PYCInt (0)]
          n3 <- n0(n2)
          return PYCNone |}]


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

      toplevel:
        b0:
          TOPLEVEL[l] <- [PYCInt (1), PYCInt (2), PYCInt (3)]
          TOPLEVEL[x] <- PYCInt (0)
          n0 <- TOPLEVEL[l]
          n1 <- TOPLEVEL[x]
          n0[n1] <- PYCInt (10)
          return PYCNone |}]


let%expect_test _ =
  let source = {|
s = {1, 2, 3}
|} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          TOPLEVEL[s] <- {PYCInt (1), PYCInt (2), PYCInt (3)}
          return PYCNone |}]


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

      toplevel:
        b0:
          TOPLEVEL[x] <- PYCString ("1")
          n0 <- TOPLEVEL[x]
          TOPLEVEL[s] <- {|n0, PYCInt (1), PYCString ("2"), PYCInt (2)|}
          n1 <- TOPLEVEL[print]
          n2 <- TOPLEVEL[s]
          n3 <- n1(n2)
          TOPLEVEL[s] <- {PYCString ("a"): PYCInt (42), PYCString ("b"): PYCInt (1664), }
          n4 <- TOPLEVEL[print]
          n5 <- TOPLEVEL[s]
          n6 <- n5[PYCString ("1")]
          n7 <- n4(n6)
          TOPLEVEL[d] <- {PYCInt (1): PYCNone, PYCInt (120): PYCString ("abc"),
                          PYCInt (121): PYCString (""), PYCBytes ("abc"):
                          PYCInt (120), }
          return PYCNone |xxx}]


let%expect_test _ =
  let source = {|
fp = open("foo.txt", "wt")
fp.write("yolo")
          |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- TOPLEVEL[open]
          n1 <- n0(PYCString ("foo.txt"), PYCString ("wt"))
          TOPLEVEL[fp] <- n1
          n2 <- TOPLEVEL[fp]
          n3 <- n2.write(PYCString ("yolo"))
          return PYCNone |}]


let%expect_test _ =
  let source = {|
with open("foo.txt", "wt") as fp:
    fp.write("yolo")
          |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- TOPLEVEL[open]
          n1 <- n0(PYCString ("foo.txt"), PYCString ("wt"))
          n2 <- $LoadMethod(n1, __enter__)()
          TOPLEVEL[fp] <- n2
          n4 <- TOPLEVEL[fp]
          n5 <- n4.write(PYCString ("yolo"))
          jmp b1(CM(n1).__exit__)

        b1:
          n7 <- n3(PYCNone, PYCNone, PYCNone)
          return PYCNone |}]


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

      toplevel:
        b0:
          TOPLEVEL[values] <- [PYCInt (1), PYCInt (2), [PYCInt (3), PYCInt (4)], PYCInt (5)]
          TOPLEVEL[values2] <- PYCTuple ([|PYCString ("a"); PYCString ("b")|])
          n0 <- TOPLEVEL[values]
          n1 <- TOPLEVEL[values2]
          TOPLEVEL[result] <- (packed)($Packed([PYCInt (10), PYCInt (100)]), $Packed(n0), $Packed(n1))
          n2 <- TOPLEVEL[print]
          n3 <- TOPLEVEL[result]
          n4 <- n2(n3)
          n5 <- TOPLEVEL[values]
          n6 <- TOPLEVEL[values2]
          TOPLEVEL[result] <- (packed)[$Packed(n5), $Packed(n6)]
          n7 <- TOPLEVEL[print]
          n8 <- TOPLEVEL[result]
          n9 <- n7(n8)
          return PYCNone |}]


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

      toplevel:
        b0:
          TOPLEVEL[x] <- PYCInt (1)
          n0 <- TOPLEVEL[x]
          TOPLEVEL[x] <- PYCInt (0)
          n1 <- $Binary.Add(n0, PYCInt (0))
          TOPLEVEL[x] <- n1
          n2 <- TOPLEVEL[print]
          n3 <- TOPLEVEL[x]
          n4 <- n2(n3)
          return PYCNone |}]
