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
          x(PyIR.Name) <- PYCInt (42)
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
          x(PyIR.Name) <- PYCInt (42)
          n0 <- print(PyIR.Name)(x(PyIR.Name))
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
          x(PyIR.Name) <- PYCInt (42)
          y(PyIR.Name) <- PYCInt (10)
          n0 <- $Binary.Add(x(PyIR.Name), y(PyIR.Name))
          n1 <- print(PyIR.Name)(n0)
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
          x(PyIR.Name) <- PYCInt (42)
          y(PyIR.Name) <- PYCInt (10)
          n0 <- $Binary.Subtract(x(PyIR.Name), y(PyIR.Name))
          n1 <- print(PyIR.Name)(n0)
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
          x(PyIR.Name) <- PYCInt (42)
          n0 <- $Inplace.Add(x(PyIR.Name), PYCInt (10))
          x(PyIR.Name) <- n0
          n1 <- print(PyIR.Name)(x(PyIR.Name))
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
          x(PyIR.Name) <- PYCInt (42)
          n0 <- $Inplace.Subtract(x(PyIR.Name), PYCInt (10))
          x(PyIR.Name) <- n0
          n1 <- print(PyIR.Name)(x(PyIR.Name))
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
          pi(PyIR.Name) <- PYCFloat (3.14)
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
          byte_data(PyIR.Name) <- PYCBytes ("Hello")
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
          l(PyIR.Name) <- [PYCInt (0), PYCInt (1), PYCInt (2), PYCInt (3), PYCInt (4), PYCInt (5)]
          n0 <- l(PyIR.Name)[[PYCInt (0):PYCInt (2)]]
          n1 <- l(PyIR.Name)[[PYCInt (0):PYCInt (2):PYCInt (1)]]
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
          l(PyIR.Name) <- [PYCInt (1), PYCInt (2), PYCInt (3)]
          n0 <- print(PyIR.Name)(l(PyIR.Name)[PYCInt (0)])
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
          l(PyIR.Name) <- [PYCInt (1), PYCInt (2), PYCInt (3)]
          x(PyIR.Name) <- PYCInt (0)
          l(PyIR.Name)[x(PyIR.Name)] <- PYCInt (10)
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
          s(PyIR.Name) <- {PYCInt (1), PYCInt (2), PYCInt (3)}
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
          x(PyIR.Name) <- PYCString ("1")
          s(PyIR.Name) <- {|x(PyIR.Name), PYCInt (1), PYCString ("2"), PYCInt (2)|}
          n0 <- print(PyIR.Name)(s(PyIR.Name))
          s(PyIR.Name) <- {PYCString ("a"): PYCInt (42), PYCString ("b"): PYCInt (1664), }
          n1 <- print(PyIR.Name)(s(PyIR.Name)[PYCString ("1")])
          d(PyIR.Name) <- {PYCInt (1): PYCNone, PYCInt (120): PYCString ("abc"),
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
          n0 <- open(PyIR.Name)(PYCString ("foo.txt"), PYCString ("wt"))
          fp(PyIR.Name) <- n0
          n1 <- $CallMethod($LoadMethod(fp(PyIR.Name), write), PYCString ("yolo"))
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
          n0 <- open(PyIR.Name)(PYCString ("foo.txt"), PYCString ("wt"))
          n1 <- $LoadMethod(n0, __enter__)()
          fp(PyIR.Name) <- n1
          n3 <- $CallMethod($LoadMethod(fp(PyIR.Name), write), PYCString ("yolo"))
          jmp b1(CM(n0).__exit__)

        b1:
          n5 <- n2(PYCNone, PYCNone, PYCNone)
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
          values(PyIR.Name) <- [PYCInt (1), PYCInt (2), [PYCInt (3), PYCInt (4)], PYCInt (5)]
          values2(PyIR.Name) <- PYCTuple ([|PYCString ("a"); PYCString ("b")|])
          result(PyIR.Name) <- (packed)($Packed([PYCInt (10), PYCInt (100)]), $Packed(values(PyIR.Name)), $Packed(values2(PyIR.Name)))
          n0 <- print(PyIR.Name)(result(PyIR.Name))
          result(PyIR.Name) <- (packed)[$Packed(values(PyIR.Name)), $Packed(values2(PyIR.Name))]
          n1 <- print(PyIR.Name)(result(PyIR.Name))
          return PYCNone |}]
