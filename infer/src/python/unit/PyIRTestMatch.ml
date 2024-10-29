(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

let%expect_test _ =
  let source =
    {|
match n:
    case 0:
        print("0")
    case 1:
        print("1")
    case 2|3|4:
        print("2..4")
|}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- TOPLEVEL[n]
          n1 <- $Compare.eq(n0, 0, None)
          if n1 then jmp b1 else jmp b2

        b1:
          n10 <- TOPLEVEL[print]
          n11 <- $Call(n10, "0", None)
          return None

        b10:
          return None

        b11:
          n6 <- TOPLEVEL[print]
          n7 <- $Call(n6, "2..4", None)
          return None

        b2:
          n2 <- $Compare.eq(n0, 1, None)
          if n2 then jmp b3 else jmp b4

        b3:
          n8 <- TOPLEVEL[print]
          n9 <- $Call(n8, "1", None)
          return None

        b4:
          n3 <- $Compare.eq(n0, 2, None)
          if n3 then jmp b5 else jmp b6

        b5:
          jmp b11

        b6:
          n4 <- $Compare.eq(n0, 3, None)
          if n4 then jmp b7 else jmp b8

        b7:
          jmp b11

        b8:
          n5 <- $Compare.eq(n0, 4, None)
          if n5 then jmp b9 else jmp b10

        b9:
          jmp b11 |}]


let%expect_test _ =
  let source =
    {|
def foo(n):
    match n:
        case int():
            print('int')
        case float():
            print('float')

foo(1)
foo(1.1)
|}
  in
  PyIR.test source ;
  [%expect {|
    IR error: Unsupported opcode: MATCH_CLASS |}]


let%expect_test _ =
  let source =
    {|
class C:
    FOO = "FOO"
    BAR = "BAR"

def foo(n):
    match n:
        case C.FOO:
            print('FOO')
        case C.BAR:
            print('BAR')

foo(C.FOO)
foo(C.BAR)
|}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $MakeFunction["C", "dummy.C", None, None, None, None]
          n1 <- $BuildClass(n0, "C", None)
          TOPLEVEL[C] <- n1
          n2 <- $MakeFunction["foo", "dummy.foo", None, None, None, None]
          TOPLEVEL[foo] <- n2
          n3 <- TOPLEVEL[foo]
          n4 <- TOPLEVEL[C]
          n5 <- n4.FOO
          n6 <- $Call(n3, n5, None)
          n7 <- TOPLEVEL[foo]
          n8 <- TOPLEVEL[C]
          n9 <- n8.BAR
          n10 <- $Call(n7, n9, None)
          return None


      function dummy.C():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          TOPLEVEL[FOO] <- "FOO"
          TOPLEVEL[BAR] <- "BAR"
          return None


      function dummy.foo(n):
        b0:
          n0 <- LOCAL[n]
          n1 <- GLOBAL[C]
          n2 <- n1.FOO
          n3 <- $Compare.eq(n0, n2, None)
          if n3 then jmp b1 else jmp b2

        b1:
          n9 <- GLOBAL[print]
          n10 <- $Call(n9, "FOO", None)
          return None

        b2:
          n4 <- GLOBAL[C]
          n5 <- n4.BAR
          n6 <- $Compare.eq(n0, n5, None)
          if n6 then jmp b3 else jmp b4

        b3:
          n7 <- GLOBAL[print]
          n8 <- $Call(n7, "BAR", None)
          return None

        b4:
          return None |}]


let%expect_test _ =
  let source =
    {|
class C:
    FOO = "FOO"
    BAR = "BAR"

def foo(n, m):
    match (n, m):
        case (C.FOO, C.FOO):
            print('FOO')
        case (C.BAR, C.BAR):
            print('BAR')

foo(C.FOO, C.FOO)
foo(C.BAR, C.BAR)
|}
  in
  PyIR.test source ;
  [%expect {|
      IR error: Unsupported opcode: MATCH_SEQUENCE |}]


let%expect_test _ =
  let source =
    {|
class Point:
    x: int
    y: int

def location(point):
    match point:
        case Point(x=0, y=0):
            print("Origin is the point's location.")
        case Point(x=0, y=y):
            print(f"Y={y} and the point is on the y-axis.")
        case Point(x=x, y=0):
            print(f"X={x} and the point is on the x-axis.")
        case Point():
            print("The point is located somewhere else on the plane.")
        case _:
            print("Not a point")
|}
  in
  PyIR.test source ;
  [%expect {|
    IR error: Unsupported opcode: MATCH_CLASS |}]


let%expect_test _ =
  let source =
    {|
class Point:
    x: int
    y: int
    __match_args__ = ('x', 'y')

def location(point):
    match point:
        case Point(0, 0):
            print("Origin is the point's location.")
        case Point(0, y):
            print(f"Y={y} and the point is on the y-axis.")
        case Point(x, 0):
            print(f"X={x} and the point is on the x-axis.")
        case Point():
            print("The point is located somewhere else on the plane.")
        case _:
            print("Not a point")
|}
  in
  PyIR.test source ;
  [%expect {|
    IR error: Unsupported opcode: MATCH_CLASS |}]


let%expect_test _ =
  let source =
    {|
class Point:
    x: int
    y: int
    __match_args__ = ('x', 'y')

def location(point):
    match points:
        case [Point(0, y1), Point(0, y2)]:
            print(f"Two points on the Y axis at {y1}, {y2} are in the list.")
        case _:
            print("Something else is found in the list.")
|}
  in
  PyIR.test source ;
  [%expect {|
      IR error: Unsupported opcode: MATCH_SEQUENCE |}]
