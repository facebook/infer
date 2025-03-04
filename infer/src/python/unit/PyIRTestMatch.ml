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
  [%expect {| |}]


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
  [%expect {| |}]


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
  [%expect {| |}]


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
  [%expect {| |}]


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
  [%expect {| |}]


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
  [%expect {| |}]


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
  [%expect {| |}]


let%expect_test _ =
  let source =
    {|
def main():
    match o:
        case [ ast.Cons1(_, ast.Const2([a, b, c])) ]:
            action(a, b, c)
|}
  in
  PyIR.test source ;
  [%expect {| |}]
