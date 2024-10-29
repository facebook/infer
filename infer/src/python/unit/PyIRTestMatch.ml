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
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $MakeFunction["foo", "dummy.foo", None, None, None, None]
          TOPLEVEL[foo] <- n0
          n1 <- TOPLEVEL[foo]
          n2 <- $Call(n1, 1, None)
          n3 <- TOPLEVEL[foo]
          n4 <- $Call(n3, 1.1, None)
          return None


      function dummy.foo(n):
        b0:
          n0 <- LOCAL[n]
          n1 <- GLOBAL[int]
          n2 <- $MatchClass(n0, n1, 0, $BuildTuple())
          if $BoolOfMatchClass(n2) then jmp b1 else jmp b2

        b1:
          n10 <- $AttributesOfMatchClass(n2)
          n11 <- GLOBAL[print]
          n12 <- $Call(n11, "int", None)
          return None

        b2:
          n3 <- $AttributesOfMatchClass(n2)
          n4 <- GLOBAL[float]
          n5 <- $MatchClass(n0, n4, 0, $BuildTuple())
          if $BoolOfMatchClass(n5) then jmp b3 else jmp b4

        b3:
          n7 <- $AttributesOfMatchClass(n5)
          n8 <- GLOBAL[print]
          n9 <- $Call(n8, "float", None)
          return None

        b4:
          n6 <- $AttributesOfMatchClass(n5)
          return None |}]


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
            n6 <- TOPLEVEL[C]
            n7 <- n6.FOO
            n8 <- $Call(n3, n5, n7, None)
            n9 <- TOPLEVEL[foo]
            n10 <- TOPLEVEL[C]
            n11 <- n10.BAR
            n12 <- TOPLEVEL[C]
            n13 <- n12.BAR
            n14 <- $Call(n9, n11, n13, None)
            return None


        function dummy.C():
          b0:
            n0 <- TOPLEVEL[__name__]
            TOPLEVEL[__module__] <- n0
            TOPLEVEL[__qualname__] <- "C"
            TOPLEVEL[FOO] <- "FOO"
            TOPLEVEL[BAR] <- "BAR"
            return None


        function dummy.foo(n, m):
          b0:
            n0 <- LOCAL[n]
            n1 <- LOCAL[m]
            if $MatchSequence($BuildTuple(n0, n1)) then jmp b1 else jmp b5(
            $BuildTuple(n0, n1))

          b1:
            n2 <- $Compare.eq($GetLen($BuildTuple(n0, n1)), 2, None)
            if n2 then jmp b2 else jmp b5($BuildTuple(n0, n1))

          b10:
            n18 <- GLOBAL[print]
            n19 <- $Call(n18, "BAR", None)
            return None

          b11(n14):
            return None

          b12:
            return None

          b2:
            n3 <- GLOBAL[C]
            n4 <- n3.FOO
            n5 <- $Compare.eq($BuildTuple(n0, n1)[0], n4, None)
            if n5 then jmp b3 else jmp b5($BuildTuple(n0, n1)[1])

          b3:
            n7 <- GLOBAL[C]
            n8 <- n7.FOO
            n9 <- $Compare.eq($BuildTuple(n0, n1)[1], n8, None)
            if n9 then jmp b4 else jmp b6

          b4:
            n20 <- $BuildTuple(n0, n1)
            n21 <- GLOBAL[print]
            n22 <- $Call(n21, "FOO", None)
            return None

          b5(n6):
            jmp b6

          b6:
            if $MatchSequence($BuildTuple(n0, n1)) then jmp b7 else jmp b11($BuildTuple(n0, n1))

          b7:
            n10 <- $Compare.eq($GetLen($BuildTuple(n0, n1)), 2, None)
            if n10 then jmp b8 else jmp b11($BuildTuple(n0, n1))

          b8:
            n11 <- GLOBAL[C]
            n12 <- n11.BAR
            n13 <- $Compare.eq($BuildTuple(n0, n1)[0], n12, None)
            if n13 then jmp b9 else jmp b11($BuildTuple(n0, n1)[1])

          b9:
            n15 <- GLOBAL[C]
            n16 <- n15.BAR
            n17 <- $Compare.eq($BuildTuple(n0, n1)[1], n16, None)
            if n17 then jmp b10 else jmp b12 |}]


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
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $MakeFunction["Point", "dummy.Point", None, None, None, None]
          n1 <- $BuildClass(n0, "Point", None)
          TOPLEVEL[Point] <- n1
          n2 <- $MakeFunction["location", "dummy.location", None, None, None, None]
          TOPLEVEL[location] <- n2
          return None


      function dummy.Point():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "Point"
          $SETUP_ANNOTATIONS
          n1 <- TOPLEVEL[int]
          n2 <- TOPLEVEL[__annotations__]
          n2["x"] <- n1
          n3 <- TOPLEVEL[int]
          n4 <- TOPLEVEL[__annotations__]
          n4["y"] <- n3
          return None


      function dummy.location(point):
        b0:
          n0 <- LOCAL[point]
          n1 <- GLOBAL[Point]
          n2 <- $MatchClass(n0, n1, 0, $BuildTuple("x", "y"))
          if $BoolOfMatchClass(n2) then jmp b1 else jmp b4

        b1:
          n3 <- $AttributesOfMatchClass(n2)[0]
          n4 <- $Compare.eq(n3, 0, None)
          if n4 then jmp b2 else jmp b4

        b10:
          n18 <- $AttributesOfMatchClass(n14)
          jmp b11(n15)

        b11(n19):
          n20 <- GLOBAL[Point]
          n21 <- $MatchClass(n0, n20, 0, $BuildTuple())
          if $BoolOfMatchClass(n21) then jmp b12 else jmp b13

        b12:
          n25 <- $AttributesOfMatchClass(n21)
          n26 <- GLOBAL[print]
          n27 <- $Call(n26, "The point is located somewhere else on the plane.", None)
          return None

        b13:
          n22 <- $AttributesOfMatchClass(n21)
          n23 <- GLOBAL[print]
          n24 <- $Call(n23, "Not a point", None)
          return None

        b2:
          n5 <- $AttributesOfMatchClass(n2)[1]
          n6 <- $Compare.eq(n5, 0, None)
          if n6 then jmp b3 else jmp b4

        b3:
          n39 <- $AttributesOfMatchClass(n2)
          n40 <- GLOBAL[print]
          n41 <- $Call(n40, "Origin is the point's location.", None)
          return None

        b4:
          n7 <- $AttributesOfMatchClass(n2)
          n8 <- GLOBAL[Point]
          n9 <- $MatchClass(n0, n8, 0, $BuildTuple("x", "y"))
          if $BoolOfMatchClass(n9) then jmp b5 else jmp b7

        b5:
          n10 <- $AttributesOfMatchClass(n9)[0]
          n11 <- $Compare.eq(n10, 0, None)
          if n11 then jmp b6 else jmp b7

        b6:
          n33 <- $AttributesOfMatchClass(n9)[1]
          n34 <- $AttributesOfMatchClass(n9)
          LOCAL[y] <- n33
          n35 <- GLOBAL[print]
          n36 <- LOCAL[y]
          n37 <- $Format(n36, None, None)
          n38 <- $Call(n35, $BuildString("Y=", n37, " and the point is on the y-axis."), None)
          return None

        b7:
          n12 <- $AttributesOfMatchClass(n9)
          n13 <- GLOBAL[Point]
          n14 <- $MatchClass(n0, n13, 0, $BuildTuple("x", "y"))
          if $BoolOfMatchClass(n14) then jmp b8 else jmp b11($AttributesOfMatchClass(n14))

        b8:
          n15 <- $AttributesOfMatchClass(n14)[0]
          n16 <- $AttributesOfMatchClass(n14)[1]
          n17 <- $Compare.eq(n16, 0, None)
          if n17 then jmp b9 else jmp b10

        b9:
          n28 <- $AttributesOfMatchClass(n14)
          LOCAL[x] <- n15
          n29 <- GLOBAL[print]
          n30 <- LOCAL[x]
          n31 <- $Format(n30, None, None)
          n32 <- $Call(n29, $BuildString("X=", n31, " and the point is on the x-axis."), None)
          return None |}]


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
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $MakeFunction["Point", "dummy.Point", None, None, None, None]
          n1 <- $BuildClass(n0, "Point", None)
          TOPLEVEL[Point] <- n1
          n2 <- $MakeFunction["location", "dummy.location", None, None, None, None]
          TOPLEVEL[location] <- n2
          return None


      function dummy.Point():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "Point"
          $SETUP_ANNOTATIONS
          n1 <- TOPLEVEL[int]
          n2 <- TOPLEVEL[__annotations__]
          n2["x"] <- n1
          n3 <- TOPLEVEL[int]
          n4 <- TOPLEVEL[__annotations__]
          n4["y"] <- n3
          TOPLEVEL[__match_args__] <- $BuildTuple("x", "y")
          return None


      function dummy.location(point):
        b0:
          n0 <- LOCAL[point]
          n1 <- GLOBAL[Point]
          n2 <- $MatchClass(n0, n1, 2, $BuildTuple())
          if $BoolOfMatchClass(n2) then jmp b1 else jmp b4

        b1:
          n3 <- $AttributesOfMatchClass(n2)[0]
          n4 <- $Compare.eq(n3, 0, None)
          if n4 then jmp b2 else jmp b4

        b10:
          n18 <- $AttributesOfMatchClass(n14)
          jmp b11(n15)

        b11(n19):
          n20 <- GLOBAL[Point]
          n21 <- $MatchClass(n0, n20, 0, $BuildTuple())
          if $BoolOfMatchClass(n21) then jmp b12 else jmp b13

        b12:
          n25 <- $AttributesOfMatchClass(n21)
          n26 <- GLOBAL[print]
          n27 <- $Call(n26, "The point is located somewhere else on the plane.", None)
          return None

        b13:
          n22 <- $AttributesOfMatchClass(n21)
          n23 <- GLOBAL[print]
          n24 <- $Call(n23, "Not a point", None)
          return None

        b2:
          n5 <- $AttributesOfMatchClass(n2)[1]
          n6 <- $Compare.eq(n5, 0, None)
          if n6 then jmp b3 else jmp b4

        b3:
          n39 <- $AttributesOfMatchClass(n2)
          n40 <- GLOBAL[print]
          n41 <- $Call(n40, "Origin is the point's location.", None)
          return None

        b4:
          n7 <- $AttributesOfMatchClass(n2)
          n8 <- GLOBAL[Point]
          n9 <- $MatchClass(n0, n8, 2, $BuildTuple())
          if $BoolOfMatchClass(n9) then jmp b5 else jmp b7

        b5:
          n10 <- $AttributesOfMatchClass(n9)[0]
          n11 <- $Compare.eq(n10, 0, None)
          if n11 then jmp b6 else jmp b7

        b6:
          n33 <- $AttributesOfMatchClass(n9)[1]
          n34 <- $AttributesOfMatchClass(n9)
          LOCAL[y] <- n33
          n35 <- GLOBAL[print]
          n36 <- LOCAL[y]
          n37 <- $Format(n36, None, None)
          n38 <- $Call(n35, $BuildString("Y=", n37, " and the point is on the y-axis."), None)
          return None

        b7:
          n12 <- $AttributesOfMatchClass(n9)
          n13 <- GLOBAL[Point]
          n14 <- $MatchClass(n0, n13, 2, $BuildTuple())
          if $BoolOfMatchClass(n14) then jmp b8 else jmp b11($AttributesOfMatchClass(n14))

        b8:
          n15 <- $AttributesOfMatchClass(n14)[0]
          n16 <- $AttributesOfMatchClass(n14)[1]
          n17 <- $Compare.eq(n16, 0, None)
          if n17 then jmp b9 else jmp b10

        b9:
          n28 <- $AttributesOfMatchClass(n14)
          LOCAL[x] <- n15
          n29 <- GLOBAL[print]
          n30 <- LOCAL[x]
          n31 <- $Format(n30, None, None)
          n32 <- $Call(n29, $BuildString("X=", n31, " and the point is on the x-axis."), None)
          return None |}]


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
  [%expect
    {|
      module dummy:

        function toplevel():
          b0:
            n0 <- $MakeFunction["Point", "dummy.Point", None, None, None, None]
            n1 <- $BuildClass(n0, "Point", None)
            TOPLEVEL[Point] <- n1
            n2 <- $MakeFunction["location", "dummy.location", None, None, None, None]
            TOPLEVEL[location] <- n2
            return None


        function dummy.Point():
          b0:
            n0 <- TOPLEVEL[__name__]
            TOPLEVEL[__module__] <- n0
            TOPLEVEL[__qualname__] <- "Point"
            $SETUP_ANNOTATIONS
            n1 <- TOPLEVEL[int]
            n2 <- TOPLEVEL[__annotations__]
            n2["x"] <- n1
            n3 <- TOPLEVEL[int]
            n4 <- TOPLEVEL[__annotations__]
            n4["y"] <- n3
            TOPLEVEL[__match_args__] <- $BuildTuple("x", "y")
            return None


        function dummy.location(point):
          b0:
            n0 <- GLOBAL[points]
            if $MatchSequence(n0) then jmp b1 else jmp b8(n0)

          b1:
            n1 <- $Compare.eq($GetLen(n0), 2, None)
            if n1 then jmp b2 else jmp b8(n0)

          b2:
            n2 <- GLOBAL[Point]
            n3 <- $MatchClass(n0[0], n2, 2, $BuildTuple())
            if $BoolOfMatchClass(n3) then jmp b3 else jmp b7(n0[1], $AttributesOfMatchClass(n3))

          b3:
            n4 <- $AttributesOfMatchClass(n3)[0]
            n5 <- $Compare.eq(n4, 0, None)
            if n5 then jmp b4 else jmp b7(n0[1], $AttributesOfMatchClass(n3))

          b4:
            n6 <- $AttributesOfMatchClass(n3)[1]
            n7 <- $AttributesOfMatchClass(n3)
            n8 <- GLOBAL[Point]
            n9 <- $MatchClass(n0[1], n8, 2, $BuildTuple())
            if $BoolOfMatchClass(n9) then jmp b5 else jmp b7(n6, $AttributesOfMatchClass(n9))

          b5:
            n10 <- $AttributesOfMatchClass(n9)[0]
            n11 <- $Compare.eq(n10, 0, None)
            if n11 then jmp b6 else jmp b7(n6, $AttributesOfMatchClass(n9))

          b6:
            n17 <- $AttributesOfMatchClass(n9)[1]
            n18 <- $AttributesOfMatchClass(n9)
            LOCAL[y1] <- n6
            LOCAL[y2] <- n17
            n19 <- GLOBAL[print]
            n20 <- LOCAL[y1]
            n21 <- $Format(n20, None, None)
            n22 <- LOCAL[y2]
            n23 <- $Format(n22, None, None)
            n24 <- $Call(n19, $BuildString("Two points on the Y axis at ", n21, ", ", n23, " are in the list."), None)
            return None

          b7(n12, n13):
            jmp b8(n12)

          b8(n14):
            n15 <- GLOBAL[print]
            n16 <- $Call(n15, "Something else is found in the list.", None)
            return None |}]


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
  [%expect
    {|
      module dummy:

        function toplevel():
          b0:
            n0 <- $MakeFunction["main", "dummy.main", None, None, None, None]
            TOPLEVEL[main] <- n0
            return None


        function dummy.main(a, b, c):
          b0:
            n0 <- GLOBAL[o]
            if $MatchSequence(n0) then jmp b1 else jmp b9(n0)

          b1:
            n1 <- $Compare.eq($GetLen(n0), 1, None)
            if n1 then jmp b2 else jmp b9(n0)

          b2:
            n2 <- GLOBAL[ast]
            n3 <- n2.Cons1
            n4 <- $MatchClass(n0[0], n3, 2, $BuildTuple())
            if $BoolOfMatchClass(n4) then jmp b3 else jmp b9($AttributesOfMatchClass(n4))

          b3:
            n5 <- $AttributesOfMatchClass(n4)[1]
            n6 <- GLOBAL[ast]
            n7 <- n6.Const2
            n8 <- $MatchClass(n5, n7, 1, $BuildTuple())
            if $BoolOfMatchClass(n8) then jmp b4 else jmp b8

          b4:
            n9 <- $AttributesOfMatchClass(n8)[0]
            if $MatchSequence(n9) then jmp b5 else jmp b7

          b5:
            n10 <- $Compare.eq($GetLen(n9), 3, None)
            if n10 then jmp b6 else jmp b7

          b6:
            n13 <- $AttributesOfMatchClass(n8)
            n14 <- $AttributesOfMatchClass(n4)
            LOCAL[a] <- n9[0]
            LOCAL[b] <- n9[1]
            LOCAL[c] <- n9[2]
            n15 <- GLOBAL[action]
            n16 <- LOCAL[a]
            n17 <- LOCAL[b]
            n18 <- LOCAL[c]
            n19 <- $Call(n15, n16, n17, n18, None)
            return None

          b7:
            jmp b8

          b8:
            n11 <- $AttributesOfMatchClass(n8)
            jmp b9($AttributesOfMatchClass(n4))

          b9(n12):
            return None |}]
