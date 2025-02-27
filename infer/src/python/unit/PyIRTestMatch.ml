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
          n0 <- None
          n3 <- TOPLEVEL[n]
          n4 <- $Compare.eq(n3, 0, n0)
          if n4 then jmp b1 else jmp b2

        b1:
          n13 <- TOPLEVEL[print]
          n14 <- $Call(n13, "0", n0)
          return n0

        b10:
          return n0

        b11:
          n9 <- TOPLEVEL[print]
          n10 <- $Call(n9, "2..4", n0)
          return n0

        b2:
          n5 <- $Compare.eq(n3, 1, n0)
          if n5 then jmp b3 else jmp b4

        b3:
          n11 <- TOPLEVEL[print]
          n12 <- $Call(n11, "1", n0)
          return n0

        b4:
          n6 <- $Compare.eq(n3, 2, n0)
          if n6 then jmp b5 else jmp b6

        b5:
          jmp b11

        b6:
          n7 <- $Compare.eq(n3, 3, n0)
          if n7 then jmp b7 else jmp b8

        b7:
          jmp b11

        b8:
          n8 <- $Compare.eq(n3, 4, n0)
          if n8 then jmp b9 else jmp b10

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
          n0 <- None
          n3 <- $MakeFunction["dummy.foo", n0, n0, n0, n0]
          TOPLEVEL[foo] <- n3
          n4 <- TOPLEVEL[foo]
          n5 <- $Call(n4, 1, n0)
          n6 <- TOPLEVEL[foo]
          n7 <- $Call(n6, 1.1, n0)
          return n0


      function dummy.foo(n):
        b0:
          n0 <- None
          n3 <- LOCAL[n]
          n4 <- GLOBAL[int]
          n5 <- $MatchClass(n3, n4, 0, $BuildTuple())
          if $BoolOfMatchClass(n5) then jmp b1 else jmp b2

        b1:
          n10 <- GLOBAL[print]
          n11 <- $Call(n10, "int", n0)
          return n0

        b2:
          n6 <- GLOBAL[float]
          n7 <- $MatchClass(n3, n6, 0, $BuildTuple())
          if $BoolOfMatchClass(n7) then jmp b3 else jmp b4

        b3:
          n8 <- GLOBAL[print]
          n9 <- $Call(n8, "float", n0)
          return n0

        b4:
          return n0 |}]


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
          n0 <- None
          n3 <- $MakeFunction["dummy.C", n0, n0, n0, n0]
          n4 <- $BuildClass(n3, "C", n0)
          TOPLEVEL[C] <- n4
          n5 <- $MakeFunction["dummy.foo", n0, n0, n0, n0]
          TOPLEVEL[foo] <- n5
          n6 <- TOPLEVEL[foo]
          n7 <- TOPLEVEL[C]
          n8 <- n7.FOO
          n9 <- $Call(n6, n8, n0)
          n10 <- TOPLEVEL[foo]
          n11 <- TOPLEVEL[C]
          n12 <- n11.BAR
          n13 <- $Call(n10, n12, n0)
          return n0


      function dummy.C():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "C"
          TOPLEVEL[FOO] <- "FOO"
          TOPLEVEL[BAR] <- "BAR"
          return n0


      function dummy.foo(n):
        b0:
          n0 <- None
          n3 <- LOCAL[n]
          n4 <- GLOBAL[C]
          n5 <- n4.FOO
          n6 <- $Compare.eq(n3, n5, n0)
          if n6 then jmp b1 else jmp b2

        b1:
          n12 <- GLOBAL[print]
          n13 <- $Call(n12, "FOO", n0)
          return n0

        b2:
          n7 <- GLOBAL[C]
          n8 <- n7.BAR
          n9 <- $Compare.eq(n3, n8, n0)
          if n9 then jmp b3 else jmp b4

        b3:
          n10 <- GLOBAL[print]
          n11 <- $Call(n10, "BAR", n0)
          return n0

        b4:
          return n0 |}]


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
          n0 <- None
          n3 <- $MakeFunction["dummy.C", n0, n0, n0, n0]
          n4 <- $BuildClass(n3, "C", n0)
          TOPLEVEL[C] <- n4
          n5 <- $MakeFunction["dummy.foo", n0, n0, n0, n0]
          TOPLEVEL[foo] <- n5
          n6 <- TOPLEVEL[foo]
          n7 <- TOPLEVEL[C]
          n8 <- n7.FOO
          n9 <- TOPLEVEL[C]
          n10 <- n9.FOO
          n11 <- $Call(n6, n8, n10, n0)
          n12 <- TOPLEVEL[foo]
          n13 <- TOPLEVEL[C]
          n14 <- n13.BAR
          n15 <- TOPLEVEL[C]
          n16 <- n15.BAR
          n17 <- $Call(n12, n14, n16, n0)
          return n0


      function dummy.C():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "C"
          TOPLEVEL[FOO] <- "FOO"
          TOPLEVEL[BAR] <- "BAR"
          return n0


      function dummy.foo(n, m):
        b0:
          n0 <- None
          n3 <- LOCAL[n]
          n4 <- LOCAL[m]
          n5 <- $BuildTuple(n3, n4)
          if $MatchSequence(n5) then jmp b1 else jmp b5(n5)

        b1:
          n6 <- $Compare.eq($GetLen(n5), 2, n0)
          if n6 then jmp b2 else jmp b5(n5)

        b10:
          n22 <- GLOBAL[print]
          n23 <- $Call(n22, "BAR", n0)
          return n0

        b11(n18):
          return n0

        b12:
          return n0

        b2:
          n7 <- GLOBAL[C]
          n8 <- n7.FOO
          n9 <- $Compare.eq(n5[0], n8, n0)
          if n9 then jmp b3 else jmp b5(n5[1])

        b3:
          n11 <- GLOBAL[C]
          n12 <- n11.FOO
          n13 <- $Compare.eq(n5[1], n12, n0)
          if n13 then jmp b4 else jmp b6

        b4:
          n24 <- GLOBAL[print]
          n25 <- $Call(n24, "FOO", n0)
          return n0

        b5(n10):
          jmp b6

        b6:
          if $MatchSequence(n5) then jmp b7 else jmp b11(n5)

        b7:
          n14 <- $Compare.eq($GetLen(n5), 2, n0)
          if n14 then jmp b8 else jmp b11(n5)

        b8:
          n15 <- GLOBAL[C]
          n16 <- n15.BAR
          n17 <- $Compare.eq(n5[0], n16, n0)
          if n17 then jmp b9 else jmp b11(n5[1])

        b9:
          n19 <- GLOBAL[C]
          n20 <- n19.BAR
          n21 <- $Compare.eq(n5[1], n20, n0)
          if n21 then jmp b10 else jmp b12 |}]


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
          n0 <- None
          n3 <- $MakeFunction["dummy.Point", n0, n0, n0, n0]
          n4 <- $BuildClass(n3, "Point", n0)
          TOPLEVEL[Point] <- n4
          n5 <- $MakeFunction["dummy.location", n0, n0, n0, n0]
          TOPLEVEL[location] <- n5
          return n0


      function dummy.Point():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "Point"
          $SETUP_ANNOTATIONS
          n4 <- TOPLEVEL[int]
          n5 <- TOPLEVEL[__annotations__]
          n5["x"] <- n4
          n6 <- TOPLEVEL[int]
          n7 <- TOPLEVEL[__annotations__]
          n7["y"] <- n6
          return n0


      function dummy.location(point):
        b0:
          n0 <- None
          n3 <- LOCAL[point]
          n4 <- GLOBAL[Point]
          n5 <- $MatchClass(n3, n4, 0, $BuildTuple("x", "y"))
          if $BoolOfMatchClass(n5) then jmp b1 else jmp b4

        b1:
          n6 <- $AttributesOfMatchClass(n5)[0]
          n7 <- $Compare.eq(n6, 0, n0)
          if n7 then jmp b2 else jmp b4

        b10:
          jmp b11(n16)

        b11(n19):
          n20 <- GLOBAL[Point]
          n21 <- $MatchClass(n3, n20, 0, $BuildTuple())
          if $BoolOfMatchClass(n21) then jmp b12 else jmp b13

        b12:
          n24 <- GLOBAL[print]
          n25 <- $Call(n24, "The point is located somewhere else on the plane.", n0)
          return n0

        b13:
          n22 <- GLOBAL[print]
          n23 <- $Call(n22, "Not a point", n0)
          return n0

        b2:
          n8 <- $AttributesOfMatchClass(n5)[1]
          n9 <- $Compare.eq(n8, 0, n0)
          if n9 then jmp b3 else jmp b4

        b3:
          n37 <- GLOBAL[print]
          n38 <- $Call(n37, "Origin is the point's location.", n0)
          return n0

        b4:
          n10 <- GLOBAL[Point]
          n11 <- $MatchClass(n3, n10, 0, $BuildTuple("x", "y"))
          if $BoolOfMatchClass(n11) then jmp b5 else jmp b7

        b5:
          n12 <- $AttributesOfMatchClass(n11)[0]
          n13 <- $Compare.eq(n12, 0, n0)
          if n13 then jmp b6 else jmp b7

        b6:
          n31 <- $AttributesOfMatchClass(n11)[1]
          LOCAL[y] <- n31
          n32 <- GLOBAL[print]
          n33 <- LOCAL[y]
          n34 <- $Format(n33, n0, n0)
          n35 <- $BuildString("Y=", n34, " and the point is on the y-axis.")
          n36 <- $Call(n32, n35, n0)
          return n0

        b7:
          n14 <- GLOBAL[Point]
          n15 <- $MatchClass(n3, n14, 0, $BuildTuple("x", "y"))
          if $BoolOfMatchClass(n15) then jmp b8 else jmp b11($AttributesOfMatchClass(n15))

        b8:
          n16 <- $AttributesOfMatchClass(n15)[0]
          n17 <- $AttributesOfMatchClass(n15)[1]
          n18 <- $Compare.eq(n17, 0, n0)
          if n18 then jmp b9 else jmp b10

        b9:
          LOCAL[x] <- n16
          n26 <- GLOBAL[print]
          n27 <- LOCAL[x]
          n28 <- $Format(n27, n0, n0)
          n29 <- $BuildString("X=", n28, " and the point is on the x-axis.")
          n30 <- $Call(n26, n29, n0)
          return n0 |}]


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
          n0 <- None
          n3 <- $MakeFunction["dummy.Point", n0, n0, n0, n0]
          n4 <- $BuildClass(n3, "Point", n0)
          TOPLEVEL[Point] <- n4
          n5 <- $MakeFunction["dummy.location", n0, n0, n0, n0]
          TOPLEVEL[location] <- n5
          return n0


      function dummy.Point():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "Point"
          $SETUP_ANNOTATIONS
          n4 <- TOPLEVEL[int]
          n5 <- TOPLEVEL[__annotations__]
          n5["x"] <- n4
          n6 <- TOPLEVEL[int]
          n7 <- TOPLEVEL[__annotations__]
          n7["y"] <- n6
          TOPLEVEL[__match_args__] <- $BuildTuple("x", "y")
          return n0


      function dummy.location(point):
        b0:
          n0 <- None
          n3 <- LOCAL[point]
          n4 <- GLOBAL[Point]
          n5 <- $MatchClass(n3, n4, 2, $BuildTuple())
          if $BoolOfMatchClass(n5) then jmp b1 else jmp b4

        b1:
          n6 <- $AttributesOfMatchClass(n5)[0]
          n7 <- $Compare.eq(n6, 0, n0)
          if n7 then jmp b2 else jmp b4

        b10:
          jmp b11(n16)

        b11(n19):
          n20 <- GLOBAL[Point]
          n21 <- $MatchClass(n3, n20, 0, $BuildTuple())
          if $BoolOfMatchClass(n21) then jmp b12 else jmp b13

        b12:
          n24 <- GLOBAL[print]
          n25 <- $Call(n24, "The point is located somewhere else on the plane.", n0)
          return n0

        b13:
          n22 <- GLOBAL[print]
          n23 <- $Call(n22, "Not a point", n0)
          return n0

        b2:
          n8 <- $AttributesOfMatchClass(n5)[1]
          n9 <- $Compare.eq(n8, 0, n0)
          if n9 then jmp b3 else jmp b4

        b3:
          n37 <- GLOBAL[print]
          n38 <- $Call(n37, "Origin is the point's location.", n0)
          return n0

        b4:
          n10 <- GLOBAL[Point]
          n11 <- $MatchClass(n3, n10, 2, $BuildTuple())
          if $BoolOfMatchClass(n11) then jmp b5 else jmp b7

        b5:
          n12 <- $AttributesOfMatchClass(n11)[0]
          n13 <- $Compare.eq(n12, 0, n0)
          if n13 then jmp b6 else jmp b7

        b6:
          n31 <- $AttributesOfMatchClass(n11)[1]
          LOCAL[y] <- n31
          n32 <- GLOBAL[print]
          n33 <- LOCAL[y]
          n34 <- $Format(n33, n0, n0)
          n35 <- $BuildString("Y=", n34, " and the point is on the y-axis.")
          n36 <- $Call(n32, n35, n0)
          return n0

        b7:
          n14 <- GLOBAL[Point]
          n15 <- $MatchClass(n3, n14, 2, $BuildTuple())
          if $BoolOfMatchClass(n15) then jmp b8 else jmp b11($AttributesOfMatchClass(n15))

        b8:
          n16 <- $AttributesOfMatchClass(n15)[0]
          n17 <- $AttributesOfMatchClass(n15)[1]
          n18 <- $Compare.eq(n17, 0, n0)
          if n18 then jmp b9 else jmp b10

        b9:
          LOCAL[x] <- n16
          n26 <- GLOBAL[print]
          n27 <- LOCAL[x]
          n28 <- $Format(n27, n0, n0)
          n29 <- $BuildString("X=", n28, " and the point is on the x-axis.")
          n30 <- $Call(n26, n29, n0)
          return n0 |}]


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
          n0 <- None
          n3 <- $MakeFunction["dummy.Point", n0, n0, n0, n0]
          n4 <- $BuildClass(n3, "Point", n0)
          TOPLEVEL[Point] <- n4
          n5 <- $MakeFunction["dummy.location", n0, n0, n0, n0]
          TOPLEVEL[location] <- n5
          return n0


      function dummy.Point():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "Point"
          $SETUP_ANNOTATIONS
          n4 <- TOPLEVEL[int]
          n5 <- TOPLEVEL[__annotations__]
          n5["x"] <- n4
          n6 <- TOPLEVEL[int]
          n7 <- TOPLEVEL[__annotations__]
          n7["y"] <- n6
          TOPLEVEL[__match_args__] <- $BuildTuple("x", "y")
          return n0


      function dummy.location(point):
        b0:
          n0 <- None
          n3 <- GLOBAL[points]
          if $MatchSequence(n3) then jmp b1 else jmp b8(n3)

        b1:
          n4 <- $Compare.eq($GetLen(n3), 2, n0)
          if n4 then jmp b2 else jmp b8(n3)

        b2:
          n5 <- GLOBAL[Point]
          n6 <- $MatchClass(n3[0], n5, 2, $BuildTuple())
          if $BoolOfMatchClass(n6) then jmp b3 else jmp b7(n3[1], $AttributesOfMatchClass(n6))

        b3:
          n7 <- $AttributesOfMatchClass(n6)[0]
          n8 <- $Compare.eq(n7, 0, n0)
          if n8 then jmp b4 else jmp b7(n3[1], $AttributesOfMatchClass(n6))

        b4:
          n9 <- $AttributesOfMatchClass(n6)[1]
          n10 <- GLOBAL[Point]
          n11 <- $MatchClass(n3[1], n10, 2, $BuildTuple())
          if $BoolOfMatchClass(n11) then jmp b5 else jmp b7(n9, $AttributesOfMatchClass(n11))

        b5:
          n12 <- $AttributesOfMatchClass(n11)[0]
          n13 <- $Compare.eq(n12, 0, n0)
          if n13 then jmp b6 else jmp b7(n9, $AttributesOfMatchClass(n11))

        b6:
          n19 <- $AttributesOfMatchClass(n11)[1]
          LOCAL[y1] <- n9
          LOCAL[y2] <- n19
          n20 <- GLOBAL[print]
          n21 <- LOCAL[y1]
          n22 <- $Format(n21, n0, n0)
          n23 <- LOCAL[y2]
          n24 <- $Format(n23, n0, n0)
          n25 <- $BuildString("Two points on the Y axis at ", n22, ", ", n24, " are in the list.")
          n26 <- $Call(n20, n25, n0)
          return n0

        b7(n14, n15):
          jmp b8(n14)

        b8(n16):
          n17 <- GLOBAL[print]
          n18 <- $Call(n17, "Something else is found in the list.", n0)
          return n0 |}]


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
          n0 <- None
          n3 <- $MakeFunction["dummy.main", n0, n0, n0, n0]
          TOPLEVEL[main] <- n3
          return n0


      function dummy.main(a, b, c):
        b0:
          n0 <- None
          n3 <- GLOBAL[o]
          if $MatchSequence(n3) then jmp b1 else jmp b9(n3)

        b1:
          n4 <- $Compare.eq($GetLen(n3), 1, n0)
          if n4 then jmp b2 else jmp b9(n3)

        b2:
          n5 <- GLOBAL[ast]
          n6 <- n5.Cons1
          n7 <- $MatchClass(n3[0], n6, 2, $BuildTuple())
          if $BoolOfMatchClass(n7) then jmp b3 else jmp b9($AttributesOfMatchClass(n7))

        b3:
          n8 <- $AttributesOfMatchClass(n7)[1]
          n9 <- GLOBAL[ast]
          n10 <- n9.Const2
          n11 <- $MatchClass(n8, n10, 1, $BuildTuple())
          if $BoolOfMatchClass(n11) then jmp b4 else jmp b8

        b4:
          n12 <- $AttributesOfMatchClass(n11)[0]
          if $MatchSequence(n12) then jmp b5 else jmp b7

        b5:
          n13 <- $Compare.eq($GetLen(n12), 3, n0)
          if n13 then jmp b6 else jmp b7

        b6:
          LOCAL[a] <- n12[0]
          LOCAL[b] <- n12[1]
          LOCAL[c] <- n12[2]
          n15 <- GLOBAL[action]
          n16 <- LOCAL[a]
          n17 <- LOCAL[b]
          n18 <- LOCAL[c]
          n19 <- $Call(n15, n16, n17, n18, n0)
          return n0

        b7:
          jmp b8

        b8:
          jmp b9($AttributesOfMatchClass(n7))

        b9(n14):
          return n0 |}]
