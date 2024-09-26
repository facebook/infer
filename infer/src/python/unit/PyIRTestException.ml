(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

(* Tests with exception handling *)

let%expect_test _ =
  let source = {|
try:
      print("TRY BLOCK")
finally:
      print("FINALLY BLOCK")
      |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- TOPLEVEL[print]
          n1 <- n0(PYCString ("TRY BLOCK"))
          jmp b1

        b1:
          n2 <- TOPLEVEL[print]
          n3 <- n2(PYCString ("FINALLY BLOCK"))
          return PYCNone |}]


let%expect_test _ =
  let source =
    {|
try:
      print("TRY BLOCK")
finally:
      if foo:
          print("X")
      else:
          print("Y")
      print("FINALLY BLOCK")
print("END")
          |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- TOPLEVEL[print]
          n1 <- n0(PYCString ("TRY BLOCK"))
          jmp b1

        b1:
          n2 <- TOPLEVEL[foo]
          if n2 then jmp b2 else jmp b3

        b2:
          n3 <- TOPLEVEL[print]
          n4 <- n3(PYCString ("X"))
          jmp b4

        b3:
          n5 <- TOPLEVEL[print]
          n6 <- n5(PYCString ("Y"))
          jmp b4

        b4:
          n7 <- TOPLEVEL[print]
          n8 <- n7(PYCString ("FINALLY BLOCK"))
          n9 <- TOPLEVEL[print]
          n10 <- n9(PYCString ("END"))
          return PYCNone |}]


let%expect_test _ =
  let source =
    {|
try:
  print("TRY BLOCK")
except:
  print("EXCEPT BLOCK")
print("END")
          |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- TOPLEVEL[print]
          n1 <- n0(PYCString ("TRY BLOCK"))
          jmp b2

        b1:
          n8 <- TOPLEVEL[print]
          n9 <- n8(PYCString ("EXCEPT BLOCK"))
          jmp b3

        b2:
          n10 <- TOPLEVEL[print]
          n11 <- n10(PYCString ("END"))
          return PYCNone

        b3:
          jmp b2 |}]


let%expect_test _ =
  let source =
    {|
import os


try:
    page_size = os.sysconf('SC_PAGESIZE')
except (ValueError, AttributeError):
    try:
        page_size = 0
    except (ValueError, AttributeError):
        page_size = 4096
                 |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- $ImportName(os)(PYCNone, PYCInt (0))
          TOPLEVEL[os] <- n0
          n1 <- TOPLEVEL[os]
          n2 <- $CallMethod($LoadMethod(n1, sysconf), PYCString ("SC_PAGESIZE"))
          TOPLEVEL[page_size] <- n2
          jmp b2

        b1:
          n9 <- TOPLEVEL[ValueError]
          n10 <- TOPLEVEL[AttributeError]
          n11 <- $Compare.exception(n8, (n9, n10))
          if n11 then jmp b3(n8, n7, n6, n5, n4, n3) else jmp b4(n8, n7, n6, n5, n4, n3)

        b10:
          jmp b2

        b2:
          return PYCNone

        b3:
          TOPLEVEL[page_size] <- PYCInt (0)
          jmp b6(n14, n13, n12)

        b4:
          jmp b2

        b5:
          n36 <- TOPLEVEL[ValueError]
          n37 <- TOPLEVEL[AttributeError]
          n38 <- $Compare.exception(n35, (n36, n37))
          if n38 then jmp b7(n35, n34, n33, n32, n31, n30, n26, n25, n24) else
          jmp b8(n35, n34, n33, n32, n31, n30, n26, n25, n24)

        b6:
          jmp b10

        b7:
          TOPLEVEL[page_size] <- PYCInt (4096)
          jmp b9(n41, n40, n39)

        b8:
          jmp b6(n50, n49, n48)

        b9:
          jmp b6(n59, n58, n57) |}]


let%expect_test _ =
  let source =
    {|
import foo

def f(x):
    for i in x:
        e = foo.Foo()
        try:
            print("yolo")
        finally:
            e.bar()
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- $ImportName(foo)(PYCNone, PYCInt (0))
          TOPLEVEL[foo] <- n0
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.f:
        b0:
          n0 <- LOCAL[x]
          n1 <- $GetIter(n0)
          jmp b1(n1)

        b1:
          n3 <- $NextIter(n2)
          n4 <- $HasNextIter(n3)
          if n4 then jmp b2 else jmp b3

        b2:
          n5 <- $IterData(n3)
          LOCAL[i] <- n5
          n6 <- GLOBAL[foo]
          n7 <- $CallMethod($LoadMethod(n6, Foo), )
          LOCAL[e] <- n7
          n9 <- GLOBAL[print]
          n10 <- n9(PYCString ("yolo"))
          jmp b4(n2)

        b3:
          return PYCNone

        b4:
          n12 <- LOCAL[e]
          n13 <- $CallMethod($LoadMethod(n12, bar), )
          jmp b1(n8) |}]


let%expect_test _ =
  let source =
    {|
from foo import ERROR

with open("foo", "r") as fp:
    for line in fp:
        try:
            print("TRY")
        except ERROR:
            print("EXCEPT")
        else:
            print("ELSE")
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- $ImportName(foo)(PYCTuple ([|PYCString ("ERROR")|]), PYCInt (0))
          n1 <- $ImportFrom(ERROR)(n0)
          TOPLEVEL[ERROR] <- n1
          n2 <- TOPLEVEL[open]
          n3 <- n2(PYCString ("foo"), PYCString ("r"))
          n4 <- $LoadMethod(n3, __enter__)()
          TOPLEVEL[fp] <- n4
          n6 <- TOPLEVEL[fp]
          n7 <- $GetIter(n6)
          jmp b2(n7, CM(n3).__exit__)

        b1:
          n56 <- n5(PYCNone, PYCNone, PYCNone)
          return PYCNone

        b2:
          n10 <- $NextIter(n9)
          n11 <- $HasNextIter(n10)
          if n11 then jmp b3(n8) else jmp b4(n8)

        b3:
          n14 <- $IterData(n10)
          TOPLEVEL[line] <- n14
          n17 <- TOPLEVEL[print]
          n18 <- n17(PYCString ("TRY"))
          jmp b6(n9, n12)

        b4:
          jmp b1(n13)

        b5:
          n27 <- TOPLEVEL[ERROR]
          n28 <- $Compare.exception(n26, n27)
          if n28 then jmp b7(n26, n25, n24, n23, n22, n21, n16, n15) else
          jmp b8(n26, n25, n24, n23, n22, n21, n16, n15)

        b6:
          n51 <- TOPLEVEL[print]
          n52 <- n51(PYCString ("ELSE"))
          jmp b2(n20, n19)

        b7:
          n45 <- TOPLEVEL[print]
          n46 <- n45(PYCString ("EXCEPT"))
          jmp b9(n30, n29)

        b8:
          jmp b6(n38, n37)

        b9:
          jmp b2(n48, n47) |}]


let%expect_test _ =
  let source =
    {|
TICKS=0

def subhelper():
    global TICKS
    TICKS += 2
    for i in range(2):
        try:
            print("foo")
        except AttributeError:
            TICKS += 3
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          GLOBAL[TICKS] <- PYCInt (0)
          TOPLEVEL[subhelper] <- $FuncObj(subhelper, dummy.subhelper, {})
          return PYCNone


      dummy.subhelper:
        b0:
          n0 <- GLOBAL[TICKS]
          n1 <- $Inplace.Add(n0, PYCInt (2))
          GLOBAL[TICKS] <- n1
          n2 <- GLOBAL[range]
          n3 <- n2(PYCInt (2))
          n4 <- $GetIter(n3)
          jmp b1(n4)

        b1:
          n6 <- $NextIter(n5)
          n7 <- $HasNextIter(n6)
          if n7 then jmp b2 else jmp b3

        b2:
          n8 <- $IterData(n6)
          LOCAL[i] <- n8
          n10 <- GLOBAL[print]
          n11 <- n10(PYCString ("foo"))
          jmp b1(n5)

        b3:
          return PYCNone

        b4:
          n19 <- GLOBAL[AttributeError]
          n20 <- $Compare.exception(n18, n19)
          if n20 then jmp b5(n18, n17, n16, n15, n14, n13, n9) else jmp b6(
                                                                    n18, n17, n16, n15, n14, n13, n9)

        b5:
          n35 <- GLOBAL[TICKS]
          n36 <- $Inplace.Add(n35, PYCInt (3))
          GLOBAL[TICKS] <- n36
          jmp b7(n21)

        b6:
          jmp b1(n28)

        b7:
          jmp b1(n37) |}]


let%expect_test _ =
  let source =
    {|
def foo():
          pass

try:
          foo()
except C as c:
          print(c)
          |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          TOPLEVEL[foo] <- $FuncObj(foo, dummy.foo, {})
          n0 <- TOPLEVEL[foo]
          n1 <- n0()
          jmp b2

        b1:
          n8 <- TOPLEVEL[C]
          n9 <- $Compare.exception(n7, n8)
          if n9 then jmp b3(n7, n6, n5, n4, n3, n2) else jmp b4(n7, n6, n5, n4, n3, n2)

        b2:
          return PYCNone

        b3:
          TOPLEVEL[c] <- n14
          n25 <- TOPLEVEL[print]
          n26 <- TOPLEVEL[c]
          n27 <- n25(n26)
          jmp b5(n12, n11, n10)

        b4:
          jmp b2

        b5:
          TOPLEVEL[c] <- PYCNone
          n31 <- $Delete(TOPLEVEL[c])()
          jmp b6

        b6:
          jmp b2


      dummy.foo:
        b0:
          return PYCNone |}]
