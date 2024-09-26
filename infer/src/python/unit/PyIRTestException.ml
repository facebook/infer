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
          n0 <- TOPLEVEL[print](PYCString ("TRY BLOCK"))
          jmp b1

        b1:
          n1 <- TOPLEVEL[print](PYCString ("FINALLY BLOCK"))
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
          n0 <- TOPLEVEL[print](PYCString ("TRY BLOCK"))
          jmp b1

        b1:
          if TOPLEVEL[foo] then jmp b2 else jmp b3

        b2:
          n1 <- TOPLEVEL[print](PYCString ("X"))
          jmp b4

        b3:
          n2 <- TOPLEVEL[print](PYCString ("Y"))
          jmp b4

        b4:
          n3 <- TOPLEVEL[print](PYCString ("FINALLY BLOCK"))
          n4 <- TOPLEVEL[print](PYCString ("END"))
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
          n0 <- TOPLEVEL[print](PYCString ("TRY BLOCK"))
          jmp b2

        b1:
          n7 <- TOPLEVEL[print](PYCString ("EXCEPT BLOCK"))
          jmp b3

        b2:
          n8 <- TOPLEVEL[print](PYCString ("END"))
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
          n1 <- $CallMethod($LoadMethod(TOPLEVEL[os], sysconf), PYCString ("SC_PAGESIZE"))
          TOPLEVEL[page_size] <- n1
          jmp b2

        b1:
          n8 <- $Compare.exception(n7, (TOPLEVEL[ValueError], TOPLEVEL[AttributeError]))
          if n8 then jmp b3(n7, n6, n5, n4, n3, n2) else jmp b4(n7, n6, n5, n4, n3, n2)

        b10:
          jmp b2

        b2:
          return PYCNone

        b3:
          TOPLEVEL[page_size] <- PYCInt (0)
          jmp b6(n11, n10, n9)

        b4:
          jmp b2

        b5:
          n33 <- $Compare.exception(n32, (TOPLEVEL[ValueError], TOPLEVEL[AttributeError]))
          if n33 then jmp b7(n32, n31, n30, n29, n28, n27, n23, n22, n21) else
          jmp b8(n32, n31, n30, n29, n28, n27, n23, n22, n21)

        b6:
          jmp b10

        b7:
          TOPLEVEL[page_size] <- PYCInt (4096)
          jmp b9(n36, n35, n34)

        b8:
          jmp b6(n45, n44, n43)

        b9:
          jmp b6(n54, n53, n52) |}]


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
          n0 <- $GetIter(LOCAL[x])
          jmp b1(n0)

        b1:
          n2 <- $NextIter(n1)
          n3 <- $HasNextIter(n2)
          if n3 then jmp b2 else jmp b3

        b2:
          n4 <- $IterData(n2)
          LOCAL[i] <- n4
          n5 <- $CallMethod($LoadMethod(GLOBAL[foo], Foo), )
          LOCAL[e] <- n5
          n7 <- GLOBAL[print](PYCString ("yolo"))
          jmp b4(n1)

        b3:
          return PYCNone

        b4:
          n9 <- $CallMethod($LoadMethod(LOCAL[e], bar), )
          jmp b1(n6) |}]


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
          n2 <- TOPLEVEL[open](PYCString ("foo"), PYCString ("r"))
          n3 <- $LoadMethod(n2, __enter__)()
          TOPLEVEL[fp] <- n3
          n5 <- $GetIter(TOPLEVEL[fp])
          jmp b2(n5, CM(n2).__exit__)

        b1:
          n50 <- n4(PYCNone, PYCNone, PYCNone)
          return PYCNone

        b2:
          n8 <- $NextIter(n7)
          n9 <- $HasNextIter(n8)
          if n9 then jmp b3(n6) else jmp b4(n6)

        b3:
          n12 <- $IterData(n8)
          TOPLEVEL[line] <- n12
          n15 <- TOPLEVEL[print](PYCString ("TRY"))
          jmp b6(n7, n10)

        b4:
          jmp b1(n11)

        b5:
          n24 <- $Compare.exception(n23, TOPLEVEL[ERROR])
          if n24 then jmp b7(n23, n22, n21, n20, n19, n18, n14, n13) else
          jmp b8(n23, n22, n21, n20, n19, n18, n14, n13)

        b6:
          n46 <- TOPLEVEL[print](PYCString ("ELSE"))
          jmp b2(n17, n16)

        b7:
          n41 <- TOPLEVEL[print](PYCString ("EXCEPT"))
          jmp b9(n26, n25)

        b8:
          jmp b6(n34, n33)

        b9:
          jmp b2(n43, n42) |}]


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
          n0 <- $Inplace.Add(GLOBAL[TICKS], PYCInt (2))
          GLOBAL[TICKS] <- n0
          n1 <- GLOBAL[range](PYCInt (2))
          n2 <- $GetIter(n1)
          jmp b1(n2)

        b1:
          n4 <- $NextIter(n3)
          n5 <- $HasNextIter(n4)
          if n5 then jmp b2 else jmp b3

        b2:
          n6 <- $IterData(n4)
          LOCAL[i] <- n6
          n8 <- GLOBAL[print](PYCString ("foo"))
          jmp b1(n3)

        b3:
          return PYCNone

        b4:
          n16 <- $Compare.exception(n15, GLOBAL[AttributeError])
          if n16 then jmp b5(n15, n14, n13, n12, n11, n10, n7) else jmp b6(
                                                                    n15, n14, n13, n12, n11, n10, n7)

        b5:
          n31 <- $Inplace.Add(GLOBAL[TICKS], PYCInt (3))
          GLOBAL[TICKS] <- n31
          jmp b7(n17)

        b6:
          jmp b1(n24)

        b7:
          jmp b1(n32) |}]


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
          n0 <- TOPLEVEL[foo]()
          jmp b2

        b1:
          n7 <- $Compare.exception(n6, TOPLEVEL[C])
          if n7 then jmp b3(n6, n5, n4, n3, n2, n1) else jmp b4(n6, n5, n4, n3, n2, n1)

        b2:
          return PYCNone

        b3:
          TOPLEVEL[c] <- n12
          n23 <- TOPLEVEL[print](TOPLEVEL[c])
          jmp b5(n10, n9, n8)

        b4:
          jmp b2

        b5:
          TOPLEVEL[c] <- PYCNone
          n27 <- $Delete(TOPLEVEL[c])()
          jmp b6

        b6:
          jmp b2


      dummy.foo:
        b0:
          return PYCNone |}]
