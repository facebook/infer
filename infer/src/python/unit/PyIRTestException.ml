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
          n0 <- print(PyIR.Name)(PYCString ("TRY BLOCK"))
          jmp b1

        b1:
          n1 <- print(PyIR.Name)(PYCString ("FINALLY BLOCK"))
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
          n0 <- print(PyIR.Name)(PYCString ("TRY BLOCK"))
          jmp b1

        b1:
          if foo(PyIR.Name) then jmp b2 else jmp b3

        b2:
          n1 <- print(PyIR.Name)(PYCString ("X"))
          jmp b4

        b3:
          n2 <- print(PyIR.Name)(PYCString ("Y"))
          jmp b4

        b4:
          n3 <- print(PyIR.Name)(PYCString ("FINALLY BLOCK"))
          n4 <- print(PyIR.Name)(PYCString ("END"))
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
          n0 <- print(PyIR.Name)(PYCString ("TRY BLOCK"))
          jmp b2

        b1:
          n7 <- print(PyIR.Name)(PYCString ("EXCEPT BLOCK"))
          jmp b3

        b2:
          n8 <- print(PyIR.Name)(PYCString ("END"))
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
          os(PyIR.Name) <- n0
          n1 <- $CallMethod($LoadMethod(os(PyIR.Name), sysconf), PYCString ("SC_PAGESIZE"))
          page_size(PyIR.Name) <- n1
          jmp b2

        b1:
          n8 <- $Compare.exception(n7, (ValueError(PyIR.Name), AttributeError(PyIR.Name)))
          if n8 then jmp b3(n7, n6, n5, n4, n3, n2) else jmp b4(n7, n6, n5, n4, n3, n2)

        b10:
          jmp b2

        b2:
          return PYCNone

        b3:
          page_size(PyIR.Name) <- PYCInt (0)
          jmp b6(n11, n10, n9)

        b4:
          jmp b2

        b5:
          n33 <- $Compare.exception(n32, (ValueError(PyIR.Name), AttributeError(PyIR.Name)))
          if n33 then jmp b7(n32, n31, n30, n29, n28, n27, n23, n22, n21) else
          jmp b8(n32, n31, n30, n29, n28, n27, n23, n22, n21)

        b6:
          jmp b10

        b7:
          page_size(PyIR.Name) <- PYCInt (4096)
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
          foo(PyIR.Name) <- n0
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.f:
        b0:
          n0 <- $GetIter(x(PyIR.Fast))
          jmp b1(n0)

        b1:
          n2 <- $NextIter(n1)
          n3 <- $HasNextIter(n2)
          if n3 then jmp b2 else jmp b3

        b2:
          n4 <- $IterData(n2)
          i(PyIR.Fast) <- n4
          n5 <- $CallMethod($LoadMethod(foo(PyIR.Global), Foo), )
          e(PyIR.Fast) <- n5
          n7 <- print(PyIR.Global)(PYCString ("yolo"))
          jmp b4(n1)

        b3:
          return PYCNone

        b4:
          n9 <- $CallMethod($LoadMethod(e(PyIR.Fast), bar), )
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
          ERROR(PyIR.Name) <- n1
          n2 <- open(PyIR.Name)(PYCString ("foo"), PYCString ("r"))
          n3 <- $LoadMethod(n2, __enter__)()
          fp(PyIR.Name) <- n3
          n5 <- $GetIter(fp(PyIR.Name))
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
          line(PyIR.Name) <- n12
          n15 <- print(PyIR.Name)(PYCString ("TRY"))
          jmp b6(n7, n10)

        b4:
          jmp b1(n11)

        b5:
          n24 <- $Compare.exception(n23, ERROR(PyIR.Name))
          if n24 then jmp b7(n23, n22, n21, n20, n19, n18, n14, n13) else
          jmp b8(n23, n22, n21, n20, n19, n18, n14, n13)

        b6:
          n46 <- print(PyIR.Name)(PYCString ("ELSE"))
          jmp b2(n17, n16)

        b7:
          n41 <- print(PyIR.Name)(PYCString ("EXCEPT"))
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
          TICKS(PyIR.Global) <- PYCInt (0)
          subhelper(PyIR.Name) <- $FuncObj(subhelper, dummy.subhelper, {})
          return PYCNone


      dummy.subhelper:
        b0:
          n0 <- $Inplace.Add(TICKS(PyIR.Global), PYCInt (2))
          TICKS(PyIR.Global) <- n0
          n1 <- range(PyIR.Global)(PYCInt (2))
          n2 <- $GetIter(n1)
          jmp b1(n2)

        b1:
          n4 <- $NextIter(n3)
          n5 <- $HasNextIter(n4)
          if n5 then jmp b2 else jmp b3

        b2:
          n6 <- $IterData(n4)
          i(PyIR.Fast) <- n6
          n8 <- print(PyIR.Global)(PYCString ("foo"))
          jmp b1(n3)

        b3:
          return PYCNone

        b4:
          n16 <- $Compare.exception(n15, AttributeError(PyIR.Global))
          if n16 then jmp b5(n15, n14, n13, n12, n11, n10, n7) else jmp b6(
                                                                    n15, n14, n13, n12, n11, n10, n7)

        b5:
          n31 <- $Inplace.Add(TICKS(PyIR.Global), PYCInt (3))
          TICKS(PyIR.Global) <- n31
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
          foo(PyIR.Name) <- $FuncObj(foo, dummy.foo, {})
          n0 <- foo(PyIR.Name)()
          jmp b2

        b1:
          n7 <- $Compare.exception(n6, C(PyIR.Name))
          if n7 then jmp b3(n6, n5, n4, n3, n2, n1) else jmp b4(n6, n5, n4, n3, n2, n1)

        b2:
          return PYCNone

        b3:
          c(PyIR.Name) <- n12
          n23 <- print(PyIR.Name)(c(PyIR.Name))
          jmp b5(n10, n9, n8)

        b4:
          jmp b2

        b5:
          c(PyIR.Name) <- PYCNone
          n27 <- $DeletePyIR.Name(c)()
          jmp b6

        b6:
          jmp b2


      dummy.foo:
        b0:
          return PYCNone |}]
