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
          n5 <- TOPLEVEL[print]
          n6 <- n5(PYCString ("X"))
          jmp b4

        b3:
          n3 <- TOPLEVEL[print]
          n4 <- n3(PYCString ("Y"))
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
          jmp b3

        b3:
          n2 <- TOPLEVEL[print]
          n3 <- n2(PYCString ("END"))
          return PYCNone |}]


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
          n2 <- n1.sysconf(PYCString ("SC_PAGESIZE"))
          TOPLEVEL[page_size] <- n2
          jmp b8

        b8:
          return PYCNone |}]


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

        b1(n2):
          n3 <- $NextIter(n2)
          n4 <- $HasNextIter(n2)
          if n4 then jmp b2 else jmp b4

        b2:
          LOCAL[i] <- n3
          n5 <- GLOBAL[foo]
          n6 <- n5.Foo()
          LOCAL[e] <- n6
          n7 <- GLOBAL[print]
          n8 <- n7(PYCString ("yolo"))
          jmp b3

        b3:
          n9 <- LOCAL[e]
          n10 <- n9.bar()
          jmp b1(n2)

        b4:
          return PYCNone |}]


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
          n4 <- n3.__enter__()
          TOPLEVEL[fp] <- n4
          n5 <- TOPLEVEL[fp]
          n6 <- $GetIter(n5)
          jmp b1(CM(n3).__exit__, n6)

        b1(n7, n8):
          n9 <- $NextIter(n8)
          n10 <- $HasNextIter(n8)
          if n10 then jmp b2 else jmp b7

        b2:
          TOPLEVEL[line] <- n9
          n12 <- TOPLEVEL[print]
          n13 <- n12(PYCString ("TRY"))
          jmp b6

        b6:
          n14 <- TOPLEVEL[print]
          n15 <- n14(PYCString ("ELSE"))
          jmp b1(n7, n8)

        b7:
          jmp b8

        b8:
          n11 <- PYCNone(PYCNone, PYCNone, PYCNone)
          return PYCNone |}]


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

        b1(n5):
          n6 <- $NextIter(n5)
          n7 <- $HasNextIter(n5)
          if n7 then jmp b2 else jmp b6

        b2:
          LOCAL[i] <- n6
          n8 <- GLOBAL[print]
          n9 <- n8(PYCString ("foo"))
          jmp b1(n5)

        b6:
          return PYCNone |}]


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
          jmp b5

        b5:
          return PYCNone


      dummy.foo:
        b0:
          return PYCNone |}]


let%expect_test _ =
  let source =
    {|
async def async_with(filename):
    async with open(filename, 'r') as f:
        await f.read()
|}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          TOPLEVEL[async_with] <- $FuncObj(async_with, dummy.async_with, {})
          return PYCNone


      dummy.async_with:
        b0:
          n0 <- GLOBAL[open]
          n1 <- LOCAL[filename]
          n2 <- n0(n1, PYCString ("r"))
          n3 <- n2.__enter__()
          n4 <- $GetAwaitable(n3)
          n5 <- $YieldFrom(n4, PYCNone)
          LOCAL[f] <- n4
          n6 <- LOCAL[f]
          n7 <- n6.read()
          n8 <- $GetAwaitable(n7)
          n9 <- $YieldFrom(n8, PYCNone)
          jmp b1

        b1:
          n10 <- PYCNone(PYCNone, PYCNone, PYCNone)
          n11 <- $GetAwaitable(n10)
          n12 <- $YieldFrom(n11, PYCNone)
          return PYCNone |}]


let%expect_test _ =
  let source =
    {|
def call_finally():
    try:
        read()
    except Exception as e:
        return
|}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          TOPLEVEL[call_finally] <- $FuncObj(call_finally, dummy.call_finally, {})
          return PYCNone


      dummy.call_finally:
        b0:
          n0 <- GLOBAL[read]
          n1 <- n0()
          jmp b6

        b6:
          return PYCNone |}]


let%expect_test _ =
  let source =
    {|
def call_finally_with_break():
    for i in range(100):
        try:
            read()
        except Exception as e:
            break
|}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          TOPLEVEL[call_finally_with_break] <- $FuncObj(call_finally_with_break, dummy.call_finally_with_break, {})
          return PYCNone


      dummy.call_finally_with_break:
        b0:
          n0 <- GLOBAL[range]
          n1 <- n0(PYCInt (100))
          n2 <- $GetIter(n1)
          jmp b1(n2)

        b1(n3):
          n4 <- $NextIter(n3)
          n5 <- $HasNextIter(n3)
          if n5 then jmp b2 else jmp b9

        b2:
          LOCAL[i] <- n4
          n6 <- GLOBAL[read]
          n7 <- n6()
          jmp b1(n3)

        b9:
          return PYCNone |}]


let%expect_test _ =
  let source = {|
def raise_from(e):
    raise IndexError from e
|} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          TOPLEVEL[raise_from] <- $FuncObj(raise_from, dummy.raise_from, {})
          return PYCNone


      dummy.raise_from:
        b0:
          n0 <- GLOBAL[IndexError]
          n1 <- LOCAL[e]
          n0.__cause__ <- n1
          throw n0 |}]
