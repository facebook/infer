(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

(* Tests with the import statement *)

let%expect_test _ =
  let source =
    {|
import base
import base # should only call base.$toplevel once

base.f(0)
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $ImportName(base, n0, 0)
          TOPLEVEL[base] <- n3
          n4 <- $ImportName(base, n0, 0)
          TOPLEVEL[base] <- n4
          n5 <- TOPLEVEL[base]
          n6 <- $CallMethod[f](n5, 0, n0)
          return n0 |}]


let%expect_test _ =
  let source =
    {|

def f():
        pass

f()

from base import f, g

f()
from base import f, g # to PyIR.test that import.toplevel is only called once
g()
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n3
          n4 <- TOPLEVEL[f]
          n5 <- $Call(n4, n0)
          n6 <- $ImportName(base, $BuildTuple("f", "g"), 0)
          n7 <- $ImportFrom(f, n6)
          TOPLEVEL[f] <- n7
          n8 <- $ImportFrom(g, n6)
          TOPLEVEL[g] <- n8
          n9 <- TOPLEVEL[f]
          n10 <- $Call(n9, n0)
          n11 <- $ImportName(base, $BuildTuple("f", "g"), 0)
          n12 <- $ImportFrom(f, n11)
          TOPLEVEL[f] <- n12
          n13 <- $ImportFrom(g, n11)
          TOPLEVEL[g] <- n13
          n14 <- TOPLEVEL[g]
          n15 <- $Call(n14, n0)
          return n0


      function dummy.f():
        b0:
          n0 <- None
          return n0 |}]


let%expect_test _ =
  let source = {|
import unittest

class MyTest(unittest.TestCase):
        pass
        |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $ImportName(unittest, n0, 0)
          TOPLEVEL[unittest] <- n3
          n4 <- $MakeFunction["dummy.MyTest", n0, n0, n0, n0]
          n5 <- TOPLEVEL[unittest]
          n6 <- n5.TestCase
          n7 <- $BuildClass(n4, "MyTest", n6, n0)
          TOPLEVEL[MyTest] <- n7
          return n0


      function dummy.MyTest():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "MyTest"
          return n0 |}]


(* Extracted from Cinder's PyIR.test suite. Currently amended to avoid unsupported opcodes *)
let%expect_test _ =
  let source =
    {|
import os
import sys
from test.libregrtest import main


main_in_temp_cwd = main


def _main():
    global __file__

    mydir = os.path.abspath(os.path.normpath(os.path.dirname(sys.argv[0])))
    i = len(sys.path) - 1
    while i >= 0:
        if os.path.abspath(os.path.normpath(sys.path[i])) == mydir:
            # del sys.path[i] # not supported yet
            pass
        else:
            i -= 1

    __file__ = os.path.abspath(__file__)

    # sanity check
    # assert __file__ == os.path.abspath(sys.argv[0]) # not supported yet

    main()


if __name__ == '__main__':
    _main()
      |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $ImportName(os, n0, 0)
          TOPLEVEL[os] <- n3
          n4 <- $ImportName(sys, n0, 0)
          TOPLEVEL[sys] <- n4
          n5 <- $ImportName(test.libregrtest, $BuildTuple("main"), 0)
          n6 <- $ImportFrom(main, n5)
          TOPLEVEL[main] <- n6
          n7 <- TOPLEVEL[main]
          TOPLEVEL[main_in_temp_cwd] <- n7
          n8 <- $MakeFunction["dummy._main", n0, n0, n0, n0]
          TOPLEVEL[_main] <- n8
          n9 <- TOPLEVEL[__name__]
          n10 <- $Compare.eq(n9, "__main__", n0)
          if n10 then jmp b1 else jmp b2

        b1:
          n11 <- TOPLEVEL[_main]
          n12 <- $Call(n11, n0)
          return n0

        b2:
          return n0


      function dummy._main(mydir, i):
        b0:
          n0 <- None
          n3 <- GLOBAL[os]
          n4 <- n3.path
          n5 <- GLOBAL[os]
          n6 <- n5.path
          n7 <- GLOBAL[os]
          n8 <- n7.path
          n9 <- GLOBAL[sys]
          n10 <- n9.argv
          n11 <- n10[0]
          n12 <- $CallMethod[dirname](n8, n11, n0)
          n13 <- $CallMethod[normpath](n6, n12, n0)
          n14 <- $CallMethod[abspath](n4, n13, n0)
          LOCAL[mydir] <- n14
          n15 <- GLOBAL[len]
          n16 <- GLOBAL[sys]
          n17 <- n16.path
          n18 <- $Call(n15, n17, n0)
          n19 <- $Binary.Subtract(n18, 1, n0)
          LOCAL[i] <- n19
          n20 <- LOCAL[i]
          n21 <- $Compare.ge(n20, 0, n0)
          if n21 then jmp b1 else jmp b5

        b1:
          n22 <- GLOBAL[os]
          n23 <- n22.path
          n24 <- GLOBAL[os]
          n25 <- n24.path
          n26 <- GLOBAL[sys]
          n27 <- n26.path
          n28 <- LOCAL[i]
          n29 <- n27[n28]
          n30 <- $CallMethod[normpath](n25, n29, n0)
          n31 <- $CallMethod[abspath](n23, n30, n0)
          n32 <- LOCAL[mydir]
          n33 <- $Compare.eq(n31, n32, n0)
          if n33 then jmp b2 else jmp b3

        b2:
          jmp b4

        b3:
          n34 <- LOCAL[i]
          n35 <- $Inplace.Subtract(n34, 1, n0)
          LOCAL[i] <- n35
          jmp b4

        b4:
          n36 <- LOCAL[i]
          n37 <- $Compare.ge(n36, 0, n0)
          if n37 then jmp b1 else jmp b5

        b5:
          n38 <- GLOBAL[os]
          n39 <- n38.path
          n40 <- GLOBAL[__file__]
          n41 <- $CallMethod[abspath](n39, n40, n0)
          GLOBAL[__file__] <- n41
          n42 <- GLOBAL[main]
          n43 <- $Call(n42, n0)
          return n0 |}]


let%expect_test _ =
  let source =
    {|
from A import X
X()
from .B import X
X()
from ..C import X
X()

from .. import path
# this will generate a warning, expected until modules are encoded as proper Textual types
path.X()
      |}
  in
  PyIR.test ~filename:"some/long/path/dummy.py" source ;
  [%expect
    {|
    module some::long::path::dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $ImportName(A, $BuildTuple("X"), 0)
          n4 <- $ImportFrom(X, n3)
          TOPLEVEL[X] <- n4
          n5 <- TOPLEVEL[X]
          n6 <- $Call(n5, n0)
          n7 <- $ImportName(B, $BuildTuple("X"), 1)
          n8 <- $ImportFrom(X, n7)
          TOPLEVEL[X] <- n8
          n9 <- TOPLEVEL[X]
          n10 <- $Call(n9, n0)
          n11 <- $ImportName(C, $BuildTuple("X"), 2)
          n12 <- $ImportFrom(X, n11)
          TOPLEVEL[X] <- n12
          n13 <- TOPLEVEL[X]
          n14 <- $Call(n13, n0)
          n15 <- $ImportName(, $BuildTuple("path"), 2)
          n16 <- $ImportFrom(path, n15)
          TOPLEVEL[path] <- n16
          n17 <- TOPLEVEL[path]
          n18 <- $CallMethod[X](n17, n0)
          return n0 |}]


let%expect_test _ =
  let source = {|
pass
|} in
  PyIR.test ~filename:"./dir1/dir2/script.py" source ;
  [%expect
    {|
    module dir1::dir2::script:

      function toplevel():
        b0:
          n0 <- None
          return n0 |}]


let%expect_test _ =
  let source =
    {|
from x import y as z, a as b
from x import y as z, a as b #testing the single load of x's top level

z()
b()

from foo import toto, tata #testing the single load of foo's top level
from foo import toto, tata
toto()
tata()

        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $ImportName(x, $BuildTuple("y", "a"), 0)
          n4 <- $ImportFrom(y, n3)
          TOPLEVEL[z] <- n4
          n5 <- $ImportFrom(a, n3)
          TOPLEVEL[b] <- n5
          n6 <- $ImportName(x, $BuildTuple("y", "a"), 0)
          n7 <- $ImportFrom(y, n6)
          TOPLEVEL[z] <- n7
          n8 <- $ImportFrom(a, n6)
          TOPLEVEL[b] <- n8
          n9 <- TOPLEVEL[z]
          n10 <- $Call(n9, n0)
          n11 <- TOPLEVEL[b]
          n12 <- $Call(n11, n0)
          n13 <- $ImportName(foo, $BuildTuple("toto", "tata"), 0)
          n14 <- $ImportFrom(toto, n13)
          TOPLEVEL[toto] <- n14
          n15 <- $ImportFrom(tata, n13)
          TOPLEVEL[tata] <- n15
          n16 <- $ImportName(foo, $BuildTuple("toto", "tata"), 0)
          n17 <- $ImportFrom(toto, n16)
          TOPLEVEL[toto] <- n17
          n18 <- $ImportFrom(tata, n16)
          TOPLEVEL[tata] <- n18
          n19 <- TOPLEVEL[toto]
          n20 <- $Call(n19, n0)
          n21 <- TOPLEVEL[tata]
          n22 <- $Call(n21, n0)
          return n0 |}]


let%expect_test _ =
  let source = {|
# From Cinder
import xml.etree.ElementTree as ET
          |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $ImportName(xml.etree.ElementTree, n0, 0)
          n4 <- $ImportFrom(etree, n3)
          n5 <- $ImportFrom(ElementTree, n4)
          TOPLEVEL[ET] <- n5
          return n0 |}]


let%expect_test _ =
  let source =
    {|
import unittest
import signal

@unittest.skipUnless(hasattr(signal, "setitimer"), "requires setitimer()")
class Test(unittest.TestCase):
  pass
  |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $ImportName(unittest, n0, 0)
          TOPLEVEL[unittest] <- n3
          n4 <- $ImportName(signal, n0, 0)
          TOPLEVEL[signal] <- n4
          n5 <- TOPLEVEL[unittest]
          n6 <- TOPLEVEL[hasattr]
          n7 <- TOPLEVEL[signal]
          n8 <- $Call(n6, n7, "setitimer", n0)
          n9 <- $CallMethod[skipUnless](n5, n8, "requires setitimer()", n0)
          n10 <- $MakeFunction["dummy.Test", n0, n0, n0, n0]
          n11 <- TOPLEVEL[unittest]
          n12 <- n11.TestCase
          n13 <- $BuildClass(n10, "Test", n12, n0)
          n14 <- $Call(n9, n13, n0)
          TOPLEVEL[Test] <- n14
          return n0


      function dummy.Test():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "Test"
          return n0 |}]


let%expect_test _ =
  let source = {|
import foo

def f():
          raise foo.bar(42)
          |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $ImportName(foo, n0, 0)
          TOPLEVEL[foo] <- n3
          n4 <- $MakeFunction["dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n4
          return n0


      function dummy.f():
        b0:
          n0 <- None
          n3 <- GLOBAL[foo]
          n4 <- $CallMethod[bar](n3, 42, n0)
          throw n4 |}]


let%expect_test _ =
  let source =
    {|
import foo

def f(ok):
    try:
          foo.bar()
    except OverflowError:
        if not ok:
            raise
          |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $ImportName(foo, n0, 0)
          TOPLEVEL[foo] <- n3
          n4 <- $MakeFunction["dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n4
          return n0


      function dummy.f(ok):
        b0:
          n0 <- None
          n3 <- GLOBAL[foo]
          n4 <- $CallMethod[bar](n3, n0)
          return n0 |}]


let%expect_test _ =
  let source =
    {|
# from Cinder
import decimal

def assertEqual(x):
  pass

def test_format_specifier_expressions(self):
  width = 10
  precision = 4
  value = decimal.Decimal('12.34567')
  assertEqual(f'result: {value:{width}.{precision}}')
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $ImportName(decimal, n0, 0)
          TOPLEVEL[decimal] <- n3
          n4 <- $MakeFunction["dummy.assertEqual", n0, n0, n0, n0]
          TOPLEVEL[assertEqual] <- n4
          n5 <- $MakeFunction["dummy.test_format_specifier_expressions", n0, n0, n0, n0]
          TOPLEVEL[test_format_specifier_expressions] <- n5
          return n0


      function dummy.assertEqual(x):
        b0:
          n0 <- None
          return n0


      function dummy.test_format_specifier_expressions(self):
        b0:
          n0 <- None
          LOCAL[width] <- 10
          LOCAL[precision] <- 4
          n3 <- GLOBAL[decimal]
          n4 <- $CallMethod[Decimal](n3, "12.34567", n0)
          LOCAL[value] <- n4
          n5 <- GLOBAL[assertEqual]
          n6 <- LOCAL[value]
          n7 <- LOCAL[width]
          n8 <- $Format(n7, n0, n0)
          n9 <- LOCAL[precision]
          n10 <- $Format(n9, n0, n0)
          n11 <- $BuildString(n8, ".", n10)
          n12 <- $Format(n6, n11, n0)
          n13 <- $BuildString("result: ", n12)
          n14 <- $Call(n5, n13, n0)
          return n0 |}]


let%expect_test _ =
  let source =
    {|
x : int
x = 0

y : str = "zuck"


import C
z : C.T = 42

def f():
    # python bytecode doesn't keep the annotations for local variables
    u: int
    u = 0

    v: str = "tata"
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          $SETUP_ANNOTATIONS
          n3 <- TOPLEVEL[int]
          n4 <- TOPLEVEL[__annotations__]
          n4["x"] <- n3
          TOPLEVEL[x] <- 0
          TOPLEVEL[y] <- "zuck"
          n5 <- TOPLEVEL[str]
          n6 <- TOPLEVEL[__annotations__]
          n6["y"] <- n5
          n7 <- $ImportName(C, n0, 0)
          TOPLEVEL[C] <- n7
          TOPLEVEL[z] <- 42
          n8 <- TOPLEVEL[C]
          n9 <- n8.T
          n10 <- TOPLEVEL[__annotations__]
          n10["z"] <- n9
          n11 <- $MakeFunction["dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n11
          return n0


      function dummy.f(u, v):
        b0:
          n0 <- None
          LOCAL[u] <- 0
          LOCAL[v] <- "tata"
          return n0 |}]


let%expect_test _ =
  let source = {|
from foo import *
|} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $ImportName(foo, $BuildTuple("*"), 0)
          $ImportStart(n3)
          return n0 |}]


let%expect_test _ =
  let source =
    {|
if test():
    from mod import *
try:
    pass
except Exception as error:
    pass
|}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[test]
          n4 <- $Call(n3, n0)
          if n4 then jmp b1 else jmp b2

        b1:
          n5 <- $ImportName(mod, $BuildTuple("*"), 0)
          $ImportStart(n5)
          jmp b2

        b2:
          return n0 |}]
