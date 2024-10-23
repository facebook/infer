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
          n0 <- $ImportName(base, None, 0)
          TOPLEVEL[base] <- n0
          n1 <- $ImportName(base, None, 0)
          TOPLEVEL[base] <- n1
          n2 <- TOPLEVEL[base]
          n3 <- $CallMethod[f](n2, 0, None)
          return None |}]


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
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          n1 <- TOPLEVEL[f]
          n2 <- $Call(n1, None)
          n3 <- $ImportName(base, $BuildTuple("f", "g"), 0)
          n4 <- $ImportFrom(f, n3)
          TOPLEVEL[f] <- n4
          n5 <- $ImportFrom(g, n3)
          TOPLEVEL[g] <- n5
          n6 <- TOPLEVEL[f]
          n7 <- $Call(n6, None)
          n8 <- $ImportName(base, $BuildTuple("f", "g"), 0)
          n9 <- $ImportFrom(f, n8)
          TOPLEVEL[f] <- n9
          n10 <- $ImportFrom(g, n8)
          TOPLEVEL[g] <- n10
          n11 <- TOPLEVEL[g]
          n12 <- $Call(n11, None)
          return None


      function dummy.f():
        b0:
          return None |}]


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
          n0 <- $ImportName(unittest, None, 0)
          TOPLEVEL[unittest] <- n0
          n1 <- $MakeFunction["MyTest", "dummy.MyTest", None, None, None, None]
          n2 <- TOPLEVEL[unittest]
          n3 <- n2.TestCase
          n4 <- $BuildClass(n1, "MyTest", n3, None)
          TOPLEVEL[MyTest] <- n4
          return None


      function dummy.MyTest():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "MyTest"
          return None |}]


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
          n0 <- $ImportName(os, None, 0)
          TOPLEVEL[os] <- n0
          n1 <- $ImportName(sys, None, 0)
          TOPLEVEL[sys] <- n1
          n2 <- $ImportName(test.libregrtest, $BuildTuple("main"), 0)
          n3 <- $ImportFrom(main, n2)
          TOPLEVEL[main] <- n3
          n4 <- TOPLEVEL[main]
          TOPLEVEL[main_in_temp_cwd] <- n4
          n5 <- $MakeFunction["_main", "dummy._main", None, None, None, None]
          TOPLEVEL[_main] <- n5
          n6 <- TOPLEVEL[__name__]
          n7 <- $Compare.eq(n6, "__main__", None)
          if n7 then jmp b1 else jmp b2

        b1:
          n8 <- TOPLEVEL[_main]
          n9 <- $Call(n8, None)
          jmp b2

        b2:
          return None


      function dummy._main(mydir, i):
        b0:
          n0 <- GLOBAL[os]
          n1 <- n0.path
          n2 <- GLOBAL[os]
          n3 <- n2.path
          n4 <- GLOBAL[os]
          n5 <- n4.path
          n6 <- GLOBAL[sys]
          n7 <- n6.argv
          n8 <- n7[0]
          n9 <- $CallMethod[dirname](n5, n8, None)
          n10 <- $CallMethod[normpath](n3, n9, None)
          n11 <- $CallMethod[abspath](n1, n10, None)
          LOCAL[mydir] <- n11
          n12 <- GLOBAL[len]
          n13 <- GLOBAL[sys]
          n14 <- n13.path
          n15 <- $Call(n12, n14, None)
          n16 <- $Binary.Subtract(n15, 1, None)
          LOCAL[i] <- n16
          jmp b1

        b1:
          n17 <- LOCAL[i]
          n18 <- $Compare.ge(n17, 0, None)
          if n18 then jmp b2 else jmp b5

        b2:
          n25 <- GLOBAL[os]
          n26 <- n25.path
          n27 <- GLOBAL[os]
          n28 <- n27.path
          n29 <- GLOBAL[sys]
          n30 <- n29.path
          n31 <- LOCAL[i]
          n32 <- n30[n31]
          n33 <- $CallMethod[normpath](n28, n32, None)
          n34 <- $CallMethod[abspath](n26, n33, None)
          n35 <- LOCAL[mydir]
          n36 <- $Compare.eq(n34, n35, None)
          if n36 then jmp b3 else jmp b4

        b3:
          jmp b1

        b4:
          n37 <- LOCAL[i]
          n38 <- $Inplace.Subtract(n37, 1, None)
          LOCAL[i] <- n38
          jmp b1

        b5:
          n19 <- GLOBAL[os]
          n20 <- n19.path
          n21 <- GLOBAL[__file__]
          n22 <- $CallMethod[abspath](n20, n21, None)
          GLOBAL[__file__] <- n22
          n23 <- GLOBAL[main]
          n24 <- $Call(n23, None)
          return None |}]


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
          n0 <- $ImportName(A, $BuildTuple("X"), 0)
          n1 <- $ImportFrom(X, n0)
          TOPLEVEL[X] <- n1
          n2 <- TOPLEVEL[X]
          n3 <- $Call(n2, None)
          n4 <- $ImportName(B, $BuildTuple("X"), 1)
          n5 <- $ImportFrom(X, n4)
          TOPLEVEL[X] <- n5
          n6 <- TOPLEVEL[X]
          n7 <- $Call(n6, None)
          n8 <- $ImportName(C, $BuildTuple("X"), 2)
          n9 <- $ImportFrom(X, n8)
          TOPLEVEL[X] <- n9
          n10 <- TOPLEVEL[X]
          n11 <- $Call(n10, None)
          n12 <- $ImportName(, $BuildTuple("path"), 2)
          n13 <- $ImportFrom(path, n12)
          TOPLEVEL[path] <- n13
          n14 <- TOPLEVEL[path]
          n15 <- $CallMethod[X](n14, None)
          return None |}]


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
          return None |}]


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
          n0 <- $ImportName(x, $BuildTuple("y", "a"), 0)
          n1 <- $ImportFrom(y, n0)
          TOPLEVEL[z] <- n1
          n2 <- $ImportFrom(a, n0)
          TOPLEVEL[b] <- n2
          n3 <- $ImportName(x, $BuildTuple("y", "a"), 0)
          n4 <- $ImportFrom(y, n3)
          TOPLEVEL[z] <- n4
          n5 <- $ImportFrom(a, n3)
          TOPLEVEL[b] <- n5
          n6 <- TOPLEVEL[z]
          n7 <- $Call(n6, None)
          n8 <- TOPLEVEL[b]
          n9 <- $Call(n8, None)
          n10 <- $ImportName(foo, $BuildTuple("toto", "tata"), 0)
          n11 <- $ImportFrom(toto, n10)
          TOPLEVEL[toto] <- n11
          n12 <- $ImportFrom(tata, n10)
          TOPLEVEL[tata] <- n12
          n13 <- $ImportName(foo, $BuildTuple("toto", "tata"), 0)
          n14 <- $ImportFrom(toto, n13)
          TOPLEVEL[toto] <- n14
          n15 <- $ImportFrom(tata, n13)
          TOPLEVEL[tata] <- n15
          n16 <- TOPLEVEL[toto]
          n17 <- $Call(n16, None)
          n18 <- TOPLEVEL[tata]
          n19 <- $Call(n18, None)
          return None |}]


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
          n0 <- $ImportName(xml.etree.ElementTree, None, 0)
          n1 <- $ImportFrom(etree, n0)
          n2 <- $ImportFrom(ElementTree, n1)
          TOPLEVEL[ET] <- n2
          return None |}]


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
          n0 <- $ImportName(unittest, None, 0)
          TOPLEVEL[unittest] <- n0
          n1 <- $ImportName(signal, None, 0)
          TOPLEVEL[signal] <- n1
          n2 <- TOPLEVEL[unittest]
          n3 <- TOPLEVEL[hasattr]
          n4 <- TOPLEVEL[signal]
          n5 <- $Call(n3, n4, "setitimer", None)
          n6 <- $CallMethod[skipUnless](n2, n5, "requires setitimer()", None)
          n7 <- $MakeFunction["Test", "dummy.Test", None, None, None, None]
          n8 <- TOPLEVEL[unittest]
          n9 <- n8.TestCase
          n10 <- $BuildClass(n7, "Test", n9, None)
          n11 <- $Call(n6, n10, None)
          TOPLEVEL[Test] <- n11
          return None


      function dummy.Test():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "Test"
          return None |}]


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
          n0 <- $ImportName(foo, None, 0)
          TOPLEVEL[foo] <- n0
          n1 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n1
          return None


      function dummy.f():
        b0:
          n0 <- GLOBAL[foo]
          n1 <- $CallMethod[bar](n0, 42, None)
          throw n1 |}]


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
          n0 <- $ImportName(foo, None, 0)
          TOPLEVEL[foo] <- n0
          n1 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n1
          return None


      function dummy.f(ok):
        b0:
          n0 <- GLOBAL[foo]
          n1 <- $CallMethod[bar](n0, None)
          jmp b6

        b6:
          return None |}]


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
          n0 <- $ImportName(decimal, None, 0)
          TOPLEVEL[decimal] <- n0
          n1 <- $MakeFunction["assertEqual", "dummy.assertEqual", None, None, None, None]
          TOPLEVEL[assertEqual] <- n1
          n2 <- $MakeFunction["test_format_specifier_expressions", "dummy.test_format_specifier_expressions", None, None, None, None]
          TOPLEVEL[test_format_specifier_expressions] <- n2
          return None


      function dummy.assertEqual(x):
        b0:
          return None


      function dummy.test_format_specifier_expressions(self):
        b0:
          LOCAL[width] <- 10
          LOCAL[precision] <- 4
          n0 <- GLOBAL[decimal]
          n1 <- $CallMethod[Decimal](n0, "12.34567", None)
          LOCAL[value] <- n1
          n2 <- GLOBAL[assertEqual]
          n3 <- LOCAL[value]
          n4 <- LOCAL[width]
          n5 <- $Format(n4, None, None)
          n6 <- LOCAL[precision]
          n7 <- $Format(n6, None, None)
          n8 <- $Format(n3, $BuildString(n5, ".", n7), None)
          n9 <- $Call(n2, $BuildString("result: ", n8), None)
          return None |}]


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
          $SETUP_ANNOTATIONS
          n0 <- TOPLEVEL[int]
          n1 <- TOPLEVEL[__annotations__]
          n1["x"] <- n0
          TOPLEVEL[x] <- 0
          TOPLEVEL[y] <- "zuck"
          n2 <- TOPLEVEL[str]
          n3 <- TOPLEVEL[__annotations__]
          n3["y"] <- n2
          n4 <- $ImportName(C, None, 0)
          TOPLEVEL[C] <- n4
          TOPLEVEL[z] <- 42
          n5 <- TOPLEVEL[C]
          n6 <- n5.T
          n7 <- TOPLEVEL[__annotations__]
          n7["z"] <- n6
          n8 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n8
          return None


      function dummy.f(u, v):
        b0:
          LOCAL[u] <- 0
          LOCAL[v] <- "tata"
          return None |}]


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
          n0 <- $ImportName(foo, $BuildTuple("*"), 0)
          n1 <- $ImportStar(n0, None)
          return None |}]
