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

      toplevel:
        b0:
          n0 <- $ImportName(base)(PYCNone, PYCInt (0))
          TOPLEVEL[base] <- n0
          n1 <- $ImportName(base)(PYCNone, PYCInt (0))
          TOPLEVEL[base] <- n1
          n2 <- TOPLEVEL[base]
          n3 <- n2.f(PYCInt (0))
          return PYCNone |}]


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

      toplevel:
        b0:
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          n0 <- TOPLEVEL[f]
          n1 <- n0()
          n2 <- $ImportName(base)(PYCTuple ([|PYCString ("f"); PYCString ("g")|]), PYCInt (0))
          n3 <- $ImportFrom(f)(n2)
          TOPLEVEL[f] <- n3
          n4 <- $ImportFrom(g)(n2)
          TOPLEVEL[g] <- n4
          n5 <- TOPLEVEL[f]
          n6 <- n5()
          n7 <- $ImportName(base)(PYCTuple ([|PYCString ("f"); PYCString ("g")|]), PYCInt (0))
          n8 <- $ImportFrom(f)(n7)
          TOPLEVEL[f] <- n8
          n9 <- $ImportFrom(g)(n7)
          TOPLEVEL[g] <- n9
          n10 <- TOPLEVEL[g]
          n11 <- n10()
          return PYCNone


      dummy.f:
        b0:
          return PYCNone |}]


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

      toplevel:
        b0:
          n0 <- $ImportName(unittest)(PYCNone, PYCInt (0))
          TOPLEVEL[unittest] <- n0
          n1 <- TOPLEVEL[unittest]
          n2 <- n1.TestCase
          n3 <- $BuildClass($FuncObj(MyTest, dummy.MyTest, {}), PYCString ("MyTest"), n2)
          TOPLEVEL[MyTest] <- n3
          return PYCNone


      dummy.MyTest:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- PYCString ("MyTest")
          return PYCNone |}]


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

      toplevel:
        b0:
          n0 <- $ImportName(os)(PYCNone, PYCInt (0))
          TOPLEVEL[os] <- n0
          n1 <- $ImportName(sys)(PYCNone, PYCInt (0))
          TOPLEVEL[sys] <- n1
          n2 <- $ImportName(test.libregrtest)(PYCTuple ([|PYCString ("main")|]), PYCInt (0))
          n3 <- $ImportFrom(main)(n2)
          TOPLEVEL[main] <- n3
          n4 <- TOPLEVEL[main]
          TOPLEVEL[main_in_temp_cwd] <- n4
          TOPLEVEL[_main] <- $FuncObj(_main, dummy._main, {})
          n5 <- TOPLEVEL[__name__]
          n6 <- $Compare.eq(n5, PYCString ("__main__"))
          if n6 then jmp b1 else jmp b2

        b1:
          n7 <- TOPLEVEL[_main]
          n8 <- n7()
          jmp b2

        b2:
          return PYCNone


      dummy._main:
        b0:
          n0 <- GLOBAL[os]
          n1 <- n0.path
          n2 <- GLOBAL[os]
          n3 <- n2.path
          n4 <- GLOBAL[os]
          n5 <- n4.path
          n6 <- GLOBAL[sys]
          n7 <- n6.argv
          n8 <- n7[PYCInt (0)]
          n9 <- n5.dirname(n8)
          n10 <- n3.normpath(n9)
          n11 <- n1.abspath(n10)
          LOCAL[mydir] <- n11
          n12 <- GLOBAL[len]
          n13 <- GLOBAL[sys]
          n14 <- n13.path
          n15 <- n12(n14)
          n16 <- $Binary.Subtract(n15, PYCInt (1))
          LOCAL[i] <- n16
          jmp b1

        b1:
          n17 <- LOCAL[i]
          n18 <- $Compare.ge(n17, PYCInt (0))
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
          n33 <- n28.normpath(n32)
          n34 <- n26.abspath(n33)
          n35 <- LOCAL[mydir]
          n36 <- $Compare.eq(n34, n35)
          if n36 then jmp b3 else jmp b4

        b3:
          jmp b1

        b4:
          n37 <- LOCAL[i]
          n38 <- $Inplace.Subtract(n37, PYCInt (1))
          LOCAL[i] <- n38
          jmp b1

        b5:
          n19 <- GLOBAL[os]
          n20 <- n19.path
          n21 <- GLOBAL[__file__]
          n22 <- n20.abspath(n21)
          GLOBAL[__file__] <- n22
          n23 <- GLOBAL[main]
          n24 <- n23()
          return PYCNone |}]


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
    module some/long/path/dummy:

      toplevel:
        b0:
          n0 <- $ImportName(A)(PYCTuple ([|PYCString ("X")|]), PYCInt (0))
          n1 <- $ImportFrom(X)(n0)
          TOPLEVEL[X] <- n1
          n2 <- TOPLEVEL[X]
          n3 <- n2()
          n4 <- $ImportName(B)(PYCTuple ([|PYCString ("X")|]), PYCInt (1))
          n5 <- $ImportFrom(X)(n4)
          TOPLEVEL[X] <- n5
          n6 <- TOPLEVEL[X]
          n7 <- n6()
          n8 <- $ImportName(C)(PYCTuple ([|PYCString ("X")|]), PYCInt (2))
          n9 <- $ImportFrom(X)(n8)
          TOPLEVEL[X] <- n9
          n10 <- TOPLEVEL[X]
          n11 <- n10()
          n12 <- $ImportName()(PYCTuple ([|PYCString ("path")|]), PYCInt (2))
          n13 <- $ImportFrom(path)(n12)
          TOPLEVEL[path] <- n13
          n14 <- TOPLEVEL[path]
          n15 <- n14.X()
          return PYCNone |}]


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

      toplevel:
        b0:
          n0 <- $ImportName(x)(PYCTuple ([|PYCString ("y"); PYCString ("a")|]), PYCInt (0))
          n1 <- $ImportFrom(y)(n0)
          TOPLEVEL[z] <- n1
          n2 <- $ImportFrom(a)(n0)
          TOPLEVEL[b] <- n2
          n3 <- $ImportName(x)(PYCTuple ([|PYCString ("y"); PYCString ("a")|]), PYCInt (0))
          n4 <- $ImportFrom(y)(n3)
          TOPLEVEL[z] <- n4
          n5 <- $ImportFrom(a)(n3)
          TOPLEVEL[b] <- n5
          n6 <- TOPLEVEL[z]
          n7 <- n6()
          n8 <- TOPLEVEL[b]
          n9 <- n8()
          n10 <- $ImportName(foo)(PYCTuple ([|PYCString ("toto"); PYCString ("tata")|]), PYCInt (0))
          n11 <- $ImportFrom(toto)(n10)
          TOPLEVEL[toto] <- n11
          n12 <- $ImportFrom(tata)(n10)
          TOPLEVEL[tata] <- n12
          n13 <- $ImportName(foo)(PYCTuple ([|PYCString ("toto"); PYCString ("tata")|]), PYCInt (0))
          n14 <- $ImportFrom(toto)(n13)
          TOPLEVEL[toto] <- n14
          n15 <- $ImportFrom(tata)(n13)
          TOPLEVEL[tata] <- n15
          n16 <- TOPLEVEL[toto]
          n17 <- n16()
          n18 <- TOPLEVEL[tata]
          n19 <- n18()
          return PYCNone |}]


let%expect_test _ =
  let source = {|
# From Cinder
import xml.etree.ElementTree as ET
          |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- $ImportName(xml.etree.ElementTree)(PYCNone, PYCInt (0))
          n1 <- $ImportFrom(etree)(n0)
          n2 <- $ImportFrom(ElementTree)(n1)
          TOPLEVEL[ET] <- n2
          return PYCNone |}]


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

      toplevel:
        b0:
          n0 <- $ImportName(unittest)(PYCNone, PYCInt (0))
          TOPLEVEL[unittest] <- n0
          n1 <- $ImportName(signal)(PYCNone, PYCInt (0))
          TOPLEVEL[signal] <- n1
          n2 <- TOPLEVEL[unittest]
          n3 <- TOPLEVEL[hasattr]
          n4 <- TOPLEVEL[signal]
          n5 <- n3(n4, PYCString ("setitimer"))
          n6 <- n2.skipUnless(n5, PYCString ("requires setitimer()"))
          n7 <- TOPLEVEL[unittest]
          n8 <- n7.TestCase
          n9 <- $BuildClass($FuncObj(Test, dummy.Test, {}), PYCString ("Test"), n8)
          n10 <- n6(n9)
          TOPLEVEL[Test] <- n10
          return PYCNone


      dummy.Test:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- PYCString ("Test")
          return PYCNone |}]


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

      toplevel:
        b0:
          n0 <- $ImportName(foo)(PYCNone, PYCInt (0))
          TOPLEVEL[foo] <- n0
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.f:
        b0:
          n0 <- GLOBAL[foo]
          n1 <- n0.bar(PYCInt (42))
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

      toplevel:
        b0:
          n0 <- $ImportName(foo)(PYCNone, PYCInt (0))
          TOPLEVEL[foo] <- n0
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.f:
        b0:
          n0 <- GLOBAL[foo]
          n1 <- n0.bar()
          jmp b6

        b6:
          return PYCNone |}]


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

      toplevel:
        b0:
          n0 <- $ImportName(decimal)(PYCNone, PYCInt (0))
          TOPLEVEL[decimal] <- n0
          TOPLEVEL[assertEqual] <- $FuncObj(assertEqual, dummy.assertEqual, {})
          TOPLEVEL[test_format_specifier_expressions] <- $FuncObj(test_format_specifier_expressions, dummy.test_format_specifier_expressions, {})
          return PYCNone


      dummy.assertEqual:
        b0:
          return PYCNone


      dummy.test_format_specifier_expressions:
        b0:
          LOCAL[width] <- PYCInt (10)
          LOCAL[precision] <- PYCInt (4)
          n0 <- GLOBAL[decimal]
          n1 <- n0.Decimal(PYCString ("12.34567"))
          LOCAL[value] <- n1
          n2 <- GLOBAL[assertEqual]
          n3 <- LOCAL[value]
          n4 <- LOCAL[width]
          n5 <- $Format(n4, PYCNone)
          n6 <- LOCAL[precision]
          n7 <- $Format(n6, PYCNone)
          n8 <- $Format(n3, $Concat(n5, PYCString ("."), n7))
          n9 <- n2($Concat(PYCString ("result: "), n8))
          return PYCNone |}]


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

      toplevel:
        b0:
          $SETUP_ANNOTATIONS
          n0 <- TOPLEVEL[int]
          n1 <- TOPLEVEL[__annotations__]
          n1[PYCString ("x")] <- n0
          TOPLEVEL[x] <- PYCInt (0)
          TOPLEVEL[y] <- PYCString ("zuck")
          n2 <- TOPLEVEL[str]
          n3 <- TOPLEVEL[__annotations__]
          n3[PYCString ("y")] <- n2
          n4 <- $ImportName(C)(PYCNone, PYCInt (0))
          TOPLEVEL[C] <- n4
          TOPLEVEL[z] <- PYCInt (42)
          n5 <- TOPLEVEL[C]
          n6 <- n5.T
          n7 <- TOPLEVEL[__annotations__]
          n7[PYCString ("z")] <- n6
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.f:
        b0:
          LOCAL[u] <- PYCInt (0)
          LOCAL[v] <- PYCString ("tata")
          return PYCNone |}]


let%expect_test _ =
  let source = {|
from foo import *
|} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- $ImportName(foo)(PYCTuple ([|PYCString ("*")|]), PYCInt (0))
          n1 <- $ImportStar(n0)
          return PYCNone |}]
