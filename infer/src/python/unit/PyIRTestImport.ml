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
          n2 <- $CallMethod($LoadMethod(TOPLEVEL[base], f), PYCInt (0))
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
          n0 <- TOPLEVEL[f]()
          n1 <- $ImportName(base)(PYCTuple ([|PYCString ("f"); PYCString ("g")|]), PYCInt (0))
          n2 <- $ImportFrom(f)(n1)
          TOPLEVEL[f] <- n2
          n3 <- $ImportFrom(g)(n1)
          TOPLEVEL[g] <- n3
          n4 <- TOPLEVEL[f]()
          n5 <- $ImportName(base)(PYCTuple ([|PYCString ("f"); PYCString ("g")|]), PYCInt (0))
          n6 <- $ImportFrom(f)(n5)
          TOPLEVEL[f] <- n6
          n7 <- $ImportFrom(g)(n5)
          TOPLEVEL[g] <- n7
          n8 <- TOPLEVEL[g]()
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
          n1 <- $BuildClass($FuncObj(MyTest, dummy.MyTest, {}), PYCString ("MyTest"), TOPLEVEL[unittest].TestCase)
          TOPLEVEL[MyTest] <- n1
          return PYCNone


      dummy.MyTest:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
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
          TOPLEVEL[main_in_temp_cwd] <- TOPLEVEL[main]
          TOPLEVEL[_main] <- $FuncObj(_main, dummy._main, {})
          n4 <- $Compare.eq(TOPLEVEL[__name__], PYCString ("__main__"))
          if n4 then jmp b1 else jmp b2

        b1:
          n5 <- TOPLEVEL[_main]()
          jmp b2

        b2:
          return PYCNone


      dummy._main:
        b0:
          n0 <- $CallMethod($LoadMethod(GLOBAL[os].path, dirname), GLOBAL[sys].argv[PYCInt (0)])
          n1 <- $CallMethod($LoadMethod(GLOBAL[os].path, normpath), n0)
          n2 <- $CallMethod($LoadMethod(GLOBAL[os].path, abspath), n1)
          LOCAL[mydir] <- n2
          n3 <- GLOBAL[len](GLOBAL[sys].path)
          n4 <- $Binary.Subtract(n3, PYCInt (1))
          LOCAL[i] <- n4
          jmp b1

        b1:
          n5 <- $Compare.ge(LOCAL[i], PYCInt (0))
          if n5 then jmp b2 else jmp b3

        b2:
          n6 <- $CallMethod($LoadMethod(GLOBAL[os].path, normpath), GLOBAL[sys].path[LOCAL[i]])
          n7 <- $CallMethod($LoadMethod(GLOBAL[os].path, abspath), n6)
          n8 <- $Compare.eq(n7, LOCAL[mydir])
          if n8 then jmp b4 else jmp b5

        b3:
          n10 <- $CallMethod($LoadMethod(GLOBAL[os].path, abspath), GLOBAL[__file__])
          GLOBAL[__file__] <- n10
          n11 <- GLOBAL[main]()
          return PYCNone

        b4:
          jmp b1

        b5:
          n9 <- $Inplace.Subtract(LOCAL[i], PYCInt (1))
          LOCAL[i] <- n9
          jmp b1 |}]


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
          n2 <- TOPLEVEL[X]()
          n3 <- $ImportName(B)(PYCTuple ([|PYCString ("X")|]), PYCInt (1))
          n4 <- $ImportFrom(X)(n3)
          TOPLEVEL[X] <- n4
          n5 <- TOPLEVEL[X]()
          n6 <- $ImportName(C)(PYCTuple ([|PYCString ("X")|]), PYCInt (2))
          n7 <- $ImportFrom(X)(n6)
          TOPLEVEL[X] <- n7
          n8 <- TOPLEVEL[X]()
          n9 <- $ImportName()(PYCTuple ([|PYCString ("path")|]), PYCInt (2))
          n10 <- $ImportFrom(path)(n9)
          TOPLEVEL[path] <- n10
          n11 <- $CallMethod($LoadMethod(TOPLEVEL[path], X), )
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
          n6 <- TOPLEVEL[z]()
          n7 <- TOPLEVEL[b]()
          n8 <- $ImportName(foo)(PYCTuple ([|PYCString ("toto"); PYCString ("tata")|]), PYCInt (0))
          n9 <- $ImportFrom(toto)(n8)
          TOPLEVEL[toto] <- n9
          n10 <- $ImportFrom(tata)(n8)
          TOPLEVEL[tata] <- n10
          n11 <- $ImportName(foo)(PYCTuple ([|PYCString ("toto"); PYCString ("tata")|]), PYCInt (0))
          n12 <- $ImportFrom(toto)(n11)
          TOPLEVEL[toto] <- n12
          n13 <- $ImportFrom(tata)(n11)
          TOPLEVEL[tata] <- n13
          n14 <- TOPLEVEL[toto]()
          n15 <- TOPLEVEL[tata]()
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
          n2 <- TOPLEVEL[hasattr](TOPLEVEL[signal], PYCString ("setitimer"))
          n3 <- $CallMethod($LoadMethod(TOPLEVEL[unittest], skipUnless),
            n2, PYCString ("requires setitimer()"))
          n4 <- $BuildClass($FuncObj(Test, dummy.Test, {}), PYCString ("Test"), TOPLEVEL[unittest].TestCase)
          n5 <- n3(n4)
          TOPLEVEL[Test] <- n5
          return PYCNone


      dummy.Test:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
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
          n0 <- $CallMethod($LoadMethod(GLOBAL[foo], bar), PYCInt (42))
          throw n0 |}]


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
          n0 <- $CallMethod($LoadMethod(GLOBAL[foo], bar), )
          jmp b2

        b1:
          n7 <- $Compare.exception(n6, GLOBAL[OverflowError])
          if n7 then jmp b3(n6, n5, n4, n3, n2, n1) else jmp b4(n6, n5, n4, n3, n2, n1)

        b2:
          return PYCNone

        b3:
          if $Not(LOCAL[ok]) then jmp b5(n10, n9, n8) else jmp b6(n10, n9, n8)

        b4:
          jmp b2

        b5:
          n26 <- GetPreviousException()
          throw n26

        b6:
          jmp b7

        b7:
          jmp b2 |}]


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
          n0 <- $CallMethod($LoadMethod(GLOBAL[decimal], Decimal), PYCString ("12.34567"))
          LOCAL[value] <- n0
          n1 <- $Format(LOCAL[width], PYCNone)
          n2 <- $Format(LOCAL[precision], PYCNone)
          n3 <- $Format(LOCAL[value], $Concat(n1, PYCString ("."), n2))
          n4 <- GLOBAL[assertEqual]($Concat(PYCString ("result: "), n3))
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
          TOPLEVEL[__annotations__][PYCString ("x")] <- TOPLEVEL[int]
          TOPLEVEL[x] <- PYCInt (0)
          TOPLEVEL[y] <- PYCString ("zuck")
          TOPLEVEL[__annotations__][PYCString ("y")] <- TOPLEVEL[str]
          n0 <- $ImportName(C)(PYCNone, PYCInt (0))
          TOPLEVEL[C] <- n0
          TOPLEVEL[z] <- PYCInt (42)
          TOPLEVEL[__annotations__][PYCString ("z")] <- TOPLEVEL[C].T
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.f:
        b0:
          LOCAL[u] <- PYCInt (0)
          LOCAL[v] <- PYCString ("tata")
          return PYCNone |}]
