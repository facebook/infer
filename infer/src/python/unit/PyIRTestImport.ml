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
          base(PyIR.Name) <- n0
          n1 <- $ImportName(base)(PYCNone, PYCInt (0))
          base(PyIR.Name) <- n1
          n2 <- $CallMethod($LoadMethod(base(PyIR.Name), f), PYCInt (0))
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
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          n0 <- f(PyIR.Name)()
          n1 <- $ImportName(base)(PYCTuple ([|PYCString ("f"); PYCString ("g")|]), PYCInt (0))
          n2 <- $ImportFrom(f)(n1)
          f(PyIR.Name) <- n2
          n3 <- $ImportFrom(g)(n1)
          g(PyIR.Name) <- n3
          n4 <- f(PyIR.Name)()
          n5 <- $ImportName(base)(PYCTuple ([|PYCString ("f"); PYCString ("g")|]), PYCInt (0))
          n6 <- $ImportFrom(f)(n5)
          f(PyIR.Name) <- n6
          n7 <- $ImportFrom(g)(n5)
          g(PyIR.Name) <- n7
          n8 <- g(PyIR.Name)()
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
          unittest(PyIR.Name) <- n0
          n1 <- $BuildClass($FuncObj(MyTest, dummy.MyTest, {}), PYCString ("MyTest"), unittest(PyIR.Name).TestCase)
          MyTest(PyIR.Name) <- n1
          return PYCNone


      dummy.MyTest:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("MyTest")
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
          os(PyIR.Name) <- n0
          n1 <- $ImportName(sys)(PYCNone, PYCInt (0))
          sys(PyIR.Name) <- n1
          n2 <- $ImportName(test.libregrtest)(PYCTuple ([|PYCString ("main")|]), PYCInt (0))
          n3 <- $ImportFrom(main)(n2)
          main(PyIR.Name) <- n3
          main_in_temp_cwd(PyIR.Name) <- main(PyIR.Name)
          _main(PyIR.Name) <- $FuncObj(_main, dummy._main, {})
          n4 <- $Compare.eq(__name__(PyIR.Name), PYCString ("__main__"))
          if n4 then jmp b1 else jmp b2

        b1:
          n5 <- _main(PyIR.Name)()
          jmp b2

        b2:
          return PYCNone


      dummy._main:
        b0:
          n0 <- $CallMethod($LoadMethod(os(PyIR.Global).path, dirname), sys(PyIR.Global).argv[
                                                                        PYCInt (0)])
          n1 <- $CallMethod($LoadMethod(os(PyIR.Global).path, normpath), n0)
          n2 <- $CallMethod($LoadMethod(os(PyIR.Global).path, abspath), n1)
          mydir(PyIR.Fast) <- n2
          n3 <- len(PyIR.Global)(sys(PyIR.Global).path)
          n4 <- $Binary.Subtract(n3, PYCInt (1))
          i(PyIR.Fast) <- n4
          jmp b1

        b1:
          n5 <- $Compare.ge(i(PyIR.Fast), PYCInt (0))
          if n5 then jmp b2 else jmp b3

        b2:
          n6 <- $CallMethod($LoadMethod(os(PyIR.Global).path, normpath),
            sys(PyIR.Global).path[i(PyIR.Fast)])
          n7 <- $CallMethod($LoadMethod(os(PyIR.Global).path, abspath), n6)
          n8 <- $Compare.eq(n7, mydir(PyIR.Fast))
          if n8 then jmp b4 else jmp b5

        b3:
          n10 <- $CallMethod($LoadMethod(os(PyIR.Global).path, abspath), __file__(PyIR.Global))
          __file__(PyIR.Global) <- n10
          n11 <- main(PyIR.Global)()
          return PYCNone

        b4:
          jmp b1

        b5:
          n9 <- $Inplace.Subtract(i(PyIR.Fast), PYCInt (1))
          i(PyIR.Fast) <- n9
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
          X(PyIR.Name) <- n1
          n2 <- X(PyIR.Name)()
          n3 <- $ImportName(B)(PYCTuple ([|PYCString ("X")|]), PYCInt (1))
          n4 <- $ImportFrom(X)(n3)
          X(PyIR.Name) <- n4
          n5 <- X(PyIR.Name)()
          n6 <- $ImportName(C)(PYCTuple ([|PYCString ("X")|]), PYCInt (2))
          n7 <- $ImportFrom(X)(n6)
          X(PyIR.Name) <- n7
          n8 <- X(PyIR.Name)()
          n9 <- $ImportName()(PYCTuple ([|PYCString ("path")|]), PYCInt (2))
          n10 <- $ImportFrom(path)(n9)
          path(PyIR.Name) <- n10
          n11 <- $CallMethod($LoadMethod(path(PyIR.Name), X), )
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
          z(PyIR.Name) <- n1
          n2 <- $ImportFrom(a)(n0)
          b(PyIR.Name) <- n2
          n3 <- $ImportName(x)(PYCTuple ([|PYCString ("y"); PYCString ("a")|]), PYCInt (0))
          n4 <- $ImportFrom(y)(n3)
          z(PyIR.Name) <- n4
          n5 <- $ImportFrom(a)(n3)
          b(PyIR.Name) <- n5
          n6 <- z(PyIR.Name)()
          n7 <- b(PyIR.Name)()
          n8 <- $ImportName(foo)(PYCTuple ([|PYCString ("toto"); PYCString ("tata")|]), PYCInt (0))
          n9 <- $ImportFrom(toto)(n8)
          toto(PyIR.Name) <- n9
          n10 <- $ImportFrom(tata)(n8)
          tata(PyIR.Name) <- n10
          n11 <- $ImportName(foo)(PYCTuple ([|PYCString ("toto"); PYCString ("tata")|]), PYCInt (0))
          n12 <- $ImportFrom(toto)(n11)
          toto(PyIR.Name) <- n12
          n13 <- $ImportFrom(tata)(n11)
          tata(PyIR.Name) <- n13
          n14 <- toto(PyIR.Name)()
          n15 <- tata(PyIR.Name)()
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
          ET(PyIR.Name) <- n2
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
          unittest(PyIR.Name) <- n0
          n1 <- $ImportName(signal)(PYCNone, PYCInt (0))
          signal(PyIR.Name) <- n1
          n2 <- hasattr(PyIR.Name)(signal(PyIR.Name), PYCString ("setitimer"))
          n3 <- $CallMethod($LoadMethod(unittest(PyIR.Name), skipUnless),
            n2, PYCString ("requires setitimer()"))
          n4 <- $BuildClass($FuncObj(Test, dummy.Test, {}), PYCString ("Test"), unittest(PyIR.Name).TestCase)
          n5 <- n3(n4)
          Test(PyIR.Name) <- n5
          return PYCNone


      dummy.Test:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("Test")
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
          foo(PyIR.Name) <- n0
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.f:
        b0:
          n0 <- $CallMethod($LoadMethod(foo(PyIR.Global), bar), PYCInt (42))
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
          foo(PyIR.Name) <- n0
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.f:
        b0:
          n0 <- $CallMethod($LoadMethod(foo(PyIR.Global), bar), )
          jmp b2

        b1:
          n7 <- $Compare.exception(n6, OverflowError(PyIR.Global))
          if n7 then jmp b3(n6, n5, n4, n3, n2, n1) else jmp b4(n6, n5, n4, n3, n2, n1)

        b2:
          return PYCNone

        b3:
          if $Not(ok(PyIR.Fast)) then jmp b5(n10, n9, n8) else jmp b6(n10, n9, n8)

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
          decimal(PyIR.Name) <- n0
          assertEqual(PyIR.Name) <- $FuncObj(assertEqual, dummy.assertEqual, {})
          test_format_specifier_expressions(PyIR.Name) <- $FuncObj(test_format_specifier_expressions, dummy.test_format_specifier_expressions, {})
          return PYCNone


      dummy.assertEqual:
        b0:
          return PYCNone


      dummy.test_format_specifier_expressions:
        b0:
          width(PyIR.Fast) <- PYCInt (10)
          precision(PyIR.Fast) <- PYCInt (4)
          n0 <- $CallMethod($LoadMethod(decimal(PyIR.Global), Decimal), PYCString ("12.34567"))
          value(PyIR.Fast) <- n0
          n1 <- $Format(width(PyIR.Fast), PYCNone)
          n2 <- $Format(precision(PyIR.Fast), PYCNone)
          n3 <- $Format(value(PyIR.Fast), $Concat(n1, PYCString ("."), n2))
          n4 <- assertEqual(PyIR.Global)($Concat(PYCString ("result: "), n3))
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
          __annotations__(PyIR.Name)[PYCString ("x")] <- int(PyIR.Name)
          x(PyIR.Name) <- PYCInt (0)
          y(PyIR.Name) <- PYCString ("zuck")
          __annotations__(PyIR.Name)[PYCString ("y")] <- str(PyIR.Name)
          n0 <- $ImportName(C)(PYCNone, PYCInt (0))
          C(PyIR.Name) <- n0
          z(PyIR.Name) <- PYCInt (42)
          __annotations__(PyIR.Name)[PYCString ("z")] <- C(PyIR.Name).T
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          return PYCNone


      dummy.f:
        b0:
          u(PyIR.Fast) <- PYCInt (0)
          v(PyIR.Fast) <- PYCString ("tata")
          return PYCNone |}]
