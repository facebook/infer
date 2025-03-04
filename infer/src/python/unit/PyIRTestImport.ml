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
  [%expect {| |}]


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
  [%expect {| |}]


let%expect_test _ =
  let source = {|
import unittest

class MyTest(unittest.TestCase):
        pass
        |} in
  PyIR.test source ;
  [%expect {| |}]


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
  [%expect {| |}]


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
  [%expect {| |}]


let%expect_test _ =
  let source = {|
pass
|} in
  PyIR.test ~filename:"./dir1/dir2/script.py" source ;
  [%expect {| |}]


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
  [%expect {| |}]


let%expect_test _ =
  let source = {|
# From Cinder
import xml.etree.ElementTree as ET
          |} in
  PyIR.test source ;
  [%expect {| |}]


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
  [%expect {| |}]


let%expect_test _ =
  let source = {|
import foo

def f():
          raise foo.bar(42)
          |} in
  PyIR.test source ;
  [%expect {| |}]


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
  [%expect {| |}]


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
  [%expect {| |}]


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
  [%expect {| |}]


let%expect_test _ =
  let source = {|
from foo import *
|} in
  PyIR.test source ;
  [%expect {| |}]


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
  [%expect {| |}]
