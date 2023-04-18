(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let%test_module "to_proc_desc" =
  ( module struct
    let sourcefile = Textual.SourceFile.create "dummy.py"

    let%expect_test _ =
      let source = "x = 42" in
      Py.initialize ~interpreter:Version.python_exe () ;
      let code = FFI.from_string ~source ~filename:"dummy" in
      Py.finalize () ;
      let res = PyTrans.to_module ~sourcefile "$toplevel::main" code in
      F.printf "%a" Textual.Module.pp res ;
      [%expect
        {|
        .source_language = "python"

        define $toplevel::main() : *PyObject {
          #b0:
              store &$globals::x <- $builtins.python_int(42):*PyInt
              ret null

        }

        global $globals::x: *PyObject

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_string(*PyObject) : *PyString

        declare $builtins.python_int(int) : *PyInt |}]


    let%expect_test _ =
      let source = {|
x = 42
print(x)
      |} in
      Py.initialize ~interpreter:Version.python_exe () ;
      let code = FFI.from_string ~source ~filename:"dummy" in
      Py.finalize () ;
      let res = PyTrans.to_module ~sourcefile "$toplevel::main" code in
      F.printf "%a" Textual.Module.pp res ;
      [%expect
        {|
        .source_language = "python"

        define $toplevel::main() : *PyObject {
          #b0:
              store &$globals::x <- $builtins.python_int(42):*PyInt
              n0:*PyObject = load &$globals::x
              n1 = $builtins.print(n0)
              ret null

        }

        global $globals::x: *PyObject

        declare $builtins.print(...) : *PyObject

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_string(*PyObject) : *PyString

        declare $builtins.python_int(int) : *PyInt |}]


    let%expect_test _ =
      let source = {|
x = 42
y = 10
print(x + y)
      |} in
      Py.initialize ~interpreter:Version.python_exe () ;
      let code = FFI.from_string ~source ~filename:"dummy" in
      Py.finalize () ;
      let res = PyTrans.to_module ~sourcefile "$toplevel::main" code in
      F.printf "%a" Textual.Module.pp res ;
      [%expect
        {|
        .source_language = "python"

        define $toplevel::main() : *PyObject {
          #b0:
              store &$globals::x <- $builtins.python_int(42):*PyInt
              store &$globals::y <- $builtins.python_int(10):*PyInt
              n0:*PyObject = load &$globals::x
              n1:*PyObject = load &$globals::y
              n2 = $builtins.binary_add(n0, n1)
              n3 = $builtins.print(n2)
              ret null

        }

        global $globals::y: *PyObject

        global $globals::x: *PyObject

        declare $builtins.print(...) : *PyObject

        declare $builtins.binary_add(*PyObject, *PyObject) : *PyObject

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_string(*PyObject) : *PyString

        declare $builtins.python_int(int) : *PyInt |}]


    let%expect_test _ =
      let source =
        {|
# user-defined top level function
def my_fun(x, y):
        print(x)
        print(y)
        # local variable z
        z = x + y
        return z

a = 10
# global variable z
z = my_fun(42, a)
print(z)
      |}
      in
      Py.initialize ~interpreter:Version.python_exe () ;
      let code = FFI.from_string ~source ~filename:"dummy" in
      Py.finalize () ;
      let res = PyTrans.to_module ~sourcefile "$toplevel::main" code in
      F.printf "%a" Textual.Module.pp res ;
      [%expect
        {|
        .source_language = "python"

        define $toplevel::main() : *PyObject {
          #b0:
              store &$globals::a <- $builtins.python_int(10):*PyInt
              n0:*PyObject = load &$globals::a
              n1 = my_fun($builtins.python_int(42), n0)
              store &$globals::z <- n1:*PyObject
              n2:*PyObject = load &$globals::z
              n3 = $builtins.print(n2)
              ret null

        }

        define my_fun(x: *PyObject, y: *PyObject) : *PyObject {
          local z: *PyObject
          #b0:
              n0:*PyObject = load &x
              n1 = $builtins.print(n0)
              n2:*PyObject = load &y
              n3 = $builtins.print(n2)
              n4:*PyObject = load &x
              n5:*PyObject = load &y
              n6 = $builtins.binary_add(n4, n5)
              store &z <- n6:*PyObject
              n7:*PyObject = load &z
              ret n7

        }

        global $globals::z: *PyObject

        global $globals::a: *PyObject

        declare $builtins.print(...) : *PyObject

        declare $builtins.binary_add(*PyObject, *PyObject) : *PyObject

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_string(*PyObject) : *PyString

        declare $builtins.python_int(int) : *PyInt |}]


    let%expect_test _ =
      let source =
        {|
# testing global python attribute
def update_global():
        global z
        z = z + 1

z = 0
update_global()
print(z)
      |}
      in
      Py.initialize ~interpreter:Version.python_exe () ;
      let code = FFI.from_string ~source ~filename:"dummy" in
      Py.finalize () ;
      let res = PyTrans.to_module ~sourcefile "$toplevel::main" code in
      F.printf "%a" Textual.Module.pp res ;
      [%expect
        {|
        .source_language = "python"

        define $toplevel::main() : *PyObject {
          #b0:
              store &$globals::z <- $builtins.python_int(0):*PyInt
              n0 = update_global()
              n1:*PyObject = load &$globals::z
              n2 = $builtins.print(n1)
              ret null

        }

        define update_global() : *PyObject {
          #b0:
              n0:*PyObject = load &$globals::z
              n1 = $builtins.binary_add(n0, $builtins.python_int(1))
              store &$globals::z <- n1:*PyObject
              ret null

        }

        global $globals::z: *PyObject

        declare $builtins.print(...) : *PyObject

        declare $builtins.binary_add(*PyObject, *PyObject) : *PyObject

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_string(*PyObject) : *PyString

        declare $builtins.python_int(int) : *PyInt |}]
  end )
