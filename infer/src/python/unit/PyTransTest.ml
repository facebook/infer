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
    let source = "x = 42"

    let sourcefile = Textual.SourceFile.create "dummy.py"

    let%expect_test _ =
      Py.initialize ~version:3 ~minor:8 () ;
      let code = FFI.from_string ~source ~filename:"dummy" in
      Py.finalize () ;
      let res = PyTrans.to_module ~sourcefile "$toplevel::main" code in
      F.printf "%a" Textual.Module.pp res ;
      [%expect
        {|
        define $toplevel::main() : *PyObject {
          #b0:
              store &$globals::x <- $builtins.python_int(42):*PyObject
              ret null

        }

        global $globals::x: *PyObject

        declare $builtins.python_int(int) : *PyObject

        declare $builtins.python_string(*PyObject) : *PyObject

        declare $builtins.python_tuple(*PyObject) : *PyObject |}]
  end )
