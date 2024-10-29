(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let run module_ =
  let textual = PyIR2Textual.mk_module module_ in
  match TextualVerification.verify textual with
  | Ok _ ->
      F.printf "%a" Textual.Module.pp textual
  | Error errs ->
      List.iter errs ~f:(F.printf "%a@\n" TextualVerification.pp_error)


let%expect_test _ =
  let source =
    {|
x = 0

def f(y, l):
    if y:
        g(0, y)
    else:
        for i in l:
            print(i)
        done()

def g():
    print(x)
|}
  in
  PyIR.test ~run source ;
  [%expect
    {|
    define dummy::__module_body__() : *PyObject {
      #b0:
          _ = $builtins.py_store_name("x", locals, globals, $builtins.py_make_int(0))
          n0 = $builtins.py_make_function(fun (locals) -> dummy::f(globals, locals), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none())
          _ = $builtins.py_store_name("f", locals, globals, n0)
          n1 = $builtins.py_make_function(fun (locals) -> dummy::g(globals, locals), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none())
          _ = $builtins.py_store_name("g", locals, globals, n1)
          ret $builtins.py_make_none()

    }

    define dummy::f(globals: *PyGlobals, locals: *PyLocals) : *PyObject {
      #b0:
          n0 = $builtins.py_load_local("y", locals)
          if n0 then jmp b1 else jmp b2

      #b1:
          n10 = $builtins.py_load_global("g", globals)
          n11 = $builtins.py_load_local("y", locals)
          n12 = $builtins.py_call(n10, $builtins.py_make_none(), $builtins.py_make_int(0), n11)
          jmp b6

      #b2:
          n1 = $builtins.py_load_local("l", locals)
          n2 = $builtins.py_get_iter($builtins.py_make_none(), n1)
          jmp b3

      #b3:
          n3 = $builtins.py_next_iter($builtins.py_make_none(), n2)
          n4 = $builtins.py_has_next_iter($builtins.py_make_none(), n2)
          if n4 then jmp b4 else jmp b5

      #b4:
          _ = $builtins.py_store_fast("i", locals, n3)
          n7 = $builtins.py_load_global("print", globals)
          n8 = $builtins.py_load_local("i", locals)
          n9 = $builtins.py_call(n7, $builtins.py_make_none(), n8)
          jmp b3

      #b5:
          n5 = $builtins.py_load_global("done", globals)
          n6 = $builtins.py_call(n5, $builtins.py_make_none())
          jmp b6

      #b6:
          ret $builtins.py_make_none()

    }

    define dummy::g(globals: *PyGlobals, locals: *PyLocals) : *PyObject {
      #b0:
          n0 = $builtins.py_load_global("print", globals)
          n1 = $builtins.py_load_global("x", globals)
          n2 = $builtins.py_call(n0, $builtins.py_make_none(), n1)
          ret $builtins.py_make_none()

    } |}]
