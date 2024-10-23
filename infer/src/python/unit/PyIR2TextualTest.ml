(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let run module_ = PyIR2Textual.mk_module module_ |> F.printf "%a" Textual.Module.pp

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
          ret null

    }

    define dummy::f(globals: *PyGlobals, locals: *PyLocals) : *PyObject {
      #b0:
          if null then jmp b1 else jmp b2

      #b1:
          jmp b6

      #b2:
          jmp b3

      #b3:
          if null then jmp b4 else jmp b5

      #b4:
          jmp b3

      #b5:
          jmp b6

      #b6:
          ret null

    }

    define dummy::g(globals: *PyGlobals, locals: *PyLocals) : *PyObject {
      #b0:
          ret null

    } |}]
