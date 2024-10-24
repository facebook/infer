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
    function dummy::g called with 4 arguments while declared with 2 parameters
    function dummy::f called with 4 arguments while declared with 2 parameters |}]
