(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let run module_ =
  let result =
    let open IResult.Let_syntax in
    let textual = PyIR2Textual.mk_module module_ in
    let+ verified_textual =
      TextualVerification.verify textual |> Result.map_error ~f:(fun err -> `VerificationError err)
    in
    let transformed_textual, _ = TextualTransform.run Python verified_textual in
    PyIR2Textual.gen_module_default_type_debug transformed_textual
  in
  match result with
  | Ok _ ->
      ()
  | Error (`VerificationError errs) ->
      List.iter errs ~f:(F.printf "%a@\n" TextualVerification.pp_error)
  | Error (`TransformationError errs) ->
      let sourcefile = Textual.SourceFile.create "dummy.sil" in
      List.iter errs ~f:(F.printf "%a@\n" (Textual.pp_transform_error sourcefile))


let%expect_test _ =
  let source =
    {|
import random
import asyncio as a
from dir1.dir2.mod import x as y
from dir1.dir2 import mod

x = 0

def f(y, l):
    if y:
        g(0, y)
    else:
        for i in l:
            print(i)
        done()

async def g():
    await sleep(1)

class D:
    def foo():
        pass

class C:
    def foo():
        pass
|}
  in
  PyIR.test ~run source ;
  [%expect
    {|
    type PyGlobals<dummy> = {
        random: Import[random]
        a: Import[asyncio]
        y: ImportFrom[dir1::dir2::mod, x]
        mod: ImportFrom[dir1::dir2, mod]
        f: PyClosure<dummy.f>
        g: PyClosure<dummy.g>
        D: Class[D]
        C: Class[C]
    }

    type PyClassCompanion<dummy,D> = {
        foo: PyClosure<dummy.D.foo>
    }

    type PyClassCompanion<dummy,C> = {
        foo: PyClosure<dummy.C.foo>
    } |}]
