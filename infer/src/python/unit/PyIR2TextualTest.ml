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
    F.printf "TRANSFORMATION PyIR -> Textual@\n" ;
    F.printf "%a" Textual.Module.pp textual ;
    let+ verified_textual =
      TextualVerification.verify textual |> Result.map_error ~f:(fun err -> `VerificationError err)
    in
    F.printf "TYPE INFERENCE@\n" ;
    F.printf "%a" Textual.Module.pp verified_textual ;
    let transformed_textual, _ = TextualTransform.run Python verified_textual in
    F.printf "FINAL TRANSFORMATIONS@\n" ;
    F.printf "%a" Textual.Module.pp transformed_textual
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
|}
  in
  PyIR.test ~run source ;
  [%expect
    {|
    TRANSFORMATION PyIR -> Textual
    .source_language = "python"

    define dummy.__module_body__(globals: *PyGlobals::dummy) : *PyObject {
      local locals: *PyLocals
      #b0:
          store &locals <- globals
          _ = $builtins.py_store_name("x", locals, globals, $builtins.py_make_int(0))
          n0 = $builtins.py_make_function(fun (globals, locals) -> dummy.f(globals, locals), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none())
          _ = $builtins.py_store_name("f", locals, globals, n0)
          n1 = $builtins.py_make_function(fun (globals, locals) -> dummy.g(globals, locals), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none())
          _ = $builtins.py_store_name("g", locals, globals, n1)
          ret $builtins.py_make_none()

    }

    define .args = "y,l" dummy.f(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n0 = $builtins.py_load_fast("y", locals)
          if n0 then jmp b1 else jmp b2

      #b1:
          n10 = $builtins.py_load_global("g", globals)
          n11 = $builtins.py_load_fast("y", locals)
          n12 = $builtins.py_call(n10, globals, $builtins.py_make_none(), $builtins.py_make_int(0), n11)
          _ = $builtins.py_nullify_locals(locals, "i")
          ret $builtins.py_make_none()

      #b2:
          n1 = $builtins.py_load_fast("l", locals)
          n2 = $builtins.py_get_iter(n1)
          jmp b3

      #b3:
          n3 = $builtins.py_next_iter(n2)
          n4 = $builtins.py_has_next_iter(n2)
          if n4 then jmp b4 else jmp b5

      #b4:
          _ = $builtins.py_store_fast("i", locals, n3)
          n7 = $builtins.py_load_global("print", globals)
          n8 = $builtins.py_load_fast("i", locals)
          n9 = $builtins.py_call(n7, globals, $builtins.py_make_none(), n8)
          jmp b3

      #b5:
          n5 = $builtins.py_load_global("done", globals)
          n6 = $builtins.py_call(n5, globals, $builtins.py_make_none())
          _ = $builtins.py_nullify_locals(locals, "i")
          ret $builtins.py_make_none()

    }

    define .async dummy.g(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          _ = $builtins.py_gen_start_coroutine()
          n0 = $builtins.py_load_global("sleep", globals)
          n1 = $builtins.py_call(n0, globals, $builtins.py_make_none(), $builtins.py_make_int(1))
          n2 = $builtins.py_get_awaitable(n1)
          n3 = $builtins.py_yield_from(n2, $builtins.py_make_none())
          ret $builtins.py_make_none()

    }

    TYPE INFERENCE
    .source_language = "python"

    define dummy.__module_body__(globals: *PyGlobals::dummy) : *PyObject {
      local locals: *PyLocals
      #b0:
          store &locals <- [&globals:*PyGlobals::dummy]:*PyGlobals::dummy
          _ = $builtins.py_store_name("x", [&locals:*PyLocals], [&globals:*PyGlobals::dummy], $builtins.py_make_int(0))
          n0 = $builtins.py_make_function(fun (globals, locals) -> dummy.f(globals, locals), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none())
          _ = $builtins.py_store_name("f", [&locals:*PyLocals], [&globals:*PyGlobals::dummy], n0)
          n1 = $builtins.py_make_function(fun (globals, locals) -> dummy.g(globals, locals), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none())
          _ = $builtins.py_store_name("g", [&locals:*PyLocals], [&globals:*PyGlobals::dummy], n1)
          ret $builtins.py_make_none()

    }

    define .args = "y,l" dummy.f(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n0 = $builtins.py_load_fast("y", [&locals:*PyLocals])
          if n0 then jmp b1 else jmp b2

      #b1:
          n10 = $builtins.py_load_global("g", [&globals:*PyGlobals::dummy])
          n11 = $builtins.py_load_fast("y", [&locals:*PyLocals])
          n12 = $builtins.py_call(n10, [&globals:*PyGlobals::dummy], $builtins.py_make_none(), $builtins.py_make_int(0), n11)
          _ = $builtins.py_nullify_locals([&locals:*PyLocals], "i")
          ret $builtins.py_make_none()

      #b2:
          n1 = $builtins.py_load_fast("l", [&locals:*PyLocals])
          n2 = $builtins.py_get_iter(n1)
          jmp b3

      #b3:
          n3 = $builtins.py_next_iter(n2)
          n4 = $builtins.py_has_next_iter(n2)
          if n4 then jmp b4 else jmp b5

      #b4:
          _ = $builtins.py_store_fast("i", [&locals:*PyLocals], n3)
          n7 = $builtins.py_load_global("print", [&globals:*PyGlobals::dummy])
          n8 = $builtins.py_load_fast("i", [&locals:*PyLocals])
          n9 = $builtins.py_call(n7, [&globals:*PyGlobals::dummy], $builtins.py_make_none(), n8)
          jmp b3

      #b5:
          n5 = $builtins.py_load_global("done", [&globals:*PyGlobals::dummy])
          n6 = $builtins.py_call(n5, [&globals:*PyGlobals::dummy], $builtins.py_make_none())
          _ = $builtins.py_nullify_locals([&locals:*PyLocals], "i")
          ret $builtins.py_make_none()

    }

    define .async dummy.g(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          _ = $builtins.py_gen_start_coroutine()
          n0 = $builtins.py_load_global("sleep", [&globals:*PyGlobals::dummy])
          n1 = $builtins.py_call(n0, [&globals:*PyGlobals::dummy], $builtins.py_make_none(), $builtins.py_make_int(1))
          n2 = $builtins.py_get_awaitable(n1)
          n3 = $builtins.py_yield_from(n2, $builtins.py_make_none())
          ret $builtins.py_make_none()

    }

    FINAL TRANSFORMATIONS
    .source_language = "python"

    type closure:dummy:1 = {}

    define .closure_wrapper .async closure:dummy:1.call(__this: *closure:dummy:1, globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #entry:
          n0:*PyGlobals::dummy = load &globals
          n1:*PyLocals = load &locals
          n2 = dummy.g(n0, n1)
          ret n2

    }

    type closure:dummy:0 = {}

    define .closure_wrapper .args = "y,l" closure:dummy:0.call(__this: *closure:dummy:0, globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #entry:
          n0:*PyGlobals::dummy = load &globals
          n1:*PyLocals = load &locals
          n2 = dummy.f(n0, n1)
          ret n2

    }

    define dummy.__module_body__(globals: *PyGlobals::dummy) : *PyObject {
      local locals: *PyLocals
      #b0:
          n2:*PyGlobals::dummy = load &globals
          store &locals <- n2:*PyGlobals::dummy
          n3:*PyLocals = load &locals
          n4:*PyGlobals::dummy = load &globals
          n5 = $builtins.py_make_int(0)
          n6 = $builtins.py_store_name("x", n3, n4, n5)
          n7 = __sil_allocate(<closure:dummy:0>)
          n9 = $builtins.py_make_none()
          n10 = $builtins.py_make_none()
          n11 = $builtins.py_make_none()
          n12 = $builtins.py_make_none()
          n0 = $builtins.py_make_function(n7, n9, n10, n11, n12)
          n13:*PyLocals = load &locals
          n14:*PyGlobals::dummy = load &globals
          n15 = $builtins.py_store_name("f", n13, n14, n0)
          n16 = __sil_allocate(<closure:dummy:1>)
          n18 = $builtins.py_make_none()
          n19 = $builtins.py_make_none()
          n20 = $builtins.py_make_none()
          n21 = $builtins.py_make_none()
          n1 = $builtins.py_make_function(n16, n18, n19, n20, n21)
          n22:*PyLocals = load &locals
          n23:*PyGlobals::dummy = load &globals
          n24 = $builtins.py_store_name("g", n22, n23, n1)
          n25 = $builtins.py_make_none()
          ret n25

    }

    define .args = "y,l" dummy.f(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n13:*PyLocals = load &locals
          n0 = $builtins.py_load_fast("y", n13)
          jmp b1, b2

      #b1:
          prune n0
          n14:*PyGlobals::dummy = load &globals
          n10 = $builtins.py_load_global("g", n14)
          n15:*PyLocals = load &locals
          n11 = $builtins.py_load_fast("y", n15)
          n16:*PyGlobals::dummy = load &globals
          n17 = $builtins.py_make_none()
          n18 = $builtins.py_make_int(0)
          n12 = $builtins.py_call(n10, n16, n17, n18, n11)
          n19:*PyLocals = load &locals
          n20 = $builtins.py_nullify_locals(n19, "i")
          n21 = $builtins.py_make_none()
          ret n21

      #b2:
          prune __sil_lnot(n0)
          n22:*PyLocals = load &locals
          n1 = $builtins.py_load_fast("l", n22)
          n2 = $builtins.py_get_iter(n1)
          jmp b3

      #b3:
          n3 = $builtins.py_next_iter(n2)
          n4 = $builtins.py_has_next_iter(n2)
          jmp b4, b5

      #b4:
          prune n4
          n23:*PyLocals = load &locals
          n24 = $builtins.py_store_fast("i", n23, n3)
          n25:*PyGlobals::dummy = load &globals
          n7 = $builtins.py_load_global("print", n25)
          n26:*PyLocals = load &locals
          n8 = $builtins.py_load_fast("i", n26)
          n27:*PyGlobals::dummy = load &globals
          n28 = $builtins.py_make_none()
          n9 = $builtins.py_call(n7, n27, n28, n8)
          jmp b3

      #b5:
          prune __sil_lnot(n4)
          n29:*PyGlobals::dummy = load &globals
          n5 = $builtins.py_load_global("done", n29)
          n30:*PyGlobals::dummy = load &globals
          n31 = $builtins.py_make_none()
          n6 = $builtins.py_call(n5, n30, n31)
          n32:*PyLocals = load &locals
          n33 = $builtins.py_nullify_locals(n32, "i")
          n34 = $builtins.py_make_none()
          ret n34

    }

    define .async dummy.g(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n4 = $builtins.py_gen_start_coroutine()
          n5:*PyGlobals::dummy = load &globals
          n0 = $builtins.py_load_global("sleep", n5)
          n6:*PyGlobals::dummy = load &globals
          n7 = $builtins.py_make_none()
          n8 = $builtins.py_make_int(1)
          n1 = $builtins.py_call(n0, n6, n7, n8)
          n2 = $builtins.py_get_awaitable(n1)
          n9 = $builtins.py_make_none()
          n3 = $builtins.py_yield_from(n2, n9)
          n10 = $builtins.py_make_none()
          ret n10

    } |}]
