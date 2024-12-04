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
    let transformed_textual = PyIR2Textual.add_module_default_type transformed_textual in
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
import random
import asyncio as a

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
    TRANSFORMATION PyIR -> Textual
    .source_language = "python"

    define dummy.__module_body__(globals: *PyGlobals::dummy) : *PyObject {
      local locals: *PyLocals
      #b0:
          store &locals <- globals
          n0 = $builtins.py_import_name("random", $builtins.py_make_none(), $builtins.py_make_int(0))
          _ = $builtins.py_store_name("random", locals, globals, n0)
          n1 = $builtins.py_import_name("asyncio", $builtins.py_make_none(), $builtins.py_make_int(0))
          _ = $builtins.py_store_name("a", locals, globals, n1)
          _ = $builtins.py_store_name("x", locals, globals, $builtins.py_make_int(0))
          n2 = $builtins.py_make_function(fun (globals, locals) -> dummy.f(globals, locals), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none())
          _ = $builtins.py_store_name("f", locals, globals, n2)
          n3 = $builtins.py_make_function(fun (globals, locals) -> dummy.g(globals, locals), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none())
          _ = $builtins.py_store_name("g", locals, globals, n3)
          n4 = $builtins.py_make_function(fun (globals, locals) -> dummy.D(globals, locals), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none())
          n5 = $builtins.py_build_class(globals, n4, $builtins.py_make_string("D"))
          _ = $builtins.py_store_name("D", locals, globals, n5)
          n6 = $builtins.py_make_function(fun (globals, locals) -> dummy.C(globals, locals), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none())
          n7 = $builtins.py_build_class(globals, n6, $builtins.py_make_string("C"))
          _ = $builtins.py_store_name("C", locals, globals, n7)
          ret $builtins.py_make_none()

    }

    define dummy.C(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n0 = $builtins.py_load_name("__name__", locals, globals)
          _ = $builtins.py_store_name("__module__", locals, globals, n0)
          _ = $builtins.py_store_name("__qualname__", locals, globals, $builtins.py_make_string("C"))
          n1 = $builtins.py_make_function(fun (globals, locals) -> dummy.C::foo(globals, locals), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none())
          _ = $builtins.py_store_name("foo", locals, globals, n1)
          ret $builtins.py_make_none()

    }

    define dummy.C::foo(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          ret $builtins.py_make_none()

    }

    define dummy.D(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n0 = $builtins.py_load_name("__name__", locals, globals)
          _ = $builtins.py_store_name("__module__", locals, globals, n0)
          _ = $builtins.py_store_name("__qualname__", locals, globals, $builtins.py_make_string("D"))
          n1 = $builtins.py_make_function(fun (globals, locals) -> dummy.D::foo(globals, locals), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none())
          _ = $builtins.py_store_name("foo", locals, globals, n1)
          ret $builtins.py_make_none()

    }

    define dummy.D::foo(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
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
          n0 = $builtins.py_import_name("random", $builtins.py_make_none(), $builtins.py_make_int(0))
          _ = $builtins.py_store_name("random", [&locals:*PyLocals], [&globals:*PyGlobals::dummy], n0)
          n1 = $builtins.py_import_name("asyncio", $builtins.py_make_none(), $builtins.py_make_int(0))
          _ = $builtins.py_store_name("a", [&locals:*PyLocals], [&globals:*PyGlobals::dummy], n1)
          _ = $builtins.py_store_name("x", [&locals:*PyLocals], [&globals:*PyGlobals::dummy], $builtins.py_make_int(0))
          n2 = $builtins.py_make_function(fun (globals, locals) -> dummy.f(globals, locals), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none())
          _ = $builtins.py_store_name("f", [&locals:*PyLocals], [&globals:*PyGlobals::dummy], n2)
          n3 = $builtins.py_make_function(fun (globals, locals) -> dummy.g(globals, locals), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none())
          _ = $builtins.py_store_name("g", [&locals:*PyLocals], [&globals:*PyGlobals::dummy], n3)
          n4 = $builtins.py_make_function(fun (globals, locals) -> dummy.D(globals, locals), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none())
          n5 = $builtins.py_build_class([&globals:*PyGlobals::dummy], n4, $builtins.py_make_string("D"))
          _ = $builtins.py_store_name("D", [&locals:*PyLocals], [&globals:*PyGlobals::dummy], n5)
          n6 = $builtins.py_make_function(fun (globals, locals) -> dummy.C(globals, locals), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none())
          n7 = $builtins.py_build_class([&globals:*PyGlobals::dummy], n6, $builtins.py_make_string("C"))
          _ = $builtins.py_store_name("C", [&locals:*PyLocals], [&globals:*PyGlobals::dummy], n7)
          ret $builtins.py_make_none()

    }

    define dummy.C(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n0 = $builtins.py_load_name("__name__", [&locals:*PyLocals], [&globals:*PyGlobals::dummy])
          _ = $builtins.py_store_name("__module__", [&locals:*PyLocals], [&globals:*PyGlobals::dummy], n0)
          _ = $builtins.py_store_name("__qualname__", [&locals:*PyLocals], [&globals:*PyGlobals::dummy], $builtins.py_make_string("C"))
          n1 = $builtins.py_make_function(fun (globals, locals) -> dummy.C::foo(globals, locals), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none())
          _ = $builtins.py_store_name("foo", [&locals:*PyLocals], [&globals:*PyGlobals::dummy], n1)
          ret $builtins.py_make_none()

    }

    define dummy.C::foo(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          ret $builtins.py_make_none()

    }

    define dummy.D(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n0 = $builtins.py_load_name("__name__", [&locals:*PyLocals], [&globals:*PyGlobals::dummy])
          _ = $builtins.py_store_name("__module__", [&locals:*PyLocals], [&globals:*PyGlobals::dummy], n0)
          _ = $builtins.py_store_name("__qualname__", [&locals:*PyLocals], [&globals:*PyGlobals::dummy], $builtins.py_make_string("D"))
          n1 = $builtins.py_make_function(fun (globals, locals) -> dummy.D::foo(globals, locals), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none())
          _ = $builtins.py_store_name("foo", [&locals:*PyLocals], [&globals:*PyGlobals::dummy], n1)
          ret $builtins.py_make_none()

    }

    define dummy.D::foo(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
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

    type PyGlobals::dummy = {random: *PyGlobals::random; a: *PyGlobals::asyncio; f: *closure:dummy:0;
                             g: *closure:dummy:1; D: *PyClassCompanion::dummy::D;
                             C: *PyClassCompanion::dummy::C}

    type PyClassCompanion::dummy::C = {foo: *closure:dummy:4}

    type PyClassCompanion::dummy::D = {foo: *closure:dummy:5}

    type .final closure:dummy:5 = {}

    define .closure_wrapper closure:dummy:5.call(__this: *closure:dummy:5, globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #entry:
          n0:*PyGlobals::dummy = load &globals
          n1:*PyLocals = load &locals
          n2 = dummy.D::foo(n0, n1)
          ret n2

    }

    type .final closure:dummy:4 = {}

    define .closure_wrapper closure:dummy:4.call(__this: *closure:dummy:4, globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #entry:
          n0:*PyGlobals::dummy = load &globals
          n1:*PyLocals = load &locals
          n2 = dummy.C::foo(n0, n1)
          ret n2

    }

    type .final closure:dummy:3 = {}

    define .closure_wrapper closure:dummy:3.call(__this: *closure:dummy:3, globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #entry:
          n0:*PyGlobals::dummy = load &globals
          n1:*PyLocals = load &locals
          n2 = dummy.C(n0, n1)
          ret n2

    }

    type .final closure:dummy:2 = {}

    define .closure_wrapper closure:dummy:2.call(__this: *closure:dummy:2, globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #entry:
          n0:*PyGlobals::dummy = load &globals
          n1:*PyLocals = load &locals
          n2 = dummy.D(n0, n1)
          ret n2

    }

    type .final closure:dummy:1 = {}

    define .closure_wrapper .async closure:dummy:1.call(__this: *closure:dummy:1, globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #entry:
          n0:*PyGlobals::dummy = load &globals
          n1:*PyLocals = load &locals
          n2 = dummy.g(n0, n1)
          ret n2

    }

    type .final closure:dummy:0 = {}

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
          n8:*PyGlobals::dummy = load &globals
          store &locals <- n8:*PyGlobals::dummy
          n9 = $builtins.py_make_none()
          n10 = $builtins.py_make_int(0)
          n0 = $builtins.py_import_name("random", n9, n10)
          n11:*PyLocals = load &locals
          n12:*PyGlobals::dummy = load &globals
          n13 = $builtins.py_store_name("random", n11, n12, n0)
          n14 = $builtins.py_make_none()
          n15 = $builtins.py_make_int(0)
          n1 = $builtins.py_import_name("asyncio", n14, n15)
          n16:*PyLocals = load &locals
          n17:*PyGlobals::dummy = load &globals
          n18 = $builtins.py_store_name("a", n16, n17, n1)
          n19:*PyLocals = load &locals
          n20:*PyGlobals::dummy = load &globals
          n21 = $builtins.py_make_int(0)
          n22 = $builtins.py_store_name("x", n19, n20, n21)
          n23 = __sil_allocate(<closure:dummy:0>)
          n25 = $builtins.py_make_none()
          n26 = $builtins.py_make_none()
          n27 = $builtins.py_make_none()
          n28 = $builtins.py_make_none()
          n2 = $builtins.py_make_function(n23, n25, n26, n27, n28)
          n29:*PyLocals = load &locals
          n30:*PyGlobals::dummy = load &globals
          n31 = $builtins.py_store_name("f", n29, n30, n2)
          n32 = __sil_allocate(<closure:dummy:1>)
          n34 = $builtins.py_make_none()
          n35 = $builtins.py_make_none()
          n36 = $builtins.py_make_none()
          n37 = $builtins.py_make_none()
          n3 = $builtins.py_make_function(n32, n34, n35, n36, n37)
          n38:*PyLocals = load &locals
          n39:*PyGlobals::dummy = load &globals
          n40 = $builtins.py_store_name("g", n38, n39, n3)
          n41 = __sil_allocate(<closure:dummy:2>)
          n43 = $builtins.py_make_none()
          n44 = $builtins.py_make_none()
          n45 = $builtins.py_make_none()
          n46 = $builtins.py_make_none()
          n4 = $builtins.py_make_function(n41, n43, n44, n45, n46)
          n47:*PyGlobals::dummy = load &globals
          n48 = $builtins.py_make_string("D")
          n5 = $builtins.py_build_class(n47, n4, n48)
          n49:*PyLocals = load &locals
          n50:*PyGlobals::dummy = load &globals
          n51 = $builtins.py_store_name("D", n49, n50, n5)
          n52 = __sil_allocate(<closure:dummy:3>)
          n54 = $builtins.py_make_none()
          n55 = $builtins.py_make_none()
          n56 = $builtins.py_make_none()
          n57 = $builtins.py_make_none()
          n6 = $builtins.py_make_function(n52, n54, n55, n56, n57)
          n58:*PyGlobals::dummy = load &globals
          n59 = $builtins.py_make_string("C")
          n7 = $builtins.py_build_class(n58, n6, n59)
          n60:*PyLocals = load &locals
          n61:*PyGlobals::dummy = load &globals
          n62 = $builtins.py_store_name("C", n60, n61, n7)
          n63 = $builtins.py_make_none()
          ret n63

    }

    define dummy.C(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n2:*PyLocals = load &locals
          n3:*PyGlobals::dummy = load &globals
          n0 = $builtins.py_load_name("__name__", n2, n3)
          n4:*PyLocals = load &locals
          n5:*PyGlobals::dummy = load &globals
          n6 = $builtins.py_store_name("__module__", n4, n5, n0)
          n7:*PyLocals = load &locals
          n8:*PyGlobals::dummy = load &globals
          n9 = $builtins.py_make_string("C")
          n10 = $builtins.py_store_name("__qualname__", n7, n8, n9)
          n11 = __sil_allocate(<closure:dummy:4>)
          n13 = $builtins.py_make_none()
          n14 = $builtins.py_make_none()
          n15 = $builtins.py_make_none()
          n16 = $builtins.py_make_none()
          n1 = $builtins.py_make_function(n11, n13, n14, n15, n16)
          n17:*PyLocals = load &locals
          n18:*PyGlobals::dummy = load &globals
          n19 = $builtins.py_store_name("foo", n17, n18, n1)
          n20 = $builtins.py_make_none()
          ret n20

    }

    define dummy.C::foo(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n0 = $builtins.py_make_none()
          ret n0

    }

    define dummy.D(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n2:*PyLocals = load &locals
          n3:*PyGlobals::dummy = load &globals
          n0 = $builtins.py_load_name("__name__", n2, n3)
          n4:*PyLocals = load &locals
          n5:*PyGlobals::dummy = load &globals
          n6 = $builtins.py_store_name("__module__", n4, n5, n0)
          n7:*PyLocals = load &locals
          n8:*PyGlobals::dummy = load &globals
          n9 = $builtins.py_make_string("D")
          n10 = $builtins.py_store_name("__qualname__", n7, n8, n9)
          n11 = __sil_allocate(<closure:dummy:5>)
          n13 = $builtins.py_make_none()
          n14 = $builtins.py_make_none()
          n15 = $builtins.py_make_none()
          n16 = $builtins.py_make_none()
          n1 = $builtins.py_make_function(n11, n13, n14, n15, n16)
          n17:*PyLocals = load &locals
          n18:*PyGlobals::dummy = load &globals
          n19 = $builtins.py_store_name("foo", n17, n18, n1)
          n20 = $builtins.py_make_none()
          ret n20

    }

    define dummy.D::foo(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n0 = $builtins.py_make_none()
          ret n0

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
