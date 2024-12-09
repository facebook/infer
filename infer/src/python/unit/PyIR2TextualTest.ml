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
          n2 = $builtins.py_make_function(fun (locals) -> dummy.f(globals, locals), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none())
          _ = $builtins.py_store_name("f", locals, globals, n2)
          n3 = $builtins.py_make_function(fun (locals) -> dummy.g(globals, locals), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none())
          _ = $builtins.py_store_name("g", locals, globals, n3)
          n4 = $builtins.py_make_function(fun (locals) -> dummy.D(globals, locals), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none())
          n5 = $builtins.py_build_class(n4, $builtins.py_make_string("D"))
          _ = $builtins.py_store_name("D", locals, globals, n5)
          n6 = $builtins.py_make_function(fun (locals) -> dummy.C(globals, locals), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none())
          n7 = $builtins.py_build_class(n6, $builtins.py_make_string("C"))
          _ = $builtins.py_store_name("C", locals, globals, n7)
          ret $builtins.py_make_none()

    }

    define dummy.C(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n0 = $builtins.py_load_name("__name__", locals, globals)
          _ = $builtins.py_store_name("__module__", locals, globals, n0)
          _ = $builtins.py_store_name("__qualname__", locals, globals, $builtins.py_make_string("C"))
          n1 = $builtins.py_make_function(fun (locals) -> dummy.C::foo(globals, locals), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none())
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
          n1 = $builtins.py_make_function(fun (locals) -> dummy.D::foo(globals, locals), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none())
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
          n12 = $builtins.py_call(n10, $builtins.py_make_none(), $builtins.py_make_int(0), n11)
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
          n9 = $builtins.py_call(n7, $builtins.py_make_none(), n8)
          jmp b3

      #b5:
          n5 = $builtins.py_load_global("done", globals)
          n6 = $builtins.py_call(n5, $builtins.py_make_none())
          _ = $builtins.py_nullify_locals(locals, "i")
          ret $builtins.py_make_none()

    }

    define .async dummy.g(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          _ = $builtins.py_gen_start_coroutine()
          n0 = $builtins.py_load_global("sleep", globals)
          n1 = $builtins.py_call(n0, $builtins.py_make_none(), $builtins.py_make_int(1))
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
          n2 = $builtins.py_make_function(fun (locals) -> dummy.f([&globals:*PyGlobals::dummy], locals), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none())
          _ = $builtins.py_store_name("f", [&locals:*PyLocals], [&globals:*PyGlobals::dummy], n2)
          n3 = $builtins.py_make_function(fun (locals) -> dummy.g([&globals:*PyGlobals::dummy], locals), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none())
          _ = $builtins.py_store_name("g", [&locals:*PyLocals], [&globals:*PyGlobals::dummy], n3)
          n4 = $builtins.py_make_function(fun (locals) -> dummy.D([&globals:*PyGlobals::dummy], locals), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none())
          n5 = $builtins.py_build_class(n4, $builtins.py_make_string("D"))
          _ = $builtins.py_store_name("D", [&locals:*PyLocals], [&globals:*PyGlobals::dummy], n5)
          n6 = $builtins.py_make_function(fun (locals) -> dummy.C([&globals:*PyGlobals::dummy], locals), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none())
          n7 = $builtins.py_build_class(n6, $builtins.py_make_string("C"))
          _ = $builtins.py_store_name("C", [&locals:*PyLocals], [&globals:*PyGlobals::dummy], n7)
          ret $builtins.py_make_none()

    }

    define dummy.C(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n0 = $builtins.py_load_name("__name__", [&locals:*PyLocals], [&globals:*PyGlobals::dummy])
          _ = $builtins.py_store_name("__module__", [&locals:*PyLocals], [&globals:*PyGlobals::dummy], n0)
          _ = $builtins.py_store_name("__qualname__", [&locals:*PyLocals], [&globals:*PyGlobals::dummy], $builtins.py_make_string("C"))
          n1 = $builtins.py_make_function(fun (locals) -> dummy.C::foo([&globals:*PyGlobals::dummy], locals), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none())
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
          n1 = $builtins.py_make_function(fun (locals) -> dummy.D::foo([&globals:*PyGlobals::dummy], locals), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none(), $builtins.py_make_none())
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
          n12 = $builtins.py_call(n10, $builtins.py_make_none(), $builtins.py_make_int(0), n11)
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
          n9 = $builtins.py_call(n7, $builtins.py_make_none(), n8)
          jmp b3

      #b5:
          n5 = $builtins.py_load_global("done", [&globals:*PyGlobals::dummy])
          n6 = $builtins.py_call(n5, $builtins.py_make_none())
          _ = $builtins.py_nullify_locals([&locals:*PyLocals], "i")
          ret $builtins.py_make_none()

    }

    define .async dummy.g(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          _ = $builtins.py_gen_start_coroutine()
          n0 = $builtins.py_load_global("sleep", [&globals:*PyGlobals::dummy])
          n1 = $builtins.py_call(n0, $builtins.py_make_none(), $builtins.py_make_int(1))
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

    type .final closure:dummy:5 = {globals: *PyGlobals::dummy}

    define .closure_wrapper closure:dummy:5.call(__this: *closure:dummy:5, locals: *PyLocals) : *PyObject {
      #entry:
          n0:*closure:dummy:5 = load &__this
          n1:*PyGlobals::dummy = load n0.?.globals
          n2:*PyLocals = load &locals
          n3 = dummy.D::foo(n1, n2)
          ret n3

    }

    type .final closure:dummy:4 = {globals: *PyGlobals::dummy}

    define .closure_wrapper closure:dummy:4.call(__this: *closure:dummy:4, locals: *PyLocals) : *PyObject {
      #entry:
          n0:*closure:dummy:4 = load &__this
          n1:*PyGlobals::dummy = load n0.?.globals
          n2:*PyLocals = load &locals
          n3 = dummy.C::foo(n1, n2)
          ret n3

    }

    type .final closure:dummy:3 = {globals: *PyGlobals::dummy}

    define .closure_wrapper closure:dummy:3.call(__this: *closure:dummy:3, locals: *PyLocals) : *PyObject {
      #entry:
          n0:*closure:dummy:3 = load &__this
          n1:*PyGlobals::dummy = load n0.?.globals
          n2:*PyLocals = load &locals
          n3 = dummy.C(n1, n2)
          ret n3

    }

    type .final closure:dummy:2 = {globals: *PyGlobals::dummy}

    define .closure_wrapper closure:dummy:2.call(__this: *closure:dummy:2, locals: *PyLocals) : *PyObject {
      #entry:
          n0:*closure:dummy:2 = load &__this
          n1:*PyGlobals::dummy = load n0.?.globals
          n2:*PyLocals = load &locals
          n3 = dummy.D(n1, n2)
          ret n3

    }

    type .final closure:dummy:1 = {globals: *PyGlobals::dummy}

    define .closure_wrapper .async closure:dummy:1.call(__this: *closure:dummy:1, locals: *PyLocals) : *PyObject {
      #entry:
          n0:*closure:dummy:1 = load &__this
          n1:*PyGlobals::dummy = load n0.?.globals
          n2:*PyLocals = load &locals
          n3 = dummy.g(n1, n2)
          ret n3

    }

    type .final closure:dummy:0 = {globals: *PyGlobals::dummy}

    define .closure_wrapper .args = "y,l" closure:dummy:0.call(__this: *closure:dummy:0, locals: *PyLocals) : *PyObject {
      #entry:
          n0:*closure:dummy:0 = load &__this
          n1:*PyGlobals::dummy = load n0.?.globals
          n2:*PyLocals = load &locals
          n3 = dummy.f(n1, n2)
          ret n3

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
          n23:*PyGlobals::dummy = load &globals
          n24 = __sil_allocate(<closure:dummy:0>)
          store n24.?.globals <- n23:*PyGlobals::dummy
          n26 = $builtins.py_make_none()
          n27 = $builtins.py_make_none()
          n28 = $builtins.py_make_none()
          n29 = $builtins.py_make_none()
          n2 = $builtins.py_make_function(n24, n26, n27, n28, n29)
          n30:*PyLocals = load &locals
          n31:*PyGlobals::dummy = load &globals
          n32 = $builtins.py_store_name("f", n30, n31, n2)
          n33:*PyGlobals::dummy = load &globals
          n34 = __sil_allocate(<closure:dummy:1>)
          store n34.?.globals <- n33:*PyGlobals::dummy
          n36 = $builtins.py_make_none()
          n37 = $builtins.py_make_none()
          n38 = $builtins.py_make_none()
          n39 = $builtins.py_make_none()
          n3 = $builtins.py_make_function(n34, n36, n37, n38, n39)
          n40:*PyLocals = load &locals
          n41:*PyGlobals::dummy = load &globals
          n42 = $builtins.py_store_name("g", n40, n41, n3)
          n43:*PyGlobals::dummy = load &globals
          n44 = __sil_allocate(<closure:dummy:2>)
          store n44.?.globals <- n43:*PyGlobals::dummy
          n46 = $builtins.py_make_none()
          n47 = $builtins.py_make_none()
          n48 = $builtins.py_make_none()
          n49 = $builtins.py_make_none()
          n4 = $builtins.py_make_function(n44, n46, n47, n48, n49)
          n50 = $builtins.py_make_string("D")
          n5 = $builtins.py_build_class(n4, n50)
          n51:*PyLocals = load &locals
          n52:*PyGlobals::dummy = load &globals
          n53 = $builtins.py_store_name("D", n51, n52, n5)
          n54:*PyGlobals::dummy = load &globals
          n55 = __sil_allocate(<closure:dummy:3>)
          store n55.?.globals <- n54:*PyGlobals::dummy
          n57 = $builtins.py_make_none()
          n58 = $builtins.py_make_none()
          n59 = $builtins.py_make_none()
          n60 = $builtins.py_make_none()
          n6 = $builtins.py_make_function(n55, n57, n58, n59, n60)
          n61 = $builtins.py_make_string("C")
          n7 = $builtins.py_build_class(n6, n61)
          n62:*PyLocals = load &locals
          n63:*PyGlobals::dummy = load &globals
          n64 = $builtins.py_store_name("C", n62, n63, n7)
          n65 = $builtins.py_make_none()
          ret n65

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
          n11:*PyGlobals::dummy = load &globals
          n12 = __sil_allocate(<closure:dummy:4>)
          store n12.?.globals <- n11:*PyGlobals::dummy
          n14 = $builtins.py_make_none()
          n15 = $builtins.py_make_none()
          n16 = $builtins.py_make_none()
          n17 = $builtins.py_make_none()
          n1 = $builtins.py_make_function(n12, n14, n15, n16, n17)
          n18:*PyLocals = load &locals
          n19:*PyGlobals::dummy = load &globals
          n20 = $builtins.py_store_name("foo", n18, n19, n1)
          n21 = $builtins.py_make_none()
          ret n21

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
          n11:*PyGlobals::dummy = load &globals
          n12 = __sil_allocate(<closure:dummy:5>)
          store n12.?.globals <- n11:*PyGlobals::dummy
          n14 = $builtins.py_make_none()
          n15 = $builtins.py_make_none()
          n16 = $builtins.py_make_none()
          n17 = $builtins.py_make_none()
          n1 = $builtins.py_make_function(n12, n14, n15, n16, n17)
          n18:*PyLocals = load &locals
          n19:*PyGlobals::dummy = load &globals
          n20 = $builtins.py_store_name("foo", n18, n19, n1)
          n21 = $builtins.py_make_none()
          ret n21

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
          n16 = $builtins.py_make_none()
          n17 = $builtins.py_make_int(0)
          n12 = $builtins.py_call(n10, n16, n17, n11)
          n18:*PyLocals = load &locals
          n19 = $builtins.py_nullify_locals(n18, "i")
          n20 = $builtins.py_make_none()
          ret n20

      #b2:
          prune __sil_lnot(n0)
          n21:*PyLocals = load &locals
          n1 = $builtins.py_load_fast("l", n21)
          n2 = $builtins.py_get_iter(n1)
          jmp b3

      #b3:
          n3 = $builtins.py_next_iter(n2)
          n4 = $builtins.py_has_next_iter(n2)
          jmp b4, b5

      #b4:
          prune n4
          n22:*PyLocals = load &locals
          n23 = $builtins.py_store_fast("i", n22, n3)
          n24:*PyGlobals::dummy = load &globals
          n7 = $builtins.py_load_global("print", n24)
          n25:*PyLocals = load &locals
          n8 = $builtins.py_load_fast("i", n25)
          n26 = $builtins.py_make_none()
          n9 = $builtins.py_call(n7, n26, n8)
          jmp b3

      #b5:
          prune __sil_lnot(n4)
          n27:*PyGlobals::dummy = load &globals
          n5 = $builtins.py_load_global("done", n27)
          n28 = $builtins.py_make_none()
          n6 = $builtins.py_call(n5, n28)
          n29:*PyLocals = load &locals
          n30 = $builtins.py_nullify_locals(n29, "i")
          n31 = $builtins.py_make_none()
          ret n31

    }

    define .async dummy.g(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n4 = $builtins.py_gen_start_coroutine()
          n5:*PyGlobals::dummy = load &globals
          n0 = $builtins.py_load_global("sleep", n5)
          n6 = $builtins.py_make_none()
          n7 = $builtins.py_make_int(1)
          n1 = $builtins.py_call(n0, n6, n7)
          n2 = $builtins.py_get_awaitable(n1)
          n8 = $builtins.py_make_none()
          n3 = $builtins.py_yield_from(n2, n8)
          n9 = $builtins.py_make_none()
          ret n9

    } |}]
