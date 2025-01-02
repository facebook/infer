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
    TRANSFORMATION PyIR -> Textual
    .source_language = "python"

    define dummy.__module_body__(globals: *PyGlobals::dummy) : *PyObject {
      local locals: *PyLocals
      #b0:
          n2 = globals
          store &locals <- n2
          n1 = locals
          n0 = $builtins.py_make_none()
          n3 = $builtins.py_import_name(n2, "random", n0, $builtins.py_make_int(0))
          _ = $builtins.py_store_name("random", n1, n2, n3)
          n4 = $builtins.py_import_name(n2, "asyncio", n0, $builtins.py_make_int(0))
          _ = $builtins.py_store_name("a", n1, n2, n4)
          n5 = $builtins.py_import_name(n2, "dir1::dir2::mod", $builtins.py_build_tuple($builtins.py_make_string("x")), $builtins.py_make_int(0))
          n6 = $builtins.py_import_from("x", n5)
          _ = $builtins.py_store_name("y", n1, n2, n6)
          n7 = $builtins.py_import_name(n2, "dir1::dir2", $builtins.py_build_tuple($builtins.py_make_string("mod")), $builtins.py_make_int(0))
          n8 = $builtins.py_import_from("mod", n7)
          _ = $builtins.py_store_name("mod", n1, n2, n8)
          _ = $builtins.py_store_name("x", n1, n2, $builtins.py_make_int(0))
          n9 = $builtins.py_make_function(fun (locals) -> dummy.f(n2, locals), n0, n0, n0, n0)
          _ = $builtins.py_store_name("f", n1, n2, n9)
          n10 = $builtins.py_make_function(fun (locals) -> dummy.g(n2, locals), n0, n0, n0, n0)
          _ = $builtins.py_store_name("g", n1, n2, n10)
          n11 = $builtins.py_make_function(fun (locals) -> dummy.D(n2, locals), n0, n0, n0, n0)
          n12 = $builtins.py_build_class(n11, $builtins.py_make_string("D"))
          _ = $builtins.py_store_name("D", n1, n2, n12)
          n13 = $builtins.py_make_function(fun (locals) -> dummy.C(n2, locals), n0, n0, n0, n0)
          n14 = $builtins.py_build_class(n13, $builtins.py_make_string("C"))
          _ = $builtins.py_store_name("C", n1, n2, n14)
          ret n0

    }

    define dummy.C(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n2 = globals
          n1 = locals
          n0 = $builtins.py_make_none()
          n3 = $builtins.py_load_name("__name__", n1, n2)
          _ = $builtins.py_store_name("__module__", n1, n2, n3)
          _ = $builtins.py_store_name("__qualname__", n1, n2, $builtins.py_make_string("C"))
          n4 = $builtins.py_make_function(fun (locals) -> dummy.C::foo(n2, locals), n0, n0, n0, n0)
          _ = $builtins.py_store_name("foo", n1, n2, n4)
          ret n0

    }

    define dummy.C::foo(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n2 = globals
          n1 = locals
          n0 = $builtins.py_make_none()
          ret n0

    }

    define dummy.D(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n2 = globals
          n1 = locals
          n0 = $builtins.py_make_none()
          n3 = $builtins.py_load_name("__name__", n1, n2)
          _ = $builtins.py_store_name("__module__", n1, n2, n3)
          _ = $builtins.py_store_name("__qualname__", n1, n2, $builtins.py_make_string("D"))
          n4 = $builtins.py_make_function(fun (locals) -> dummy.D::foo(n2, locals), n0, n0, n0, n0)
          _ = $builtins.py_store_name("foo", n1, n2, n4)
          ret n0

    }

    define dummy.D::foo(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n2 = globals
          n1 = locals
          n0 = $builtins.py_make_none()
          ret n0

    }

    define .args = "y,l" dummy.f(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n2 = globals
          n1 = locals
          n0 = $builtins.py_make_none()
          n3 = $builtins.py_load_fast("y", n1)
          if n3 then jmp b1 else jmp b2

      #b1:
          n13 = $builtins.py_load_global("g", n2)
          n14 = $builtins.py_load_fast("y", n1)
          n15 = $builtins.py_call(n13, n0, $builtins.py_make_int(0), n14)
          _ = $builtins.py_nullify_locals(n1, "i")
          ret n0

      #b2:
          n4 = $builtins.py_load_fast("l", n1)
          n5 = $builtins.py_get_iter(n4)
          jmp b3

      #b3:
          n6 = $builtins.py_next_iter(n5)
          n7 = $builtins.py_has_next_iter(n5)
          if n7 then jmp b4 else jmp b5

      #b4:
          _ = $builtins.py_store_fast("i", n1, n6)
          n10 = $builtins.py_load_global("print", n2)
          n11 = $builtins.py_load_fast("i", n1)
          n12 = $builtins.py_call(n10, n0, n11)
          jmp b3

      #b5:
          n8 = $builtins.py_load_global("done", n2)
          n9 = $builtins.py_call(n8, n0)
          _ = $builtins.py_nullify_locals(n1, "i")
          ret n0

    }

    define .async dummy.g(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n2 = globals
          n1 = locals
          n0 = $builtins.py_make_none()
          _ = $builtins.py_gen_start_coroutine()
          n3 = $builtins.py_load_global("sleep", n2)
          n4 = $builtins.py_call(n3, n0, $builtins.py_make_int(1))
          n5 = $builtins.py_get_awaitable(n4)
          n6 = $builtins.py_yield_from(n5, n0)
          ret n0

    }

    TYPE INFERENCE
    .source_language = "python"

    define dummy.__module_body__(globals: *PyGlobals::dummy) : *PyObject {
      local locals: *PyLocals
      #b0:
          n2 = [&globals:*PyGlobals::dummy]
          store &locals <- n2:*PyGlobals::dummy
          n1 = [&locals:*PyLocals]
          n0 = $builtins.py_make_none()
          n3 = $builtins.py_import_name(n2, "random", n0, $builtins.py_make_int(0))
          _ = $builtins.py_store_name("random", n1, n2, n3)
          n4 = $builtins.py_import_name(n2, "asyncio", n0, $builtins.py_make_int(0))
          _ = $builtins.py_store_name("a", n1, n2, n4)
          n5 = $builtins.py_import_name(n2, "dir1::dir2::mod", $builtins.py_build_tuple($builtins.py_make_string("x")), $builtins.py_make_int(0))
          n6 = $builtins.py_import_from("x", n5)
          _ = $builtins.py_store_name("y", n1, n2, n6)
          n7 = $builtins.py_import_name(n2, "dir1::dir2", $builtins.py_build_tuple($builtins.py_make_string("mod")), $builtins.py_make_int(0))
          n8 = $builtins.py_import_from("mod", n7)
          _ = $builtins.py_store_name("mod", n1, n2, n8)
          _ = $builtins.py_store_name("x", n1, n2, $builtins.py_make_int(0))
          n9 = $builtins.py_make_function(fun (locals) -> dummy.f(n2, locals), n0, n0, n0, n0)
          _ = $builtins.py_store_name("f", n1, n2, n9)
          n10 = $builtins.py_make_function(fun (locals) -> dummy.g(n2, locals), n0, n0, n0, n0)
          _ = $builtins.py_store_name("g", n1, n2, n10)
          n11 = $builtins.py_make_function(fun (locals) -> dummy.D(n2, locals), n0, n0, n0, n0)
          n12 = $builtins.py_build_class(n11, $builtins.py_make_string("D"))
          _ = $builtins.py_store_name("D", n1, n2, n12)
          n13 = $builtins.py_make_function(fun (locals) -> dummy.C(n2, locals), n0, n0, n0, n0)
          n14 = $builtins.py_build_class(n13, $builtins.py_make_string("C"))
          _ = $builtins.py_store_name("C", n1, n2, n14)
          ret n0

    }

    define dummy.C(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n2 = [&globals:*PyGlobals::dummy]
          n1 = [&locals:*PyLocals]
          n0 = $builtins.py_make_none()
          n3 = $builtins.py_load_name("__name__", n1, n2)
          _ = $builtins.py_store_name("__module__", n1, n2, n3)
          _ = $builtins.py_store_name("__qualname__", n1, n2, $builtins.py_make_string("C"))
          n4 = $builtins.py_make_function(fun (locals) -> dummy.C::foo(n2, locals), n0, n0, n0, n0)
          _ = $builtins.py_store_name("foo", n1, n2, n4)
          ret n0

    }

    define dummy.C::foo(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n2 = [&globals:*PyGlobals::dummy]
          n1 = [&locals:*PyLocals]
          n0 = $builtins.py_make_none()
          ret n0

    }

    define dummy.D(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n2 = [&globals:*PyGlobals::dummy]
          n1 = [&locals:*PyLocals]
          n0 = $builtins.py_make_none()
          n3 = $builtins.py_load_name("__name__", n1, n2)
          _ = $builtins.py_store_name("__module__", n1, n2, n3)
          _ = $builtins.py_store_name("__qualname__", n1, n2, $builtins.py_make_string("D"))
          n4 = $builtins.py_make_function(fun (locals) -> dummy.D::foo(n2, locals), n0, n0, n0, n0)
          _ = $builtins.py_store_name("foo", n1, n2, n4)
          ret n0

    }

    define dummy.D::foo(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n2 = [&globals:*PyGlobals::dummy]
          n1 = [&locals:*PyLocals]
          n0 = $builtins.py_make_none()
          ret n0

    }

    define .args = "y,l" dummy.f(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n2 = [&globals:*PyGlobals::dummy]
          n1 = [&locals:*PyLocals]
          n0 = $builtins.py_make_none()
          n3 = $builtins.py_load_fast("y", n1)
          if n3 then jmp b1 else jmp b2

      #b1:
          n13 = $builtins.py_load_global("g", n2)
          n14 = $builtins.py_load_fast("y", n1)
          n15 = $builtins.py_call(n13, n0, $builtins.py_make_int(0), n14)
          _ = $builtins.py_nullify_locals(n1, "i")
          ret n0

      #b2:
          n4 = $builtins.py_load_fast("l", n1)
          n5 = $builtins.py_get_iter(n4)
          jmp b3

      #b3:
          n6 = $builtins.py_next_iter(n5)
          n7 = $builtins.py_has_next_iter(n5)
          if n7 then jmp b4 else jmp b5

      #b4:
          _ = $builtins.py_store_fast("i", n1, n6)
          n10 = $builtins.py_load_global("print", n2)
          n11 = $builtins.py_load_fast("i", n1)
          n12 = $builtins.py_call(n10, n0, n11)
          jmp b3

      #b5:
          n8 = $builtins.py_load_global("done", n2)
          n9 = $builtins.py_call(n8, n0)
          _ = $builtins.py_nullify_locals(n1, "i")
          ret n0

    }

    define .async dummy.g(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n2 = [&globals:*PyGlobals::dummy]
          n1 = [&locals:*PyLocals]
          n0 = $builtins.py_make_none()
          _ = $builtins.py_gen_start_coroutine()
          n3 = $builtins.py_load_global("sleep", n2)
          n4 = $builtins.py_call(n3, n0, $builtins.py_make_int(1))
          n5 = $builtins.py_get_awaitable(n4)
          n6 = $builtins.py_yield_from(n5, n0)
          ret n0

    }

    FINAL TRANSFORMATIONS
    .source_language = "python"

    type PyGlobals::dummy = {random: *PyGlobals::random; a: *PyGlobals::asyncio;
                             y: *PyModuleAttr::dir1::dir2::mod::x; mod: *PyModuleAttr::dir1::dir2::mod;
                             f: *closure:dummy:0; g: *closure:dummy:1; D: *PyClassCompanion::dummy::D;
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
          n15:*PyGlobals::dummy = load &globals
          store &locals <- n15:*PyGlobals::dummy
          n16:*PyLocals = load &locals
          n0 = $builtins.py_make_none()
          n17 = $builtins.py_make_int(0)
          n3 = $builtins.py_import_name(n15, "random", n0, n17)
          n18 = $builtins.py_store_name("random", n16, n15, n3)
          n19 = $builtins.py_make_int(0)
          n4 = $builtins.py_import_name(n15, "asyncio", n0, n19)
          n20 = $builtins.py_store_name("a", n16, n15, n4)
          n21 = $builtins.py_make_string("x")
          n22 = $builtins.py_build_tuple(n21)
          n23 = $builtins.py_make_int(0)
          n5 = $builtins.py_import_name(n15, "dir1::dir2::mod", n22, n23)
          n6 = $builtins.py_import_from("x", n5)
          n24 = $builtins.py_store_name("y", n16, n15, n6)
          n25 = $builtins.py_make_string("mod")
          n26 = $builtins.py_build_tuple(n25)
          n27 = $builtins.py_make_int(0)
          n7 = $builtins.py_import_name(n15, "dir1::dir2", n26, n27)
          n8 = $builtins.py_import_from("mod", n7)
          n28 = $builtins.py_store_name("mod", n16, n15, n8)
          n29 = $builtins.py_make_int(0)
          n30 = $builtins.py_store_name("x", n16, n15, n29)
          n31 = __sil_allocate(<closure:dummy:0>)
          store n31.?.globals <- n15:*PyGlobals::dummy
          n9 = $builtins.py_make_function(n31, n0, n0, n0, n0)
          n33 = $builtins.py_store_name("f", n16, n15, n9)
          n34 = __sil_allocate(<closure:dummy:1>)
          store n34.?.globals <- n15:*PyGlobals::dummy
          n10 = $builtins.py_make_function(n34, n0, n0, n0, n0)
          n36 = $builtins.py_store_name("g", n16, n15, n10)
          n37 = __sil_allocate(<closure:dummy:2>)
          store n37.?.globals <- n15:*PyGlobals::dummy
          n11 = $builtins.py_make_function(n37, n0, n0, n0, n0)
          n39 = $builtins.py_make_string("D")
          n12 = $builtins.py_build_class(n11, n39)
          n40 = $builtins.py_store_name("D", n16, n15, n12)
          n41 = __sil_allocate(<closure:dummy:3>)
          store n41.?.globals <- n15:*PyGlobals::dummy
          n13 = $builtins.py_make_function(n41, n0, n0, n0, n0)
          n43 = $builtins.py_make_string("C")
          n14 = $builtins.py_build_class(n13, n43)
          n44 = $builtins.py_store_name("C", n16, n15, n14)
          ret n0

    }

    define dummy.C(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n5:*PyGlobals::dummy = load &globals
          n6:*PyLocals = load &locals
          n0 = $builtins.py_make_none()
          n3 = $builtins.py_load_name("__name__", n6, n5)
          n7 = $builtins.py_store_name("__module__", n6, n5, n3)
          n8 = $builtins.py_make_string("C")
          n9 = $builtins.py_store_name("__qualname__", n6, n5, n8)
          n10 = __sil_allocate(<closure:dummy:4>)
          store n10.?.globals <- n5:*PyGlobals::dummy
          n4 = $builtins.py_make_function(n10, n0, n0, n0, n0)
          n12 = $builtins.py_store_name("foo", n6, n5, n4)
          ret n0

    }

    define dummy.C::foo(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n3:*PyGlobals::dummy = load &globals
          n4:*PyLocals = load &locals
          n0 = $builtins.py_make_none()
          ret n0

    }

    define dummy.D(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n5:*PyGlobals::dummy = load &globals
          n6:*PyLocals = load &locals
          n0 = $builtins.py_make_none()
          n3 = $builtins.py_load_name("__name__", n6, n5)
          n7 = $builtins.py_store_name("__module__", n6, n5, n3)
          n8 = $builtins.py_make_string("D")
          n9 = $builtins.py_store_name("__qualname__", n6, n5, n8)
          n10 = __sil_allocate(<closure:dummy:5>)
          store n10.?.globals <- n5:*PyGlobals::dummy
          n4 = $builtins.py_make_function(n10, n0, n0, n0, n0)
          n12 = $builtins.py_store_name("foo", n6, n5, n4)
          ret n0

    }

    define dummy.D::foo(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n3:*PyGlobals::dummy = load &globals
          n4:*PyLocals = load &locals
          n0 = $builtins.py_make_none()
          ret n0

    }

    define .args = "y,l" dummy.f(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n16:*PyGlobals::dummy = load &globals
          n17:*PyLocals = load &locals
          n0 = $builtins.py_make_none()
          n3 = $builtins.py_load_fast("y", n17)
          jmp b1, b2

      #b1:
          prune n3
          n13 = $builtins.py_load_global("g", n16)
          n14 = $builtins.py_load_fast("y", n17)
          n18 = $builtins.py_make_int(0)
          n15 = $builtins.py_call(n13, n0, n18, n14)
          n19 = $builtins.py_nullify_locals(n17, "i")
          ret n0

      #b2:
          prune __sil_lnot(n3)
          n4 = $builtins.py_load_fast("l", n17)
          n5 = $builtins.py_get_iter(n4)
          jmp b3

      #b3:
          n6 = $builtins.py_next_iter(n5)
          n7 = $builtins.py_has_next_iter(n5)
          jmp b4, b5

      #b4:
          prune n7
          n20 = $builtins.py_store_fast("i", n17, n6)
          n10 = $builtins.py_load_global("print", n16)
          n11 = $builtins.py_load_fast("i", n17)
          n12 = $builtins.py_call(n10, n0, n11)
          jmp b3

      #b5:
          prune __sil_lnot(n7)
          n8 = $builtins.py_load_global("done", n16)
          n9 = $builtins.py_call(n8, n0)
          n21 = $builtins.py_nullify_locals(n17, "i")
          ret n0

    }

    define .async dummy.g(globals: *PyGlobals::dummy, locals: *PyLocals) : *PyObject {
      #b0:
          n7:*PyGlobals::dummy = load &globals
          n8:*PyLocals = load &locals
          n0 = $builtins.py_make_none()
          n9 = $builtins.py_gen_start_coroutine()
          n3 = $builtins.py_load_global("sleep", n7)
          n10 = $builtins.py_make_int(1)
          n4 = $builtins.py_call(n3, n0, n10)
          n5 = $builtins.py_get_awaitable(n4)
          n6 = $builtins.py_yield_from(n5, n0)
          ret n0

    } |}]
