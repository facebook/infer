(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let run pyir =
  let show module_ = F.printf "%a" (Textual.Module.pp ~show_location:true) module_ in
  let result =
    let open IResult.Let_syntax in
    let textual = PyIR2Textual.mk_module pyir in
    F.printf "TRANSFORMATION PyIR -> Textual@\n" ;
    show textual ;
    let+ verified_textual =
      TextualVerification.verify_strict textual
      |> Result.map_error ~f:(fun err -> `VerificationError err)
    in
    F.printf "TYPE INFERENCE@\n" ;
    show verified_textual ;
    let transformed_textual, _ = TextualTransform.run Python verified_textual in
    let {PyIR.Module.name= module_name} = pyir in
    let transformed_textual =
      PyIRTypeInference.gen_module_default_type pyir
      |> Option.value_map ~default:transformed_textual ~f:(fun pyir_type ->
             PyIR2Textual.add_pyir_type pyir_type ~module_name transformed_textual )
    in
    F.printf "FINAL TRANSFORMATIONS@\n" ;
    show transformed_textual
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

from foo import E

#type inference will currently skip E because it is imported
class B(C, D, E):
    pass
|}
  in
  PyIR.test ~run source ;
  [%expect
    {|
    TRANSFORMATION PyIR -> Textual
    .source_language = "python" @?

    define dummy.__module_body__(globals: *PyGlobals<dummy>) : *PyObject {
      local locals: *PyLocals
      #b0: @2
          n2 = globals @2
          store &locals <- n2 @2
          n1 = locals @2
          n0 = $builtins.py_make_none() @2
          n3 = $builtins.py_import_name(n2, "random", n0, $builtins.py_make_int(0)) @2
          _ = $builtins.py_store_name("random", n1, n2, n3) @2
          jmp b1 @2

      #b1: @3
          n4 = $builtins.py_import_name(n2, "asyncio", n0, $builtins.py_make_int(0)) @3
          _ = $builtins.py_store_name("a", n1, n2, n4) @3
          jmp b2 @3

      #b10: @31
          n17 = $builtins.py_make_function(.name = "dummy.B"fun (locals) -> dummy.B(n2, locals), n0, n0, n0, n0) @31
          n18 = $builtins.py_load_name("C", n1, n2) @31
          n19 = $builtins.py_load_name("D", n1, n2) @31
          n20 = $builtins.py_load_name("E", n1, n2) @31
          n21 = $builtins.py_build_class(n17, $builtins.py_make_string("B"), n18, n19, n20) @31
          _ = $builtins.py_store_name("B", n1, n2, n21) @31
          ret n0 @31

      #b2: @4
          n5 = $builtins.py_import_name(n2, "dir1::dir2::mod", $builtins.py_build_tuple($builtins.py_make_string("x")), $builtins.py_make_int(0)) @4
          n6 = $builtins.py_import_from("x", n5) @4
          _ = $builtins.py_store_name("y", n1, n2, n6) @4
          jmp b3 @4

      #b3: @5
          n7 = $builtins.py_import_name(n2, "dir1::dir2", $builtins.py_build_tuple($builtins.py_make_string("mod")), $builtins.py_make_int(0)) @5
          n8 = $builtins.py_import_from("mod", n7) @5
          _ = $builtins.py_store_name("mod", n1, n2, n8) @5
          jmp b4 @5

      #b4: @7
          _ = $builtins.py_store_name("x", n1, n2, $builtins.py_make_int(0)) @7
          jmp b5 @7

      #b5: @9
          n9 = $builtins.py_make_function(.name = "dummy.f"fun (locals) -> dummy.f(n2, locals), n0, n0, n0, n0) @9
          _ = $builtins.py_store_name("f", n1, n2, n9) @9
          jmp b6 @9

      #b6: @17
          n10 = $builtins.py_make_function(.name = "dummy.g"fun (locals) -> dummy.g(n2, locals), n0, n0, n0, n0) @17
          _ = $builtins.py_store_name("g", n1, n2, n10) @17
          jmp b7 @17

      #b7: @20
          n11 = $builtins.py_make_function(.name = "dummy.D"fun (locals) -> dummy.D(n2, locals), n0, n0, n0, n0) @20
          n12 = $builtins.py_build_class(n11, $builtins.py_make_string("D")) @20
          _ = $builtins.py_store_name("D", n1, n2, n12) @20
          jmp b8 @20

      #b8: @24
          n13 = $builtins.py_make_function(.name = "dummy.C"fun (locals) -> dummy.C(n2, locals), n0, n0, n0, n0) @24
          n14 = $builtins.py_build_class(n13, $builtins.py_make_string("C")) @24
          _ = $builtins.py_store_name("C", n1, n2, n14) @24
          jmp b9 @24

      #b9: @28
          n15 = $builtins.py_import_name(n2, "foo", $builtins.py_build_tuple($builtins.py_make_string("E")), $builtins.py_make_int(0)) @28
          n16 = $builtins.py_import_from("E", n15) @28
          _ = $builtins.py_store_name("E", n1, n2, n16) @28
          jmp b10 @28

    } @31

    define dummy.B(globals: *PyGlobals<dummy>, locals: *PyLocals) : *PyObject {
      #b0: @31
          n2 = globals @31
          n1 = locals @31
          n0 = $builtins.py_make_none() @31
          n3 = $builtins.py_load_name("__name__", n1, n2) @31
          _ = $builtins.py_store_name("__module__", n1, n2, n3) @31
          _ = $builtins.py_store_name("__qualname__", n1, n2, $builtins.py_make_string("B")) @31
          jmp b1 @31

      #b1: @32
          ret n0 @32

    } @32

    define dummy.C(globals: *PyGlobals<dummy>, locals: *PyLocals) : *PyObject {
      #b0: @24
          n2 = globals @24
          n1 = locals @24
          n0 = $builtins.py_make_none() @24
          n3 = $builtins.py_load_name("__name__", n1, n2) @24
          _ = $builtins.py_store_name("__module__", n1, n2, n3) @24
          _ = $builtins.py_store_name("__qualname__", n1, n2, $builtins.py_make_string("C")) @24
          jmp b1 @24

      #b1: @25
          n4 = $builtins.py_make_function(.name = "dummy.C.foo"fun (locals) -> dummy.C::foo(n2, locals), n0, n0, n0, n0) @25
          _ = $builtins.py_store_name("foo", n1, n2, n4) @25
          ret n0 @25

    } @25

    define dummy.C::foo(globals: *PyGlobals<dummy>, locals: *PyLocals) : *PyObject {
      #b0: @26
          n2 = globals @26
          n1 = locals @26
          n0 = $builtins.py_make_none() @26
          ret n0 @26

    } @26

    define dummy.D(globals: *PyGlobals<dummy>, locals: *PyLocals) : *PyObject {
      #b0: @20
          n2 = globals @20
          n1 = locals @20
          n0 = $builtins.py_make_none() @20
          n3 = $builtins.py_load_name("__name__", n1, n2) @20
          _ = $builtins.py_store_name("__module__", n1, n2, n3) @20
          _ = $builtins.py_store_name("__qualname__", n1, n2, $builtins.py_make_string("D")) @20
          jmp b1 @20

      #b1: @21
          n4 = $builtins.py_make_function(.name = "dummy.D.foo"fun (locals) -> dummy.D::foo(n2, locals), n0, n0, n0, n0) @21
          _ = $builtins.py_store_name("foo", n1, n2, n4) @21
          ret n0 @21

    } @21

    define dummy.D::foo(globals: *PyGlobals<dummy>, locals: *PyLocals) : *PyObject {
      #b0: @22
          n2 = globals @22
          n1 = locals @22
          n0 = $builtins.py_make_none() @22
          ret n0 @22

    } @22

    define .args = "y,l" dummy.f(globals: *PyGlobals<dummy>, locals: *PyLocals) : *PyObject {
      #b0: @10
          n2 = globals @10
          n1 = locals @10
          n0 = $builtins.py_make_none() @10
          n3 = $builtins.py_load_fast("y", n1) @10
          if $builtins.py_bool(n3) then jmp b1 else jmp b2 @10

      #b1: @11
          n13 = $builtins.py_load_global("g", n2) @11
          n14 = $builtins.py_load_fast("y", n1) @11
          n15 = $builtins.py_call(n13, n0, $builtins.py_make_int(0), n14) @11
          _ = $builtins.py_nullify_locals(n1, "i") @11
          ret n0 @11

      #b2: @13
          n4 = $builtins.py_load_fast("l", n1) @13
          n5 = $builtins.py_get_iter(n4) @13
          jmp b3 @13

      #b3: @13
          n6 = $builtins.py_next_iter(n5) @13
          n7 = $builtins.py_has_next_iter(n5) @13
          if $builtins.py_bool(n7) then jmp b4 else jmp b6 @13

      #b4: @13
          _ = $builtins.py_store_fast("i", n1, n6) @13
          jmp b5 @13

      #b5: @14
          n10 = $builtins.py_load_global("print", n2) @14
          n11 = $builtins.py_load_fast("i", n1) @14
          n12 = $builtins.py_call(n10, n0, n11) @14
          jmp b3 @14

      #b6: @15
          n8 = $builtins.py_load_global("done", n2) @15
          n9 = $builtins.py_call(n8, n0) @15
          _ = $builtins.py_nullify_locals(n1, "i") @15
          ret n0 @15

    } @15

    define .async dummy.g(globals: *PyGlobals<dummy>, locals: *PyLocals) : *PyObject {
      #b0: @?
          n2 = globals @?
          n1 = locals @?
          n0 = $builtins.py_make_none() @?
          _ = $builtins.py_gen_start_coroutine() @?
          jmp b1 @?

      #b1: @18
          n3 = $builtins.py_load_global("sleep", n2) @18
          n4 = $builtins.py_call(n3, n0, $builtins.py_make_int(1)) @18
          n5 = $builtins.py_get_awaitable(n4) @18
          n6 = $builtins.py_yield_from(n5, n0) @18
          ret n0 @18

    } @18

    TYPE INFERENCE
    .source_language = "python" @?

    define dummy.__module_body__(globals: *PyGlobals<dummy>) : *PyObject {
      local locals: *PyLocals
      #b0: @2
          n2 = [&globals:*PyGlobals<dummy>] @2
          store &locals <- n2:*PyGlobals<dummy> @2
          n1 = [&locals:*PyLocals] @2
          n0 = $builtins.py_make_none() @2
          n3 = $builtins.py_import_name(n2, "random", n0, $builtins.py_make_int(0)) @2
          _ = $builtins.py_store_name("random", n1, n2, n3) @2
          jmp b1 @2

      #b1: @3
          n4 = $builtins.py_import_name(n2, "asyncio", n0, $builtins.py_make_int(0)) @3
          _ = $builtins.py_store_name("a", n1, n2, n4) @3
          jmp b2 @3

      #b10: @31
          n17 = $builtins.py_make_function(.name = "dummy.B"fun (locals) -> dummy.B(n2, locals), n0, n0, n0, n0) @31
          n18 = $builtins.py_load_name("C", n1, n2) @31
          n19 = $builtins.py_load_name("D", n1, n2) @31
          n20 = $builtins.py_load_name("E", n1, n2) @31
          n21 = $builtins.py_build_class(n17, $builtins.py_make_string("B"), n18, n19, n20) @31
          _ = $builtins.py_store_name("B", n1, n2, n21) @31
          ret n0 @31

      #b2: @4
          n5 = $builtins.py_import_name(n2, "dir1::dir2::mod", $builtins.py_build_tuple($builtins.py_make_string("x")), $builtins.py_make_int(0)) @4
          n6 = $builtins.py_import_from("x", n5) @4
          _ = $builtins.py_store_name("y", n1, n2, n6) @4
          jmp b3 @4

      #b3: @5
          n7 = $builtins.py_import_name(n2, "dir1::dir2", $builtins.py_build_tuple($builtins.py_make_string("mod")), $builtins.py_make_int(0)) @5
          n8 = $builtins.py_import_from("mod", n7) @5
          _ = $builtins.py_store_name("mod", n1, n2, n8) @5
          jmp b4 @5

      #b4: @7
          _ = $builtins.py_store_name("x", n1, n2, $builtins.py_make_int(0)) @7
          jmp b5 @7

      #b5: @9
          n9 = $builtins.py_make_function(.name = "dummy.f"fun (locals) -> dummy.f(n2, locals), n0, n0, n0, n0) @9
          _ = $builtins.py_store_name("f", n1, n2, n9) @9
          jmp b6 @9

      #b6: @17
          n10 = $builtins.py_make_function(.name = "dummy.g"fun (locals) -> dummy.g(n2, locals), n0, n0, n0, n0) @17
          _ = $builtins.py_store_name("g", n1, n2, n10) @17
          jmp b7 @17

      #b7: @20
          n11 = $builtins.py_make_function(.name = "dummy.D"fun (locals) -> dummy.D(n2, locals), n0, n0, n0, n0) @20
          n12 = $builtins.py_build_class(n11, $builtins.py_make_string("D")) @20
          _ = $builtins.py_store_name("D", n1, n2, n12) @20
          jmp b8 @20

      #b8: @24
          n13 = $builtins.py_make_function(.name = "dummy.C"fun (locals) -> dummy.C(n2, locals), n0, n0, n0, n0) @24
          n14 = $builtins.py_build_class(n13, $builtins.py_make_string("C")) @24
          _ = $builtins.py_store_name("C", n1, n2, n14) @24
          jmp b9 @24

      #b9: @28
          n15 = $builtins.py_import_name(n2, "foo", $builtins.py_build_tuple($builtins.py_make_string("E")), $builtins.py_make_int(0)) @28
          n16 = $builtins.py_import_from("E", n15) @28
          _ = $builtins.py_store_name("E", n1, n2, n16) @28
          jmp b10 @28

    } @31

    define dummy.B(globals: *PyGlobals<dummy>, locals: *PyLocals) : *PyObject {
      #b0: @31
          n2 = [&globals:*PyGlobals<dummy>] @31
          n1 = [&locals:*PyLocals] @31
          n0 = $builtins.py_make_none() @31
          n3 = $builtins.py_load_name("__name__", n1, n2) @31
          _ = $builtins.py_store_name("__module__", n1, n2, n3) @31
          _ = $builtins.py_store_name("__qualname__", n1, n2, $builtins.py_make_string("B")) @31
          jmp b1 @31

      #b1: @32
          ret n0 @32

    } @32

    define dummy.C(globals: *PyGlobals<dummy>, locals: *PyLocals) : *PyObject {
      #b0: @24
          n2 = [&globals:*PyGlobals<dummy>] @24
          n1 = [&locals:*PyLocals] @24
          n0 = $builtins.py_make_none() @24
          n3 = $builtins.py_load_name("__name__", n1, n2) @24
          _ = $builtins.py_store_name("__module__", n1, n2, n3) @24
          _ = $builtins.py_store_name("__qualname__", n1, n2, $builtins.py_make_string("C")) @24
          jmp b1 @24

      #b1: @25
          n4 = $builtins.py_make_function(.name = "dummy.C.foo"fun (locals) -> dummy.C::foo(n2, locals), n0, n0, n0, n0) @25
          _ = $builtins.py_store_name("foo", n1, n2, n4) @25
          ret n0 @25

    } @25

    define dummy.C::foo(globals: *PyGlobals<dummy>, locals: *PyLocals) : *PyObject {
      #b0: @26
          n2 = [&globals:*PyGlobals<dummy>] @26
          n1 = [&locals:*PyLocals] @26
          n0 = $builtins.py_make_none() @26
          ret n0 @26

    } @26

    define dummy.D(globals: *PyGlobals<dummy>, locals: *PyLocals) : *PyObject {
      #b0: @20
          n2 = [&globals:*PyGlobals<dummy>] @20
          n1 = [&locals:*PyLocals] @20
          n0 = $builtins.py_make_none() @20
          n3 = $builtins.py_load_name("__name__", n1, n2) @20
          _ = $builtins.py_store_name("__module__", n1, n2, n3) @20
          _ = $builtins.py_store_name("__qualname__", n1, n2, $builtins.py_make_string("D")) @20
          jmp b1 @20

      #b1: @21
          n4 = $builtins.py_make_function(.name = "dummy.D.foo"fun (locals) -> dummy.D::foo(n2, locals), n0, n0, n0, n0) @21
          _ = $builtins.py_store_name("foo", n1, n2, n4) @21
          ret n0 @21

    } @21

    define dummy.D::foo(globals: *PyGlobals<dummy>, locals: *PyLocals) : *PyObject {
      #b0: @22
          n2 = [&globals:*PyGlobals<dummy>] @22
          n1 = [&locals:*PyLocals] @22
          n0 = $builtins.py_make_none() @22
          ret n0 @22

    } @22

    define .args = "y,l" dummy.f(globals: *PyGlobals<dummy>, locals: *PyLocals) : *PyObject {
      #b0: @10
          n2 = [&globals:*PyGlobals<dummy>] @10
          n1 = [&locals:*PyLocals] @10
          n0 = $builtins.py_make_none() @10
          n3 = $builtins.py_load_fast("y", n1) @10
          if $builtins.py_bool(n3) then jmp b1 else jmp b2 @10

      #b1: @11
          n13 = $builtins.py_load_global("g", n2) @11
          n14 = $builtins.py_load_fast("y", n1) @11
          n15 = $builtins.py_call(n13, n0, $builtins.py_make_int(0), n14) @11
          _ = $builtins.py_nullify_locals(n1, "i") @11
          ret n0 @11

      #b2: @13
          n4 = $builtins.py_load_fast("l", n1) @13
          n5 = $builtins.py_get_iter(n4) @13
          jmp b3 @13

      #b3: @13
          n6 = $builtins.py_next_iter(n5) @13
          n7 = $builtins.py_has_next_iter(n5) @13
          if $builtins.py_bool(n7) then jmp b4 else jmp b6 @13

      #b4: @13
          _ = $builtins.py_store_fast("i", n1, n6) @13
          jmp b5 @13

      #b5: @14
          n10 = $builtins.py_load_global("print", n2) @14
          n11 = $builtins.py_load_fast("i", n1) @14
          n12 = $builtins.py_call(n10, n0, n11) @14
          jmp b3 @14

      #b6: @15
          n8 = $builtins.py_load_global("done", n2) @15
          n9 = $builtins.py_call(n8, n0) @15
          _ = $builtins.py_nullify_locals(n1, "i") @15
          ret n0 @15

    } @15

    define .async dummy.g(globals: *PyGlobals<dummy>, locals: *PyLocals) : *PyObject {
      #b0: @?
          n2 = [&globals:*PyGlobals<dummy>] @?
          n1 = [&locals:*PyLocals] @?
          n0 = $builtins.py_make_none() @?
          _ = $builtins.py_gen_start_coroutine() @?
          jmp b1 @?

      #b1: @18
          n3 = $builtins.py_load_global("sleep", n2) @18
          n4 = $builtins.py_call(n3, n0, $builtins.py_make_int(1)) @18
          n5 = $builtins.py_get_awaitable(n4) @18
          n6 = $builtins.py_yield_from(n5, n0) @18
          ret n0 @18

    } @18

    FINAL TRANSFORMATIONS
    .source_language = "python" @?

    type PyGlobals<dummy> = {y: *PyModuleAttr<dir1::dir2::mod,x>; random: *PyGlobals<random>;
                             mod: *PyModuleAttr<dir1::dir2,mod>; g: *PyClosure<dummy.g>;
                             f: *PyClosure<dummy.f>; a: *PyGlobals<asyncio>; E: *PyModuleAttr<foo,E>;
                             D: *PyClassCompanion<dummy,D>; C: *PyClassCompanion<dummy,C>;
                             B: *PyClassCompanion<dummy,B>}

    type PyClassCompanion<dummy,B> extends PyClassCompanion<dummy,C>, PyClassCompanion<dummy,D> = {
    }

    type PyClassCompanion<dummy,C> = {foo: *PyClosure<dummy.C.foo>}

    type PyClassCompanion<dummy,D> = {foo: *PyClosure<dummy.D.foo>}

    type .final PyClosure<dummy.D.foo> = {globals: *PyGlobals<dummy>}

    define .closure_wrapper PyClosure<dummy.D.foo>.call(__this: *PyClosure<dummy.D.foo>, locals: *PyLocals) : *PyObject {
      #entry: @20
          n0:*PyClosure<dummy.D.foo> = load &__this @20
          n1:*PyGlobals<dummy> = load n0.?.globals @20
          n2:*PyLocals = load &locals @20
          n3 = dummy.D::foo(n1, n2) @20
          ret n3 @20

    } @20

    type .final PyClosure<dummy.C.foo> = {globals: *PyGlobals<dummy>}

    define .closure_wrapper PyClosure<dummy.C.foo>.call(__this: *PyClosure<dummy.C.foo>, locals: *PyLocals) : *PyObject {
      #entry: @24
          n0:*PyClosure<dummy.C.foo> = load &__this @24
          n1:*PyGlobals<dummy> = load n0.?.globals @24
          n2:*PyLocals = load &locals @24
          n3 = dummy.C::foo(n1, n2) @24
          ret n3 @24

    } @24

    type .final PyClosure<dummy.C> = {globals: *PyGlobals<dummy>}

    define .closure_wrapper PyClosure<dummy.C>.call(__this: *PyClosure<dummy.C>, locals: *PyLocals) : *PyObject {
      #entry: @23
          n0:*PyClosure<dummy.C> = load &__this @23
          n1:*PyGlobals<dummy> = load n0.?.globals @23
          n2:*PyLocals = load &locals @23
          n3 = dummy.C(n1, n2) @23
          ret n3 @23

    } @23

    type .final PyClosure<dummy.D> = {globals: *PyGlobals<dummy>}

    define .closure_wrapper PyClosure<dummy.D>.call(__this: *PyClosure<dummy.D>, locals: *PyLocals) : *PyObject {
      #entry: @19
          n0:*PyClosure<dummy.D> = load &__this @19
          n1:*PyGlobals<dummy> = load n0.?.globals @19
          n2:*PyLocals = load &locals @19
          n3 = dummy.D(n1, n2) @19
          ret n3 @19

    } @19

    type .final PyClosure<dummy.g> = {globals: *PyGlobals<dummy>}

    define .closure_wrapper .async PyClosure<dummy.g>.call(__this: *PyClosure<dummy.g>, locals: *PyLocals) : *PyObject {
      #entry: @16
          n0:*PyClosure<dummy.g> = load &__this @16
          n1:*PyGlobals<dummy> = load n0.?.globals @16
          n2:*PyLocals = load &locals @16
          n3 = dummy.g(n1, n2) @16
          ret n3 @16

    } @16

    type .final PyClosure<dummy.f> = {globals: *PyGlobals<dummy>}

    define .closure_wrapper .args = "y,l" PyClosure<dummy.f>.call(__this: *PyClosure<dummy.f>, locals: *PyLocals) : *PyObject {
      #entry: @8
          n0:*PyClosure<dummy.f> = load &__this @8
          n1:*PyGlobals<dummy> = load n0.?.globals @8
          n2:*PyLocals = load &locals @8
          n3 = dummy.f(n1, n2) @8
          ret n3 @8

    } @8

    type .final PyClosure<dummy.B> = {globals: *PyGlobals<dummy>}

    define .closure_wrapper PyClosure<dummy.B>.call(__this: *PyClosure<dummy.B>, locals: *PyLocals) : *PyObject {
      #entry: @30
          n0:*PyClosure<dummy.B> = load &__this @30
          n1:*PyGlobals<dummy> = load n0.?.globals @30
          n2:*PyLocals = load &locals @30
          n3 = dummy.B(n1, n2) @30
          ret n3 @30

    } @30

    define dummy.__module_body__(globals: *PyGlobals<dummy>) : *PyObject {
      local locals: *PyLocals
      #b0: @2
          n22:*PyGlobals<dummy> = load &globals @2
          store &locals <- n22:*PyGlobals<dummy> @2
          n23:*PyLocals = load &locals @2
          n0 = $builtins.py_make_none() @2
          n24 = $builtins.py_make_int(0) @2
          n3 = $builtins.py_import_name(n22, "random", n0, n24) @2
          n25 = $builtins.py_store_name("random", n23, n22, n3) @2
          jmp b1 @2

      #b1: @3
          n26 = $builtins.py_make_int(0) @3
          n4 = $builtins.py_import_name(n22, "asyncio", n0, n26) @3
          n27 = $builtins.py_store_name("a", n23, n22, n4) @3
          jmp b2 @3

      #b10: @31
          n28 = __sil_allocate(<PyClosure<dummy.B>>) @31
          store n28.?.globals <- n22:*PyGlobals<dummy> @31
          n17 = $builtins.py_make_function(n28, n0, n0, n0, n0) @31
          n18 = $builtins.py_load_name("C", n23, n22) @31
          n19 = $builtins.py_load_name("D", n23, n22) @31
          n20 = $builtins.py_load_name("E", n23, n22) @31
          n30 = $builtins.py_make_string("B") @31
          n21 = $builtins.py_build_class(n17, n30, n18, n19, n20) @31
          n31 = $builtins.py_store_name("B", n23, n22, n21) @31
          ret n0 @31

      #b2: @4
          n32 = $builtins.py_make_string("x") @4
          n33 = $builtins.py_build_tuple(n32) @4
          n34 = $builtins.py_make_int(0) @4
          n5 = $builtins.py_import_name(n22, "dir1::dir2::mod", n33, n34) @4
          n6 = $builtins.py_import_from("x", n5) @4
          n35 = $builtins.py_store_name("y", n23, n22, n6) @4
          jmp b3 @4

      #b3: @5
          n36 = $builtins.py_make_string("mod") @5
          n37 = $builtins.py_build_tuple(n36) @5
          n38 = $builtins.py_make_int(0) @5
          n7 = $builtins.py_import_name(n22, "dir1::dir2", n37, n38) @5
          n8 = $builtins.py_import_from("mod", n7) @5
          n39 = $builtins.py_store_name("mod", n23, n22, n8) @5
          jmp b4 @5

      #b4: @7
          n40 = $builtins.py_make_int(0) @7
          n41 = $builtins.py_store_name("x", n23, n22, n40) @7
          jmp b5 @7

      #b5: @9
          n42 = __sil_allocate(<PyClosure<dummy.f>>) @9
          store n42.?.globals <- n22:*PyGlobals<dummy> @9
          n9 = $builtins.py_make_function(n42, n0, n0, n0, n0) @9
          n44 = $builtins.py_store_name("f", n23, n22, n9) @9
          jmp b6 @9

      #b6: @17
          n45 = __sil_allocate(<PyClosure<dummy.g>>) @17
          store n45.?.globals <- n22:*PyGlobals<dummy> @17
          n10 = $builtins.py_make_function(n45, n0, n0, n0, n0) @17
          n47 = $builtins.py_store_name("g", n23, n22, n10) @17
          jmp b7 @17

      #b7: @20
          n48 = __sil_allocate(<PyClosure<dummy.D>>) @20
          store n48.?.globals <- n22:*PyGlobals<dummy> @20
          n11 = $builtins.py_make_function(n48, n0, n0, n0, n0) @20
          n50 = $builtins.py_make_string("D") @20
          n12 = $builtins.py_build_class(n11, n50) @20
          n51 = $builtins.py_store_name("D", n23, n22, n12) @20
          jmp b8 @20

      #b8: @24
          n52 = __sil_allocate(<PyClosure<dummy.C>>) @24
          store n52.?.globals <- n22:*PyGlobals<dummy> @24
          n13 = $builtins.py_make_function(n52, n0, n0, n0, n0) @24
          n54 = $builtins.py_make_string("C") @24
          n14 = $builtins.py_build_class(n13, n54) @24
          n55 = $builtins.py_store_name("C", n23, n22, n14) @24
          jmp b9 @24

      #b9: @28
          n56 = $builtins.py_make_string("E") @28
          n57 = $builtins.py_build_tuple(n56) @28
          n58 = $builtins.py_make_int(0) @28
          n15 = $builtins.py_import_name(n22, "foo", n57, n58) @28
          n16 = $builtins.py_import_from("E", n15) @28
          n59 = $builtins.py_store_name("E", n23, n22, n16) @28
          jmp b10 @28

    } @31

    define dummy.B(globals: *PyGlobals<dummy>, locals: *PyLocals) : *PyObject {
      #b0: @31
          n4:*PyGlobals<dummy> = load &globals @31
          n5:*PyLocals = load &locals @31
          n0 = $builtins.py_make_none() @31
          n3 = $builtins.py_load_name("__name__", n5, n4) @31
          n6 = $builtins.py_store_name("__module__", n5, n4, n3) @31
          n7 = $builtins.py_make_string("B") @31
          n8 = $builtins.py_store_name("__qualname__", n5, n4, n7) @31
          jmp b1 @31

      #b1: @32
          ret n0 @32

    } @32

    define dummy.C(globals: *PyGlobals<dummy>, locals: *PyLocals) : *PyObject {
      #b0: @24
          n5:*PyGlobals<dummy> = load &globals @24
          n6:*PyLocals = load &locals @24
          n0 = $builtins.py_make_none() @24
          n3 = $builtins.py_load_name("__name__", n6, n5) @24
          n7 = $builtins.py_store_name("__module__", n6, n5, n3) @24
          n8 = $builtins.py_make_string("C") @24
          n9 = $builtins.py_store_name("__qualname__", n6, n5, n8) @24
          jmp b1 @24

      #b1: @25
          n10 = __sil_allocate(<PyClosure<dummy.C.foo>>) @25
          store n10.?.globals <- n5:*PyGlobals<dummy> @25
          n4 = $builtins.py_make_function(n10, n0, n0, n0, n0) @25
          n12 = $builtins.py_store_name("foo", n6, n5, n4) @25
          ret n0 @25

    } @25

    define dummy.C::foo(globals: *PyGlobals<dummy>, locals: *PyLocals) : *PyObject {
      #b0: @26
          n3:*PyGlobals<dummy> = load &globals @26
          n4:*PyLocals = load &locals @26
          n0 = $builtins.py_make_none() @26
          ret n0 @26

    } @26

    define dummy.D(globals: *PyGlobals<dummy>, locals: *PyLocals) : *PyObject {
      #b0: @20
          n5:*PyGlobals<dummy> = load &globals @20
          n6:*PyLocals = load &locals @20
          n0 = $builtins.py_make_none() @20
          n3 = $builtins.py_load_name("__name__", n6, n5) @20
          n7 = $builtins.py_store_name("__module__", n6, n5, n3) @20
          n8 = $builtins.py_make_string("D") @20
          n9 = $builtins.py_store_name("__qualname__", n6, n5, n8) @20
          jmp b1 @20

      #b1: @21
          n10 = __sil_allocate(<PyClosure<dummy.D.foo>>) @21
          store n10.?.globals <- n5:*PyGlobals<dummy> @21
          n4 = $builtins.py_make_function(n10, n0, n0, n0, n0) @21
          n12 = $builtins.py_store_name("foo", n6, n5, n4) @21
          ret n0 @21

    } @21

    define dummy.D::foo(globals: *PyGlobals<dummy>, locals: *PyLocals) : *PyObject {
      #b0: @22
          n3:*PyGlobals<dummy> = load &globals @22
          n4:*PyLocals = load &locals @22
          n0 = $builtins.py_make_none() @22
          ret n0 @22

    } @22

    define .args = "y,l" dummy.f(globals: *PyGlobals<dummy>, locals: *PyLocals) : *PyObject {
      #b0: @10
          n16:*PyGlobals<dummy> = load &globals @10
          n17:*PyLocals = load &locals @10
          n0 = $builtins.py_make_none() @10
          n3 = $builtins.py_load_fast("y", n17) @10
          jmp b1, b2 @10

      #b1: @11
          n18 = $builtins.py_bool(n3) @10
          prune n18 @10
          n13 = $builtins.py_load_global("g", n16) @11
          n14 = $builtins.py_load_fast("y", n17) @11
          n19 = $builtins.py_make_int(0) @11
          n15 = $builtins.py_call(n13, n0, n19, n14) @11
          n20 = $builtins.py_nullify_locals(n17, "i") @11
          ret n0 @11

      #b2: @13
          n21 = $builtins.py_bool(n3) @10
          prune __sil_lnot(n21) @10
          n4 = $builtins.py_load_fast("l", n17) @13
          n5 = $builtins.py_get_iter(n4) @13
          jmp b3 @13

      #b3: @13
          n6 = $builtins.py_next_iter(n5) @13
          n7 = $builtins.py_has_next_iter(n5) @13
          jmp b4, b6 @13

      #b4: @13
          n22 = $builtins.py_bool(n7) @13
          prune n22 @13
          n23 = $builtins.py_store_fast("i", n17, n6) @13
          jmp b5 @13

      #b5: @14
          n10 = $builtins.py_load_global("print", n16) @14
          n11 = $builtins.py_load_fast("i", n17) @14
          n12 = $builtins.py_call(n10, n0, n11) @14
          jmp b3 @14

      #b6: @15
          n24 = $builtins.py_bool(n7) @13
          prune __sil_lnot(n24) @13
          n8 = $builtins.py_load_global("done", n16) @15
          n9 = $builtins.py_call(n8, n0) @15
          n25 = $builtins.py_nullify_locals(n17, "i") @15
          ret n0 @15

    } @15

    define .async dummy.g(globals: *PyGlobals<dummy>, locals: *PyLocals) : *PyObject {
      #b0: @?
          n7:*PyGlobals<dummy> = load &globals @?
          n8:*PyLocals = load &locals @?
          n0 = $builtins.py_make_none() @?
          n9 = $builtins.py_gen_start_coroutine() @?
          jmp b1 @?

      #b1: @18
          n3 = $builtins.py_load_global("sleep", n7) @18
          n10 = $builtins.py_make_int(1) @18
          n4 = $builtins.py_call(n3, n0, n10) @18
          n5 = $builtins.py_get_awaitable(n4) @18
          n6 = $builtins.py_yield_from(n5, n0) @18
          ret n0 @18

    } @18 |}]
