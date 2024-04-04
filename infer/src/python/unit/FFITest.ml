(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

let dummy = "dummy.py"

let test ?(filename = dummy) source =
  if not (Py.is_initialized ()) then Py.initialize ~interpreter:Version.python_exe () ;
  ( match FFI.from_string ~source ~filename with
  | Error (kind, err) ->
      L.die kind "FFI error: %a@\n" FFI.Error.pp_kind err
  | Ok code ->
      F.printf "%s" (FFI.Code.full_show code)
  | exception (Py.E _ as e) ->
      L.die ExternalError "Pyml exception: %s@\n" (Exn.to_string e) ) ;
  Py.finalize ()


let%test_module "load_code" =
  ( module struct
    let%expect_test _ =
      let source = "x = 42" in
      test source ;
      [%expect
        {|
        { co_name = "<module>"; co_filename = "dummy.py"; co_flags = 64;
          co_cellvars = [||]; co_freevars = [||]; co_names = [|"x"|];
          co_varnames = [||]; co_nlocals = 0; co_argcount = 0; co_firstlineno = 1;
          co_posonlyargcount = 0; co_stacksize = 1; co_kwonlyargcount = 0;
          co_lnotab = [||]; co_consts = [|FFI.PYCInt (42); FFI.PYCNone|];
          instructions =
          [{ FFI.opname = "LOAD_CONST"; FFI.opcode = 100; FFI.arg = 0;
             FFI.argval = FFI.PYCInt (42); FFI.offset = 0;
             FFI.starts_line = Some (1); FFI.is_jump_target = false };
           { FFI.opname = "STORE_NAME"; FFI.opcode = 90; FFI.arg = 0;
             FFI.argval = FFI.PYCString ("x"); FFI.offset = 2;
             FFI.starts_line = None; FFI.is_jump_target = false };
           { FFI.opname = "LOAD_CONST"; FFI.opcode = 100; FFI.arg = 1;
             FFI.argval = FFI.PYCNone; FFI.offset = 4; FFI.starts_line = None;
             FFI.is_jump_target = false };
           { FFI.opname = "RETURN_VALUE"; FFI.opcode = 83; FFI.arg = 0;
             FFI.argval = FFI.PYCNone; FFI.offset = 6; FFI.starts_line = None;
             FFI.is_jump_target = false }]
          } |}]


    let%expect_test _ =
      let source = "print(5j)" in
      test source ;
      [%expect
        {|
        { co_name = "<module>"; co_filename = "dummy.py"; co_flags = 64;
          co_cellvars = [||]; co_freevars = [||]; co_names = [|"print"|];
          co_varnames = [||]; co_nlocals = 0; co_argcount = 0; co_firstlineno = 1;
          co_posonlyargcount = 0; co_stacksize = 2; co_kwonlyargcount = 0;
          co_lnotab = [||];
          co_consts = [|FFI.PYCComplex ({ real = 0.; imag = 5. }); FFI.PYCNone|];
          instructions =
          [{ FFI.opname = "LOAD_NAME"; FFI.opcode = 101; FFI.arg = 0;
             FFI.argval = FFI.PYCString ("print"); FFI.offset = 0;
             FFI.starts_line = Some (1); FFI.is_jump_target = false };
           { FFI.opname = "LOAD_CONST"; FFI.opcode = 100; FFI.arg = 0;
             FFI.argval = FFI.PYCComplex ({ real = 0.; imag = 5. }); FFI.offset = 2;
             FFI.starts_line = None; FFI.is_jump_target = false };
           { FFI.opname = "CALL_FUNCTION"; FFI.opcode = 131; FFI.arg = 1;
             FFI.argval = FFI.PYCInt (1); FFI.offset = 4; FFI.starts_line = None;
             FFI.is_jump_target = false };
           { FFI.opname = "POP_TOP"; FFI.opcode = 1; FFI.arg = 0;
             FFI.argval = FFI.PYCNone; FFI.offset = 6; FFI.starts_line = None;
             FFI.is_jump_target = false };
           { FFI.opname = "LOAD_CONST"; FFI.opcode = 100; FFI.arg = 1;
             FFI.argval = FFI.PYCNone; FFI.offset = 8; FFI.starts_line = None;
             FFI.is_jump_target = false };
           { FFI.opname = "RETURN_VALUE"; FFI.opcode = 83; FFI.arg = 0;
             FFI.argval = FFI.PYCNone; FFI.offset = 10; FFI.starts_line = None;
             FFI.is_jump_target = false }]
          } |}]


    let%expect_test _ =
      let source = "x = ..." in
      test source ;
      [%expect
        {|
        { co_name = "<module>"; co_filename = "dummy.py"; co_flags = 64;
          co_cellvars = [||]; co_freevars = [||]; co_names = [|"x"|];
          co_varnames = [||]; co_nlocals = 0; co_argcount = 0; co_firstlineno = 1;
          co_posonlyargcount = 0; co_stacksize = 1; co_kwonlyargcount = 0;
          co_lnotab = [||]; co_consts = [|FFI.PYCNone; FFI.PYCNone|];
          instructions =
          [{ FFI.opname = "LOAD_CONST"; FFI.opcode = 100; FFI.arg = 0;
             FFI.argval = FFI.PYCNone; FFI.offset = 0; FFI.starts_line = Some (1);
             FFI.is_jump_target = false };
           { FFI.opname = "STORE_NAME"; FFI.opcode = 90; FFI.arg = 0;
             FFI.argval = FFI.PYCString ("x"); FFI.offset = 2;
             FFI.starts_line = None; FFI.is_jump_target = false };
           { FFI.opname = "LOAD_CONST"; FFI.opcode = 100; FFI.arg = 1;
             FFI.argval = FFI.PYCNone; FFI.offset = 4; FFI.starts_line = None;
             FFI.is_jump_target = false };
           { FFI.opname = "RETURN_VALUE"; FFI.opcode = 83; FFI.arg = 0;
             FFI.argval = FFI.PYCNone; FFI.offset = 6; FFI.starts_line = None;
             FFI.is_jump_target = false }]
          } |}]


    let%expect_test _ =
      let source = "x = 100000000000000000000" in
      test source ;
      [%expect
        {|
        { co_name = "<module>"; co_filename = "dummy.py"; co_flags = 64;
          co_cellvars = [||]; co_freevars = [||]; co_names = [|"x"|];
          co_varnames = [||]; co_nlocals = 0; co_argcount = 0; co_firstlineno = 1;
          co_posonlyargcount = 0; co_stacksize = 1; co_kwonlyargcount = 0;
          co_lnotab = [||];
          co_consts = [|FFI.PYCInt (100000000000000000000); FFI.PYCNone|];
          instructions =
          [{ FFI.opname = "LOAD_CONST"; FFI.opcode = 100; FFI.arg = 0;
             FFI.argval = FFI.PYCInt (100000000000000000000); FFI.offset = 0;
             FFI.starts_line = Some (1); FFI.is_jump_target = false };
           { FFI.opname = "STORE_NAME"; FFI.opcode = 90; FFI.arg = 0;
             FFI.argval = FFI.PYCString ("x"); FFI.offset = 2;
             FFI.starts_line = None; FFI.is_jump_target = false };
           { FFI.opname = "LOAD_CONST"; FFI.opcode = 100; FFI.arg = 1;
             FFI.argval = FFI.PYCNone; FFI.offset = 4; FFI.starts_line = None;
             FFI.is_jump_target = false };
           { FFI.opname = "RETURN_VALUE"; FFI.opcode = 83; FFI.arg = 0;
             FFI.argval = FFI.PYCNone; FFI.offset = 6; FFI.starts_line = None;
             FFI.is_jump_target = false }]
          } |}]


    let%expect_test _ =
      let source = "x in {'fnv', 'siphash24'}" in
      test source ;
      [%expect
        {|
        { co_name = "<module>"; co_filename = "dummy.py"; co_flags = 64;
          co_cellvars = [||]; co_freevars = [||]; co_names = [|"x"|];
          co_varnames = [||]; co_nlocals = 0; co_argcount = 0; co_firstlineno = 1;
          co_posonlyargcount = 0; co_stacksize = 2; co_kwonlyargcount = 0;
          co_lnotab = [||];
          co_consts =
          [|FFI.PYCFrozenSet ([FFI.PYCString ("fnv"); FFI.PYCString ("siphash24")])
            ; FFI.PYCNone|];
          instructions =
          [{ FFI.opname = "LOAD_NAME"; FFI.opcode = 101; FFI.arg = 0;
             FFI.argval = FFI.PYCString ("x"); FFI.offset = 0;
             FFI.starts_line = Some (1); FFI.is_jump_target = false };
           { FFI.opname = "LOAD_CONST"; FFI.opcode = 100; FFI.arg = 0;
             FFI.argval =
             FFI.PYCFrozenSet ([FFI.PYCString ("fnv"); FFI.PYCString ("siphash24")]);
             FFI.offset = 2; FFI.starts_line = None; FFI.is_jump_target = false };
           { FFI.opname = "COMPARE_OP"; FFI.opcode = 107; FFI.arg = 6;
             FFI.argval = FFI.PYCString ("in"); FFI.offset = 4;
             FFI.starts_line = None; FFI.is_jump_target = false };
           { FFI.opname = "POP_TOP"; FFI.opcode = 1; FFI.arg = 0;
             FFI.argval = FFI.PYCNone; FFI.offset = 6; FFI.starts_line = None;
             FFI.is_jump_target = false };
           { FFI.opname = "LOAD_CONST"; FFI.opcode = 100; FFI.arg = 1;
             FFI.argval = FFI.PYCNone; FFI.offset = 8; FFI.starts_line = None;
             FFI.is_jump_target = false };
           { FFI.opname = "RETURN_VALUE"; FFI.opcode = 83; FFI.arg = 0;
             FFI.argval = FFI.PYCNone; FFI.offset = 10; FFI.starts_line = None;
             FFI.is_jump_target = false }]
          } |}]


    let%expect_test _ =
      let source = {|
normal = 'yolo'
badx = '\uD800'|} in
      test source ;
      [%expect
        {|
        { co_name = "<module>"; co_filename = "dummy.py"; co_flags = 64;
          co_cellvars = [||]; co_freevars = [||]; co_names = [|"normal"; "badx"|];
          co_varnames = [||]; co_nlocals = 0; co_argcount = 0; co_firstlineno = 2;
          co_posonlyargcount = 0; co_stacksize = 1; co_kwonlyargcount = 0;
          co_lnotab = [|; |];
          co_consts =
          [|FFI.PYCString ("yolo"); FFI.PYCInvalidUnicode ([|27648|]); FFI.PYCNone|];
          instructions =
          [{ FFI.opname = "LOAD_CONST"; FFI.opcode = 100; FFI.arg = 0;
             FFI.argval = FFI.PYCString ("yolo"); FFI.offset = 0;
             FFI.starts_line = Some (2); FFI.is_jump_target = false };
           { FFI.opname = "STORE_NAME"; FFI.opcode = 90; FFI.arg = 0;
             FFI.argval = FFI.PYCString ("normal"); FFI.offset = 2;
             FFI.starts_line = None; FFI.is_jump_target = false };
           { FFI.opname = "LOAD_CONST"; FFI.opcode = 100; FFI.arg = 1;
             FFI.argval = FFI.PYCInvalidUnicode ([|27648|]); FFI.offset = 4;
             FFI.starts_line = Some (3); FFI.is_jump_target = false };
           { FFI.opname = "STORE_NAME"; FFI.opcode = 90; FFI.arg = 1;
             FFI.argval = FFI.PYCString ("badx"); FFI.offset = 6;
             FFI.starts_line = None; FFI.is_jump_target = false };
           { FFI.opname = "LOAD_CONST"; FFI.opcode = 100; FFI.arg = 2;
             FFI.argval = FFI.PYCNone; FFI.offset = 8; FFI.starts_line = None;
             FFI.is_jump_target = false };
           { FFI.opname = "RETURN_VALUE"; FFI.opcode = 83; FFI.arg = 0;
             FFI.argval = FFI.PYCNone; FFI.offset = 10; FFI.starts_line = None;
             FFI.is_jump_target = false }]
          } |}]
  end )
