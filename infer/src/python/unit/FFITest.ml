(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let%test_module "load_code" =
  ( module struct
    let source = "x = 42"

    let%expect_test _ =
      if not (Py.is_initialized ()) then Py.initialize ~interpreter:Version.python_exe () ;
      match FFI.from_string ~source ~filename:"dummy" with
      | Error (err, kind) ->
          Logging.die err "%a@\n" FFI.Error.pp_kind kind
      | Ok res ->
          Py.finalize () ;
          F.printf "%s" (FFI.Code.full_show res) ;
          [%expect
            {|
        { co_name = "<module>"; co_filename = "dummy"; co_flags = 64;
          co_cellvars = [||]; co_freevars = [||]; co_names = [|"x"|];
          co_varnames = [||]; co_nlocals = 0; co_argcount = 0; co_firstlineno = 1;
          co_posonlyargcount = 0; co_stacksize = 1; co_kwonlyargcount = 0;
          co_lnotab = [||]; co_consts = [|FFI.PYCInt (42L); FFI.PYCNone|];
          instructions =
          [{ FFI.opname = "LOAD_CONST"; FFI.opcode = 100; FFI.arg = 0;
             FFI.argval = FFI.PYCInt (42L); FFI.offset = 0;
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
  end )
