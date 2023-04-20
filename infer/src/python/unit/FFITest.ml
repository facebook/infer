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
      Py.initialize ~interpreter:Version.python_exe () ;
      let res = FFI.from_string ~source ~filename:"dummy" in
      Py.finalize () ;
      F.printf "%s" (FFI.Code.full_show res) ;
      [%expect
        {|
        { co_name = "<module>"; co_filename = "dummy"; co_flags = 64;
          co_cellvars = [||]; co_freevars = [||]; co_names = [|"x"|];
          co_varnames = [||]; co_nlocals = 0; co_argcount = 0; co_firstlineno = 1;
          co_posonlyargcount = 0; co_stacksize = 1; co_kwonlyargcount = 0;
          co_lnotab = [||]; co_consts = [|(FFI.PYCInt 42L); FFI.PYCNone|];
          instructions =
          [{ FFI.opname = "LOAD_CONST"; opcode = 100; arg = 0;
             argval = (FFI.PYCInt 42L); offset = 0; starts_line = (Some 1);
             is_jump_target = false };
            { FFI.opname = "STORE_NAME"; opcode = 90; arg = 0;
              argval = (FFI.PYCString "x"); offset = 2; starts_line = None;
              is_jump_target = false };
            { FFI.opname = "LOAD_CONST"; opcode = 100; arg = 1; argval = FFI.PYCNone;
              offset = 4; starts_line = None; is_jump_target = false };
            { FFI.opname = "RETURN_VALUE"; opcode = 83; arg = 0;
              argval = FFI.PYCNone; offset = 6; starts_line = None;
              is_jump_target = false }
            ]
          } |}]
  end )
