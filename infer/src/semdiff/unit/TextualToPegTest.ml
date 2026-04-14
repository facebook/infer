(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(* ---------- Helpers ---------- *)

let convert_and_print text = TextualPegDiff.convert_and_print text

(* ---------- Tests ---------- *)

let%test_module "textual to peg" =
  ( module struct
    let%expect_test "straight-line: ret constant" =
      convert_and_print
        {|
        .source_language = "python"
        define foo() : int {
          #b0:
              n0 = $builtins.py_make_int(42)
              ret n0
        }
        |} ;
      [%expect
        {|
        === foo ===
        Equations:
        n0     = ($builtins.py_make_int 42)  [let]
        RET    = (@ret (@seq @state0 ($builtins.py_make_int 42)) ($builtins.py_make_int 42))  [ret]
        PEG: (@ret (@seq @state0 ($builtins.py_make_int 42)) ($builtins.py_make_int 42)) |}]
  end )
