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


    let%expect_test "store_fast then load_fast" =
      convert_and_print
        {|
        .source_language = "python"
        define .args = "x" foo(globals: *PyGlobals, locals: *PyLocals) : *PyObject {
          #b0:
              n1 = locals
              n2 = $builtins.py_load_fast("x", n1)
              n3 = $builtins.py_make_int(1)
              n4 = $builtins.py_binary_add(n2, n3)
              _ = $builtins.py_store_fast("y", n1, n4)
              jmp b1

          #b1:
              n5 = $builtins.py_load_fast("y", n1)
              ret n5
        }
        |} ;
      [%expect
        {|
        === foo ===
        Equations:
        x      = @param:x  [param]
        n1     = (@load (@lvar locals))  [let]
        n2     = @param:x  [load_fast: locals]
        n3     = ($builtins.py_make_int 1)  [let]
        n4     = ($builtins.py_binary_add @param:x ($builtins.py_make_int 1))  [let]
        y      = ($builtins.py_binary_add @param:x ($builtins.py_make_int 1))  [store_fast: locals]
        n5     = ($builtins.py_binary_add @param:x ($builtins.py_make_int 1))  [load_fast: locals]
        RET    = (@ret
                     (@seq
                         (@seq @state0 ($builtins.py_make_int 1))
                         ($builtins.py_binary_add @param:x ($builtins.py_make_int 1)))
                     ($builtins.py_binary_add @param:x ($builtins.py_make_int 1)))  [ret]
        PEG: (@ret
                 (@seq
                     (@seq @state0 ($builtins.py_make_int 1))
                     ($builtins.py_binary_add @param:x ($builtins.py_make_int 1)))
                 ($builtins.py_binary_add @param:x ($builtins.py_make_int 1)))
        |}]


    let%expect_test "if branch with phi" =
      convert_and_print
        {|
        .source_language = "python"
        define .args = "c" foo(globals: *PyGlobals, locals: *PyLocals) : *PyObject {
          #b0:
              n1 = locals
              n2 = $builtins.py_load_fast("c", n1)
              if n2 then jmp b1 else jmp b2

          #b1:
              n3 = $builtins.py_make_int(1)
              ret n3

          #b2:
              n4 = $builtins.py_make_int(2)
              ret n4
        }
        |} ;
      [%expect
        {|
        === foo ===
        Equations:
        c      = @param:c  [param]
        n1     = (@load (@lvar locals))  [let]
        n2     = @param:c  [load_fast: locals]
        n3     = ($builtins.py_make_int 1)  [let]
        RET    = (@ret (@seq @state0 ($builtins.py_make_int 1)) ($builtins.py_make_int 1))  [ret]
        n4     = ($builtins.py_make_int 2)  [let]
        RET    = (@ret (@seq @state0 ($builtins.py_make_int 2)) ($builtins.py_make_int 2))  [ret]
        PHI    = (@phi
                     @param:c
                     (@ret (@seq @state0 ($builtins.py_make_int 1)) ($builtins.py_make_int 1))
                     (@ret (@seq @state0 ($builtins.py_make_int 2)) ($builtins.py_make_int 2)))  [if]
        PEG: (@phi
                 @param:c
                 (@ret (@seq @state0 ($builtins.py_make_int 1)) ($builtins.py_make_int 1))
                 (@ret (@seq @state0 ($builtins.py_make_int 2)) ($builtins.py_make_int 2)))
        |}]
  end )
