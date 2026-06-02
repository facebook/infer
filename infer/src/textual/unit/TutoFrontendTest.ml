(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* ======================================================================================= *)
(*  EXECUTABLE COMPANION of TutoFrontend.ml                                                *)
(* ======================================================================================= *)
(*                                                                                         *)
(*  This is the runnable counterpart of the ToyLang frontend tutorial in                   *)
(*  ../TutoFrontend.ml. Where the main file *explains and builds* the pipeline, this file  *)
(*  *runs* it, step by step, on the example program, and pins the output with [%expect]    *)
(*  blocks (so the tutorial stays correct: if the translation ever changes, these tests    *)
(*  break).                                                                                *)
(*                                                                                         *)
(*  Read the corresponding section of TutoFrontend.ml first, then come here to see it      *)
(*  actually execute. Each [let%expect_test] below maps to one stage of the pipeline.      *)
(*                                                                                         *)
(*  Run / refresh the expected outputs with:                                               *)
(*      make -C infer/src runtest        # builds and auto-promotes the [%expect] blocks   *)
(* ======================================================================================= *)

open! IStd
module F = Format

(* The example ToyLang program from the top of TutoFrontend.ml: [main] dereferences a null *)
(* pointer, which Pulse flags once this is captured through the driver.                    *)
let example =
  {|
    fun max(a, b) {
      if (a < b) { return b; } else { return a; }
    }

    fun main() {
      var p = alloc();
      *p = max(3, 7);
      var q = nil;
      return *q;
    }
  |}


let sourcefile = Textual.SourceFile.create "demo.toy"

let%test_module "ToyLang tutorial frontend" =
  ( module struct
    (* ---- Steps (1)+(2): ToyLang source -> Textual.Module.t ------------------------- *)
    (* This exercises [TutoFrontend.parse] (lexer + recursive-descent parser) followed   *)
    (* by [TutoFrontend.build_module] (the AST -> Textual translation). Note in the       *)
    (* output how reads become [load &x], how pointers are typed [*Cell] and [*p] is a    *)
    (* [contents] field access, how arithmetic / comparisons / calls are nested calls to  *)
    (* builtins / user procedures, and how [if] is a single structured terminator. *)
    let%expect_test "translation to Textual" =
      let prog = TutoFrontend.parse example in
      let module_ = TutoFrontend.build_module sourcefile prog in
      F.printf "%a@\n" (Textual.Module.pp ~show_location:false) module_ ;
      [%expect
        {|
        .source_language = "C"

        type Cell = {contents: int}

        define max(a: int, b: int) : int {
          #b0:
              if __sil_lt([&a:int], [&b:int]) then jmp b1 else jmp b2

          #b1:
              ret [&b:int]

          #b2:
              ret [&a:int]

          #b3:
              ret null

        }

        define main() : int {
          local p: *Cell, q: *Cell
          #b0:
              store &p <- __sil_allocate(<Cell>):*Cell
              store [&p:*Cell].Cell.contents <- max(3, 7):int
              store &q <- null:*Cell
              ret [[&q:*Cell].Cell.contents:int]

        }
        |}]


    (* ---- Step (3): strict verification -------------------------------------------- *)
    (* The recommended path (as in integration/Python.ml): full type checking + inference. *)
    (* If this passes, our ToyLang really does fit into Textual's type system. *)
    let%expect_test "strict verification succeeds" =
      let prog = TutoFrontend.parse example in
      let module_ = TutoFrontend.build_module sourcefile prog in
      ( match TextualVerification.verify_strict module_ with
      | Ok _ ->
          F.printf "strict verification succeeded@\n"
      | Error errors ->
          F.printf "strict verification failed:@\n" ;
          List.iter errors
            ~f:(F.printf "%a@\n" (TextualVerification.pp_error_with_sourcefile sourcefile)) ) ;
      [%expect {| strict verification succeeded |}]


    (* ---- Step (4): the standard transforms ---------------------------------------- *)
    (* Here the nested sub-expressions get hoisted into fresh SSA temporaries by          *)
    (* [remove_effects_in_subexprs], the structured [if] is lowered to pruned successor    *)
    (* nodes by [remove_if_terminator], etc. Compare with the step (1)+(2) output to see   *)
    (* exactly what the transforms do for us. *)
    let%expect_test "after verification and transforms" =
      let prog = TutoFrontend.parse example in
      let module_ = TutoFrontend.build_module sourcefile prog in
      ( match TextualVerification.verify_strict module_ with
      | Error _ ->
          F.printf "verification failed@\n"
      | Ok verified ->
          let transformed, _decls = TextualTransform.run_exn Textual.Lang.C verified in
          F.printf "%a@\n" (Textual.Module.pp ~show_location:false) transformed ) ;
      [%expect
        {|
        .source_language = "C"

        type Cell = {contents: int}

        define max(a: int, b: int) : int {
          #b0:
              n0:int = load &a
              n1:int = load &b
              jmp b1, b2

          #b1:
              prune __sil_lt(n0, n1)
              n2:int = load &b
              ret n2

          #b2:
              prune __sil_lnot(__sil_lt(n0, n1))
              n3:int = load &a
              ret n3

        }

        define main() : int {
          local p: *Cell, q: *Cell
          #b0:
              n0 = __sil_allocate(<Cell>)
              store &p <- n0:*Cell
              n1:*Cell = load &p
              n2 = max(3, 7)
              store n1.Cell.contents <- n2:int
              store &q <- null:*Cell
              n3:*Cell = load &q
              n4:int = load n3.Cell.contents
              ret n4

        }
        |}]


    (* ---- Step (5): lowering to SIL ------------------------------------------------- *)
    (* The real proof that the frontend is well-formed: Textual lowers to SIL without      *)
    (* error. We print the resulting procedure signatures. (Step (6), capturing into       *)
    (* capture.db, needs a results directory and so belongs to the driver path, not a       *)
    (* unit test -- see section 7 of TutoFrontend.ml.) *)
    let%expect_test "lowering to SIL succeeds" =
      let prog = TutoFrontend.parse example in
      let module_ = TutoFrontend.build_module sourcefile prog in
      ( match TextualVerification.verify_strict module_ with
      | Error _ ->
          F.printf "verification failed@\n"
      | Ok verified -> (
          let transformed, decls = TextualTransform.run_exn Textual.Lang.C verified in
          match TextualSil.module_to_sil Textual.Lang.C transformed decls with
          | Error _ ->
              F.printf "SIL conversion failed@\n"
          | Ok (cfg, _tenv) ->
              F.printf "SIL conversion succeeded; procedures:@\n%a" Cfg.pp_proc_signatures cfg ) ) ;
      [%expect
        {|
        SIL conversion succeeded; procedures:
        METHOD SIGNATURES
        main [defined, Return type: int, Formals: None, Locals:  p:Cell* q:Cell*, is_deleted:false]
        max [defined, Return type: int, Formals:  a:int b:int, Locals: None, is_deleted:false]
        |}]
  end )
