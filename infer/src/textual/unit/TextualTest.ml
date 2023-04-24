(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open Textual

let%test_module "procnames" =
  ( module struct
    let%expect_test _ =
      let toplevel_proc =
        ProcDecl.
          { qualified_name=
              { enclosing_class= TopLevel
              ; name= {value= "toplevel"; loc= Location.known ~line:0 ~col:0} }
          ; formals_types= Some []
          ; result_type= Typ.mk_without_attributes Typ.Void
          ; attributes= [] }
      in
      let as_java = TextualSil.proc_decl_to_sil Lang.Java toplevel_proc in
      let as_hack = TextualSil.proc_decl_to_sil Lang.Hack toplevel_proc in
      F.printf "%a@\n" Procname.pp as_java ;
      F.printf "%a@\n" Procname.pp as_hack ;
      [%expect {|
        void $TOPLEVEL$CLASS$.toplevel()
        toplevel |}]
  end )


let%test_module "line map" =
  ( module struct
    let text =
      {|
          // TEXTUAL UNIT START level1.hack
          .source_language = "hack"

          // .file "level1.hack"
          // .line 6
          define $root.taintSource(this: *void) : *HackInt {
          #b0:
          // .line 7
            n0 = $builtins.hhbc_is_type_int($builtins.hack_int(42))
            n1 = $builtins.hhbc_not(n0)
            jmp b1, b2
          #b1:
            prune $builtins.hack_is_true(n1)
            n2 = $builtins.hhbc_verify_failed()
            unreachable
          #b2:
            prune ! $builtins.hack_is_true(n1)
            ret $builtins.hack_int(42)
          }

          // .file "level1.hack"
          // .line 10
          define $root.taintSink(this: *void, $i: *HackInt) : *void {
          #b0:
          // .line 11
            ret $builtins.hack_null()
          }

          // .file "level1.hack"
          // .line 13
          define $root.FN_basicFlowBad(this: *void) : *void {
          local $tainted: *void
          #b0:
          // .line 14
            n0 = $root.taintSource(null)
            store &$tainted <- n0: *Mixed
          // .line 15
            n1: *Mixed = load &$tainted
            n2 = $root.taintSink(null, n1)
          // .line 16
            ret $builtins.hack_null()
          }

          // .file "level1.hack"
          // .line 18
          define $root.basicFlowOk(this: *void, $untainted: *HackInt) : *void {
          #b0:
          // .line 19
            n0: *Mixed = load &$untainted
            n1 = $root.taintSink(null, n0)
          // .line 20
            ret $builtins.hack_null()
          }
          // TEXTUAL UNIT END level1.hack
      |}


    let%expect_test _ =
      let line_map = LineMap.create text in
      let lines = String.split_lines text in
      List.iteri lines ~f:(fun i text ->
          let line = LineMap.find line_map i in
          F.printf "%i: %s\n" (Option.value_exn line) text ) ;
      [%expect
        {|
        1:
        1:           // TEXTUAL UNIT START level1.hack
        1:           .source_language = "hack"
        1:
        1:           // .file "level1.hack"
        1:           // .line 6
        6:           define $root.taintSource(this: *void) : *HackInt {
        6:           #b0:
        6:           // .line 7
        7:             n0 = $builtins.hhbc_is_type_int($builtins.hack_int(42))
        7:             n1 = $builtins.hhbc_not(n0)
        7:             jmp b1, b2
        7:           #b1:
        7:             prune $builtins.hack_is_true(n1)
        7:             n2 = $builtins.hhbc_verify_failed()
        7:             unreachable
        7:           #b2:
        7:             prune ! $builtins.hack_is_true(n1)
        7:             ret $builtins.hack_int(42)
        7:           }
        7:
        7:           // .file "level1.hack"
        7:           // .line 10
        10:           define $root.taintSink(this: *void, $i: *HackInt) : *void {
        10:           #b0:
        10:           // .line 11
        11:             ret $builtins.hack_null()
        11:           }
        11:
        11:           // .file "level1.hack"
        11:           // .line 13
        13:           define $root.FN_basicFlowBad(this: *void) : *void {
        13:           local $tainted: *void
        13:           #b0:
        13:           // .line 14
        14:             n0 = $root.taintSource(null)
        14:             store &$tainted <- n0: *Mixed
        14:           // .line 15
        15:             n1: *Mixed = load &$tainted
        15:             n2 = $root.taintSink(null, n1)
        15:           // .line 16
        16:             ret $builtins.hack_null()
        16:           }
        16:
        16:           // .file "level1.hack"
        16:           // .line 18
        18:           define $root.basicFlowOk(this: *void, $untainted: *HackInt) : *void {
        18:           #b0:
        18:           // .line 19
        19:             n0: *Mixed = load &$untainted
        19:             n1 = $root.taintSink(null, n0)
        19:           // .line 20
        20:             ret $builtins.hack_null()
        20:           }
        20:           // TEXTUAL UNIT END level1.hack
        20: |}]
  end )
