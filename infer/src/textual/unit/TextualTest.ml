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
// .line 8
define $root.Level1::taintSource($this: *void) : .notnull *HackInt {
#b0:
// .line 9
// .column 3
  n0 = $builtins.hhbc_is_type_int($builtins.hack_int(42))
// .line 9
// .column 3
  n1 = $builtins.hhbc_verify_type_pred($builtins.hack_int(42), n0)
// .line 9
// .column 3
  ret $builtins.hack_int(42)
}

// .file "level1.hack"
// .line 12
define $root.Level1::taintSink($this: *void, $args: *HackMixed) : *void {
#b0:
// .line 13
// .column 2
  ret null
}

// .file "level1.hack"
// .line 15
define $root.Level1::basicFlowBad($this: *void) : *void {
local $tainted: *void
#b0:
// .line 16
// .column 14
  n0 = $root.Level1::taintSource(null)
// .line 16
// .column 3
  store &$tainted <- n0: *HackMixed
// .line 17
// .column 13
  n1: *HackMixed = load &$tainted
// .line 17
// .column 3
  n2 = $root.Level1::taintSink(null, n1)
// .line 18
// .column 2
  ret null
}

// .file "level1.hack"
// .line 20
define $root.Level1::basicFlowOk($this: *void, $untainted: .notnull *HackInt) : *void {
#b0:
// .line 21
// .column 13
  n0: *HackMixed = load &$untainted
// .line 21
// .column 3
  n1 = $root.Level1::taintSink(null, n0)
// .line 22
// .column 2
  ret null
}

// ----- EXTERNALS -----
declare $builtins.hhbc_is_type_int(...): *HackMixed
declare $builtins.hhbc_verify_type_pred(...): *HackMixed

// ----- BUILTIN DECLS STARTS HERE -----
declare $builtins.hack_int(int): *HackInt

// TEXTUAL UNIT END level1.hack
|}


    let%expect_test _ =
      let line_map = LineMap.create text in
      let lines = String.split_lines text in
      List.iteri lines ~f:(fun i text ->
          Option.iter (LineMap.find line_map i) ~f:(fun {LineMap.line; column} ->
              let pp_column fmt =
                if column > 0 then F.fprintf fmt " column:%2i" column
                else F.pp_print_string fmt "          "
              in
              F.printf "line:%2i%t %s\n" line pp_column text ) ) ;
      [%expect
        {|
        line: 1
        line: 1           // TEXTUAL UNIT START level1.hack
        line: 1           .source_language = "hack"
        line: 1
        line: 1           // .file "level1.hack"
        line: 1           // .line 8
        line: 8           define $root.Level1::taintSource($this: *void) : .notnull *HackInt {
        line: 8           #b0:
        line: 8           // .line 9
        line: 9           // .column 3
        line: 9 column: 3   n0 = $builtins.hhbc_is_type_int($builtins.hack_int(42))
        line: 9 column: 3 // .line 9
        line: 9           // .column 3
        line: 9 column: 3   n1 = $builtins.hhbc_verify_type_pred($builtins.hack_int(42), n0)
        line: 9 column: 3 // .line 9
        line: 9           // .column 3
        line: 9 column: 3   ret $builtins.hack_int(42)
        line: 9 column: 3 }
        line: 9 column: 3
        line: 9 column: 3 // .file "level1.hack"
        line: 9 column: 3 // .line 12
        line:12           define $root.Level1::taintSink($this: *void, $args: *HackMixed) : *void {
        line:12           #b0:
        line:12           // .line 13
        line:13           // .column 2
        line:13 column: 2   ret null
        line:13 column: 2 }
        line:13 column: 2
        line:13 column: 2 // .file "level1.hack"
        line:13 column: 2 // .line 15
        line:15           define $root.Level1::basicFlowBad($this: *void) : *void {
        line:15           local $tainted: *void
        line:15           #b0:
        line:15           // .line 16
        line:16           // .column 14
        line:16 column:14   n0 = $root.Level1::taintSource(null)
        line:16 column:14 // .line 16
        line:16           // .column 3
        line:16 column: 3   store &$tainted <- n0: *HackMixed
        line:16 column: 3 // .line 17
        line:17           // .column 13
        line:17 column:13   n1: *HackMixed = load &$tainted
        line:17 column:13 // .line 17
        line:17           // .column 3
        line:17 column: 3   n2 = $root.Level1::taintSink(null, n1)
        line:17 column: 3 // .line 18
        line:18           // .column 2
        line:18 column: 2   ret null
        line:18 column: 2 }
        line:18 column: 2
        line:18 column: 2 // .file "level1.hack"
        line:18 column: 2 // .line 20
        line:20           define $root.Level1::basicFlowOk($this: *void, $untainted: .notnull *HackInt) : *void {
        line:20           #b0:
        line:20           // .line 21
        line:21           // .column 13
        line:21 column:13   n0: *HackMixed = load &$untainted
        line:21 column:13 // .line 21
        line:21           // .column 3
        line:21 column: 3   n1 = $root.Level1::taintSink(null, n0)
        line:21 column: 3 // .line 22
        line:22           // .column 2
        line:22 column: 2   ret null
        line:22 column: 2 }
        line:22 column: 2
        line:22 column: 2 // ----- EXTERNALS -----
        line:22 column: 2 declare $builtins.hhbc_is_type_int(...): *HackMixed
        line:22 column: 2 declare $builtins.hhbc_verify_type_pred(...): *HackMixed
        line:22 column: 2
        line:22 column: 2 // ----- BUILTIN DECLS STARTS HERE -----
        line:22 column: 2 declare $builtins.hack_int(int): *HackInt
        line:22 column: 2
        line:22 column: 2 // TEXTUAL UNIT END level1.hack |}]
  end )
