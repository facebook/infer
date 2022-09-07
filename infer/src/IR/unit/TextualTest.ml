(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Textual
module F = Format

let parse_module text =
  match TextualParser.parse_string (SourceFile.create "dummy.sil") text with
  | Ok m ->
      m
  | _ ->
      raise (Failure "Couldn't parse a module")


let%test_module "parsing" =
  ( module struct
    let text =
      {| 
       attribute source_language = "hack"
       attribute source_file = "original.hack"
       
       attribute source_language = "java" // Won't have an effect
       
       define nothing(): void {
         #node0:
           ret null
       }
       |}


    let%expect_test _ =
      let module_ = parse_module text in
      let attrs = module_.attrs in
      F.printf "%a" (Pp.seq ~sep:"\n" Attr.pp_with_loc) attrs ;
      [%expect
        {|
        line 2, column 7: source_language = "hack"
        line 3, column 7: source_file = "original.hack"
        line 5, column 7: source_language = "java" |}] ;
      let lang = Option.value_exn (Module.lang module_) in
      F.printf "%s" (Lang.to_string lang) ;
      [%expect {| hack |}]

    let%expect_test _ =
      let module_ = parse_module text in
      F.printf "%a" Module.pp module_ ;
      [%expect
        {|
          attribute source_language = "hack"

          attribute source_file = "original.hack"

          attribute source_language = "java"

          define nothing() : void {
            #node0:
                ret null

          } |}]
  end )

let%test_module "procnames" =
  ( module struct
    let%expect_test _ =
      let toplevel_proc =
        Procname.
          { qualified_name=
              { enclosing_class= TopLevel
              ; name= {value= "toplevel"; loc= Location.known ~line:0 ~col:0} }
          ; formals_types= []
          ; result_type= Typ.Void
          ; kind= NonVirtual }
      in
      let as_java = Procname.to_sil Lang.Java toplevel_proc in
      let as_hack = Procname.to_sil Lang.Hack toplevel_proc in
      F.printf "%a@\n" SilProcname.pp as_java ;
      F.printf "%a@\n" SilProcname.pp as_hack ;
      [%expect {|
        void $TOPLEVEL$CLASS$.toplevel()
        toplevel |}]
  end )

let%test_module "to_sil" =
  ( module struct
    let%expect_test _ =
      let no_lang = {|define nothing() : void { #node: ret null }|} in
      let m = parse_module no_lang in
      try Module.to_sil m |> ignore
      with ToSilTransformationError pp_msg ->
        pp_msg F.std_formatter () ;
        [%expect {| Missing or unsupported source_language attribute |}]
  end )
