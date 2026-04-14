(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module CC = CongruenceClosureSolver

let convert_and_print ?(debug = false) text =
  let sourcefile = Textual.SourceFile.create "test.sil" in
  match TextualParser.parse_string sourcefile text with
  | Ok module_ ->
      let procs =
        List.filter_map module_.decls ~f:(fun decl ->
            match decl with Textual.Module.Proc p -> Some p | _ -> None )
      in
      List.iter procs ~f:(fun (proc : Textual.ProcDesc.t) ->
          let cc = CC.init ~debug:false in
          match TextualPeg.convert_proc cc proc with
          | Ok (root, eqs) ->
              let name =
                F.asprintf "%a" Textual.QualifiedProcName.pp proc.procdecl.qualified_name
              in
              F.printf "=== %s ===@." name ;
              F.printf "Equations:@." ;
              TextualPeg.Equations.pp cc F.std_formatter eqs ;
              F.printf "PEG: %a@.@." (CC.pp_nested_term cc) root
          | Error msg ->
              F.printf "Error: %s@." msg ) ;
      if debug then ()
  | Error errs ->
      List.iter errs ~f:(fun e ->
          F.printf "Parse error: %a@." (TextualParser.pp_error sourcefile) e )
