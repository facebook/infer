(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module CC = CongruenceClosureSolver
module Rewrite = CongruenceClosureRewrite

let parse_rules cc str_rules : Rewrite.Rule.t list =
  List.map str_rules ~f:(fun prog ->
      match Rewrite.parse_rule cc prog with
      | Ok rule ->
          rule
      | Error err ->
          L.die InternalError "%a" Rewrite.pp_parse_error err )


let gen_rules cc : Rewrite.Rule.t list =
  parse_rules cc [(* phi simplification *) "(@phi ?C ?X ?X) ==> ?X"]


let check_equivalence ?(debug = false) (proc1 : Textual.ProcDesc.t) (proc2 : Textual.ProcDesc.t) =
  let cc = CC.init ~debug:false in
  match (TextualPeg.convert_proc cc proc1, TextualPeg.convert_proc cc proc2) with
  | Ok (atom1, eqs1), Ok (atom2, eqs2) ->
      let rules = gen_rules cc in
      let _rounds = Rewrite.Rule.full_rewrite ~debug cc rules in
      let res = CC.is_equiv cc atom1 atom2 in
      if debug && not res then (
        F.printf "=== Procedure 1 equations ===@." ;
        TextualPeg.Equations.pp cc F.std_formatter eqs1 ;
        F.printf "@.=== Procedure 2 equations ===@." ;
        TextualPeg.Equations.pp cc F.std_formatter eqs2 ;
        F.printf "@.NOT EQUIVALENT@." ;
        F.printf "atom1: %a@." (CC.pp_nested_term cc) atom1 ;
        F.printf "atom2: %a@." (CC.pp_nested_term cc) atom2 ) ;
      res
  | Error msg, _ | _, Error msg ->
      if debug then F.printf "PEG conversion failed: %s@." msg ;
      false


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
