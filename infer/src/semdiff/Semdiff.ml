(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let semdiff_with_eqsat ~debug ~previous_file ~current_file previous_src current_src =
  let parse = PythonSourceAst.build_parser () in
  match (parse ~filename:previous_file previous_src, parse ~filename:current_file current_src) with
  | Error error, _ | Ok _, Error error ->
      L.user_error "%a" PythonSourceAst.pp_error error ;
      []
  | Ok ast1, Ok ast2 ->
      if PythonSourceAstDiff.check_equivalence ~debug ast1 ast2 then [] else [Diff.dummy_explicit]


let python_ast_diff ~debug ~config ?filename1 ?filename2 previous_content current_content =
  let parse = PythonSourceAst.build_parser () in
  match (parse ?filename:filename1 previous_content, parse ?filename:filename2 current_content) with
  | Error error, _ | Ok _, Error error ->
      L.user_error "%a" PythonSourceAst.pp_error error ;
      []
  | Ok ast1, Ok ast2 ->
      SemdiffDirectEngine.ast_diff ~debug ~config ~previous_content ~current_content ast1 ast2


let hack_ast_diff ~debug ~config ~previous_file ~current_file previous_content current_content =
  let ast1 = HackSourceAst.parse_file previous_file in
  let ast2 = HackSourceAst.parse_file current_file in
  SemdiffDirectEngine.ast_diff ~debug ~config ~previous_content ~current_content ast1 ast2


let is_hack_file filename =
  String.is_suffix filename ~suffix:".php" || String.is_suffix filename ~suffix:".hack"


let resolve_config ~config_files ~is_hack =
  match config_files with
  | [] ->
      if is_hack then HackSemdiffConfig.hack_type_annotations_config
      else PythonSemdiffConfig.missing_python_type_annotations_config
  | files ->
      List.map files ~f:PythonConfigParser.parse_file
      |> List.reduce_exn ~f:SemdiffDirectEngine.Rules.union


let semdiff ~config_files ~previous_file ~current_file =
  let debug = Config.debug_mode in
  let previous_src = In_channel.with_file previous_file ~f:In_channel.input_all in
  let current_src = In_channel.with_file current_file ~f:In_channel.input_all in
  let diffs =
    if Config.semdiff_experimental_eqsat_engine then
      semdiff_with_eqsat ~debug ~previous_file ~current_file previous_src current_src
    else
      let config = resolve_config ~config_files ~is_hack:(is_hack_file current_file) in
      if is_hack_file current_file then
        hack_ast_diff ~debug ~config ~previous_file ~current_file previous_src current_src
      else
        python_ast_diff ~debug ~config ~filename1:previous_file ~filename2:current_file previous_src
          current_src
  in
  let out_path = ResultsDir.get_path SemDiff in
  Diff.write_json ~previous_file ~current_file ~out_path diffs


let extract_procs (module_ : Textual.Module.t) =
  List.filter_map module_.decls ~f:(fun decl ->
      match decl with Textual.Module.Proc p -> Some p | _ -> None )


let proc_fun_name (proc : Textual.ProcDesc.t) =
  Textual.ProcName.to_string proc.procdecl.qualified_name.name


let is_module_body name = String.is_substring name ~substring:"__module_body__"

let semdiff_b007_textual ~debug (module_old : Textual.Module.t) (module_new : Textual.Module.t) =
  let procs_old = extract_procs module_old in
  let procs_new = extract_procs module_new in
  let all_accepted =
    List.for_all procs_old ~f:(fun proc_old ->
        let name = proc_fun_name proc_old in
        if is_module_body name then true
        else
          match List.find procs_new ~f:(fun p -> String.equal (proc_fun_name p) name) with
          | None ->
              if debug then L.user_error "Procedure %s not found in current file@." name ;
              true (* procedure removed — not a migration concern *)
          | Some proc_new -> (
            match TextualPegDiff.check_b007_migration ~debug proc_old proc_new with
            | result ->
                result
            | exception CongruenceClosureRewrite.Rule.FuelExhausted _ ->
                if debug then L.user_error "Fuel exhausted for procedure %s@." name ;
                false ) )
  in
  if all_accepted then [] else [Diff.dummy_explicit]


let semdiff_from_json ~config_files json_path =
  let debug = Config.debug_mode in
  let input = Semdiff_batch_j.semdiff_input_of_string (In_channel.read_all json_path) in
  let is_hack = String.equal input.language "hack" in
  let config = resolve_config ~config_files ~is_hack in
  let results =
    List.map input.pairs ~f:(fun (pair : Semdiff_batch_t.semdiff_pair) ->
        let parse_side (side : Semdiff_batch_t.semdiff_file) =
          (side.filename, side.source, PythonSourceAst.Node.of_yojson side.ast)
        in
        let prev_file, prev_src, prev_ast = parse_side pair.previous in
        let curr_file, curr_src, curr_ast = parse_side pair.current in
        match
          if Config.semdiff_experimental_eqsat_engine then
            if PythonSourceAstDiff.check_equivalence ~debug prev_ast curr_ast then []
            else [Diff.dummy_explicit]
          else
            SemdiffDirectEngine.ast_diff ~debug ~config ~previous_content:prev_src
              ~current_content:curr_src prev_ast curr_ast
        with
        | diffs ->
            Diff.pair_to_json ~previous_file:prev_file ~current_file:curr_file diffs
        | exception CongruenceClosureRewrite.Rule.FuelExhausted _ ->
            Diff.pair_to_json_with_outcome ~previous_file:prev_file ~current_file:curr_file
              ~outcome:"fuel_exhausted" )
  in
  let out_path = ResultsDir.get_path SemDiff in
  Out_channel.with_file out_path ~f:(fun oc -> Yojson.Safe.to_channel oc (`List results))
