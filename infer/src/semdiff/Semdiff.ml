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


let semdiff ~config_files ~previous_file ~current_file =
  let debug = Config.debug_mode in
  let previous_src = In_channel.with_file previous_file ~f:In_channel.input_all in
  let current_src = In_channel.with_file current_file ~f:In_channel.input_all in
  let diffs =
    if Config.semdiff_experimental_eqsat_engine then
      semdiff_with_eqsat ~debug ~previous_file ~current_file previous_src current_src
    else
      let default_config =
        if is_hack_file current_file then HackSemdiffConfig.hack_type_annotations_config
        else PythonSemdiffConfig.missing_python_type_annotations_config
      in
      let config =
        match config_files with
        | [] ->
            default_config
        | files ->
            List.map files ~f:PythonConfigParser.parse_file
            |> List.reduce_exn ~f:SemdiffDirectEngine.Rules.union
      in
      if is_hack_file current_file then
        hack_ast_diff ~debug ~config ~previous_file ~current_file previous_src current_src
      else
        python_ast_diff ~debug ~config ~filename1:previous_file ~filename2:current_file previous_src
          current_src
  in
  let out_path = ResultsDir.get_path SemDiff in
  Diff.write_json ~previous_file ~current_file ~out_path diffs
