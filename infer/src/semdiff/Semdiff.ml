(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let semdiff_with_eqsat ~previous_file ~current_file previous_src current_src =
  let parse = PythonSourceAst.build_parser () in
  match (parse ~filename:previous_file previous_src, parse ~filename:current_file current_src) with
  | Error error, _ | Ok _, Error error ->
      L.user_error "%a" PythonSourceAst.pp_error error ;
      []
  | Ok ast1, Ok ast2 ->
      if PythonSourceAstDiff.check_equivalence ~debug:false ast1 ast2 then []
      else [Diff.dummy_explicit]


let semdiff ~config_file ~previous_file ~current_file =
  let debug = Config.debug_mode in
  let previous_src = In_channel.with_file previous_file ~f:In_channel.input_all in
  let current_src = In_channel.with_file current_file ~f:In_channel.input_all in
  let diffs =
    if Config.semdiff_experimental_eqsat_engine then
      semdiff_with_eqsat ~previous_file ~current_file previous_src current_src
    else
      let config =
        Option.value_map config_file
          ~default:PythonCompareDirectRewrite.missing_python_type_annotations_config
          ~f:PythonConfigParser.parse_file
      in
      PythonCompareDirectRewrite.ast_diff ~debug ~config ~filename1:previous_file
        ~filename2:current_file previous_src current_src
  in
  let out_path = ResultsDir.get_path SemDiff in
  Diff.write_json ~previous_file ~current_file ~out_path diffs
