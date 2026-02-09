(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let semdiff ~config_file ~previous_file ~current_file =
  let debug = Config.debug_mode in
  let previous_src = In_channel.with_file previous_file ~f:In_channel.input_all in
  let current_src = In_channel.with_file current_file ~f:In_channel.input_all in
  let config =
    Option.value_map config_file
      ~default:PythonCompareDirectRewrite.missing_python_type_annotations_config
      ~f:PythonConfigParser.parse_file
  in
  let diffs =
    PythonCompareDirectRewrite.ast_diff ~debug ~config ~filename1:previous_file
      ~filename2:current_file previous_src current_src
  in
  let out_path = ResultsDir.get_path SemDiff in
  Diff.write_json ~previous_file ~current_file ~out_path diffs
