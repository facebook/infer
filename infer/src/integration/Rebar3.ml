(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

let run_rebar result_dir args =
  let args = [result_dir; "--"; "rebar3"] @ args in
  let prog = Config.lib_dir ^/ "erlang" ^/ "erlang.sh" in
  L.debug Capture Verbose "executing %s@." prog ;
  Process.create_process_and_wait ~prog ~args


let parse_and_store result_dir =
  let process json =
    match ErlangJsonParser.to_module json with
    | None ->
        false
    | Some ast ->
        let source_file, cfg = ErlangTranslator.to_source_and_cfg ast in
        let tenv = (* TODO: types *) Tenv.Global in
        SourceFiles.add source_file cfg tenv None ;
        true
  in
  let log error = L.progress "E: %s@." error in
  let read_one_ast json_file =
    if Filename.check_suffix json_file ".json" then (
      L.progress "P: parsing %s@." json_file ;
      match Utils.read_safe_json_file json_file with
      | Ok json ->
          if not (process json) then L.debug Capture Verbose "Failed to parse %s@." json_file
      | Error error ->
          log error )
  in
  Utils.directory_iter read_one_ast result_dir


let capture ~args =
  Option.iter ~f:parse_and_store Config.erlang_ast_dir ;
  if not Config.erlang_skip_rebar3 then (
    let in_dir = ResultsDir.get_path Temporary in
    let rebar_result_dir = Filename.temp_dir ~in_dir "rebar3infer" "" in
    run_rebar rebar_result_dir args ;
    parse_and_store rebar_result_dir ;
    if not Config.debug_mode then Utils.rmtree rebar_result_dir )
