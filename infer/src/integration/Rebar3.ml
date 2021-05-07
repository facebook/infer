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


let capture ~args =
  let in_dir = ResultsDir.get_path Temporary in
  let rebar_result_dir = Filename.temp_dir ~in_dir "rebar3infer" "" in
  run_rebar rebar_result_dir args ;
  (* TODO: parse the JSON files *)
  if not Config.debug_mode then Utils.rmtree rebar_result_dir
