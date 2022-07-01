(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

let run_compile with_otp_specs command result_dir args =
  let args = [result_dir; "--"; command] @ args in
  let args = if with_otp_specs then ["--with_otp_specs"] @ args else args in
  let prog = Config.lib_dir ^/ "erlang" ^/ "erlang.sh" in
  L.debug Capture Verbose "executing %s@." prog ;
  ignore (Process.create_process_and_wait_with_output ~prog ~args ReadStdout)


let should_process filename =
  (* NOTE: [run_compile] makes the usual Erlang assumption that (a) modules have unique names, and
     (b) module X is in file X.erl. In particular, [run_compile] drops relative paths. So, here, we
     match [Config.skip_analysis_in_path] only against the filename, not all relative path. *)
  Filename.check_suffix filename ".json"
  &&
  match Config.skip_analysis_in_path with
  | None ->
      true
  | Some re ->
      let original_filename = Filename.chop_extension (Filename.basename filename) ^ ".erl" in
      not (Str.string_match re original_filename 0)


let parse_translate_store result_dir =
  let process_one_ast json =
    match ErlangJsonParser.to_module json with
    | None ->
        false
    | Some ast -> (
        let env = ErlangEnvironment.initialize_environment ast in
        match ErlangAstValidator.validate env ast with
        | true ->
            ErlangScopes.annotate_scopes env ast ;
            ErlangTranslator.translate_module env ast ;
            true
        | false ->
            L.debug Capture Verbose "Invalid AST@." ;
            false )
  in
  let process_one_file json_file =
    ( if should_process json_file then
      match Utils.read_safe_json_file json_file with
      | Ok json ->
          if not (process_one_ast json) then
            L.debug Capture Verbose "Failed to parse %s@." json_file
      | Error error ->
          L.internal_error "E: %s@." error ) ;
    None
  in
  Tasks.Runner.create ~jobs:Config.jobs
    ~child_prologue:(fun () -> ())
    ~f:process_one_file
    ~child_epilogue:(fun () -> ())
    ~tasks:(fun () ->
      ProcessPool.TaskGenerator.of_list (Utils.directory_fold (fun l p -> p :: l) [] result_dir) )
  |> Tasks.Runner.run |> ignore


let capture ~command ~args =
  Option.iter ~f:parse_translate_store Config.erlang_ast_dir ;
  if not Config.erlang_skip_compile then (
    let in_dir = ResultsDir.get_path Temporary in
    let result_dir = Filename.temp_dir ~in_dir (command ^ "infer") "" in
    run_compile Config.erlang_with_otp_specs command result_dir args ;
    parse_translate_store result_dir ;
    if not Config.debug_mode then Utils.rmtree result_dir )
