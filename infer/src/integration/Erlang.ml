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


let parse_translate_store ?(base_dir = None) result_dir =
  let otp_modules_file = Filename.concat result_dir "otp_modules.list" in
  let otp_modules =
    match Utils.read_file otp_modules_file with
    | Ok modules ->
        String.Set.of_list modules
    | Error err ->
        L.die InternalError "Error while loading list of OTP modules from file %s: %s@."
          otp_modules_file err
  in
  let process_one_ast json =
    match ErlangJsonParser.to_module json with
    | None ->
        false
    | Some ast -> (
        let env = ErlangEnvironment.initialize_environment ast otp_modules in
        match ErlangAstValidator.validate env ast with
        | true ->
            ErlangScopes.annotate_scopes env ast ;
            ErlangTranslator.translate_module env ast base_dir ;
            true
        | false ->
            L.internal_error "Ignoring module %s due to invalid AST.@\n" env.current_module ;
            false )
  in
  let process_one_file json_file =
    if should_process json_file then (
      let t0 = Mtime_clock.now () in
      let status = Filename.basename json_file in
      !ProcessPoolState.update_status t0 status ;
      match Utils.read_json_file json_file with
      | Ok json ->
          if not (process_one_ast json) then
            L.debug Capture Verbose "Failed to parse %s@." json_file
      | Error error ->
          L.internal_error "E: %s@." error ) ;
    None
  in
  let tasks () =
    ProcessPool.TaskGenerator.of_list (Utils.directory_fold (fun l p -> p :: l) [] result_dir)
  in
  Tasks.Runner.create ~jobs:Config.jobs ~child_prologue:ignore ~f:process_one_file
    ~child_epilogue:ignore tasks
  |> Tasks.Runner.run |> ignore


let capture ~command ~args =
  let base_dir = Utils.filename_to_relative (Unix.getcwd ()) ~root:Config.project_root in
  Option.iter ~f:(parse_translate_store ~base_dir) Config.erlang_ast_dir ;
  if not Config.erlang_skip_compile then (
    let in_dir = ResultsDir.get_path Temporary in
    let result_dir = Filename.temp_dir ~in_dir command "infer" in
    run_compile Config.erlang_with_otp_specs command result_dir args ;
    parse_translate_store ~base_dir result_dir ;
    if not Config.debug_mode then Utils.rmtree result_dir )


let add_or_get_build_report_path args =
  let split_on s list = List.split_while list ~f:(String.equal s |> Fn.non) in
  let split_on_build args =
    let before_build, from_build = split_on "build" args in
    match List.tl from_build with
    | None ->
        L.die UserError "Only 'build' command of buck is supported."
    | Some after_build ->
        (before_build, after_build)
  in
  let add_or_get args =
    let before_option, from_option = split_on "--build-report" args in
    match from_option with
    | [] ->
        let build_report_path =
          Filename.temp_file ~in_dir:(ResultsDir.get_path Temporary) "build-report" ".json"
        in
        (before_option @ ["--build-report"; build_report_path], build_report_path)
    | "--build-report" :: existing_path :: _ ->
        (args, existing_path)
    | ["--build-report"] ->
        L.die UserError "--build-report should be followed by a path"
    | _ ->
        L.die InternalError "Can't happen"
  in
  let before_build, after_build = split_on_build args in
  let after_build, build_report_path = add_or_get after_build in
  (build_report_path, before_build @ ["build"] @ after_build)


let save_beams_from_report ~project_root ~report beam_list_path =
  let rec get_strings under_outputs strings json =
    match json with
    | `String s when under_outputs ->
        Set.add strings s
    | `List jsons ->
        List.fold ~init:strings ~f:(get_strings under_outputs) jsons
    | `Assoc tagged_jsons ->
        let f strings (tag, json) =
          get_strings (under_outputs || String.equal tag "outputs") strings json
        in
        List.fold ~init:strings ~f tagged_jsons
    | _ ->
        strings
  in
  let get_beams beams dir =
    let ebin_dir = project_root ^/ dir ^/ "ebin" in
    match Sys.is_directory ebin_dir with
    | `Yes ->
        let new_beams = Utils.find_files ~path:ebin_dir ~extension:".beam" in
        Set.union beams (String.Set.of_list new_beams)
    | _ ->
        beams
  in
  let all = get_strings false String.Set.empty report in
  let beams = Set.fold ~init:String.Set.empty ~f:get_beams all in
  Out_channel.write_lines beam_list_path (Set.elements beams)


let run_in_dir ~dir ~prog ~args =
  let here = Sys.getcwd () in
  Sys.chdir dir ;
  let _ignore_err = Process.create_process_and_wait_with_output ~prog ~args ReadStderr in
  Sys.chdir here


let process_beams ~project_root beam_list_path =
  let base_dir = Some project_root in
  Option.iter ~f:(parse_translate_store ~base_dir) Config.erlang_ast_dir ;
  if not Config.erlang_skip_compile then (
    let jsonast_dir =
      let in_dir = ResultsDir.get_path Temporary in
      Filename.realpath (Filename.temp_dir ~in_dir "buck2-erlang" "infer")
    in
    let prog = Config.lib_dir ^/ "erlang" ^/ "extract.escript" in
    let args =
      if Config.erlang_with_otp_specs then
        ["--list"; beam_list_path; "--specs-only"; "--otp"; jsonast_dir]
      else ["--list"; beam_list_path; jsonast_dir]
    in
    (* extract.escript prints a warning to stdout if the abstract forms are missing,
       but keeps going *)
    run_in_dir ~dir:project_root ~prog ~args ;
    parse_translate_store ~base_dir jsonast_dir ;
    if not Config.debug_mode then Utils.rmtree jsonast_dir )


let parse_buck_arguments args =
  let is_option s = Caml.String.starts_with ~prefix:"--" s in
  let global_options, args = List.split_while args ~f:is_option in
  let args =
    match args with
    | "build" :: rest ->
        rest
    | _ ->
        L.die UserError "Expecting: [GLOBALOPTIONS] build [BUILDOPTIONS] TARGETS"
  in
  let build_options, targets = List.partition_tf args ~f:is_option in
  let build_options = List.filter ~f:(Fn.non (String.equal "--")) build_options in
  (global_options, build_options, targets)


let parse_dependencies buck_query_output =
  let parse_line line =
    match String.split_on_chars ~on:[' '] line with
    | "" :: _ ->
        L.external_warning "@[<v>@[Unexpected query result@]@;@]" ;
        None
    | target :: _ ->
        Some target
    | [] ->
        L.die InternalError "split_on_chars never returns empty list"
  in
  List.filter_map buck_query_output ~f:parse_line


(* Removes duplicates. Also, if both A/... and A/B are targets, keep only the former.
   Finally, if both A: and A:B are targets, keep only the former. *)
let simplify_targets targets =
  let targets = Set.elements (String.Set.of_list targets) in
  let ellipsis_suffix = "/..." in
  let ellipsis_patterns, targets =
    List.partition_tf ~f:(Caml.String.ends_with ~suffix:ellipsis_suffix) targets
  in
  let colon_patterns, targets = List.partition_tf ~f:(Caml.String.ends_with ~suffix:":") targets in
  let make_regex len patterns =
    match patterns with
    | [] ->
        Str.regexp "$" (* something that does not match targets *)
    | _ ->
        let regexps =
          let f p = String.concat ["\\("; String.drop_suffix p len; "\\)"] in
          List.map ~f patterns
        in
        Str.regexp (String.concat ~sep:"\\|" regexps)
  in
  let in_ellipsis = make_regex (String.length ellipsis_suffix - 1) ellipsis_patterns in
  let filter regex patterns = List.filter ~f:(fun x -> not (Str.string_match regex x 0)) patterns in
  let colon_patterns = filter in_ellipsis colon_patterns in
  let targets = filter in_ellipsis targets in
  let in_colon = make_regex 0 colon_patterns in
  let targets = filter in_colon targets in
  ellipsis_patterns @ colon_patterns @ targets


let run_buck ~command ~args =
  let args_file = Filename.temp_file ~in_dir:(ResultsDir.get_path Temporary) "buck" ".args" in
  Utils.with_file_out args_file ~f:(fun channel -> Out_channel.output_lines channel args) ;
  Buck.wrap_buck_call ~label:"erlang" V2 [command; "@" ^ args_file]


let update_buck_targets ~command ~args =
  (* Precondition: [args] contains "[GLOBALOPTS] build [BUILDOPTS] [TARGETS]" *)
  let global_options, build_options, old_targets = parse_buck_arguments args in
  let query =
    global_options @ ["cquery"] @ build_options @ ["kind('erlang', deps(%s))"] @ old_targets
  in
  let query_result = run_buck ~command ~args:query in
  let new_targets = parse_dependencies query_result in
  let all_targets =
    simplify_targets (old_targets @ new_targets)
    (* to avoid long command lines *)
  in
  global_options @ ["build"] @ build_options @ all_targets


let capture_buck ~command ~args =
  let args = update_buck_targets ~command ~args in
  let build_report_path, args = add_or_get_build_report_path args in
  run_buck ~command ~args |> ignore ;
  let report, project_root =
    match Utils.read_json_file build_report_path with
    | Ok json ->
        let open Yojson.Safe.Util in
        let root = json |> member "project_root" |> to_string in
        (json, root)
    | Error message ->
        L.die InternalError "@[<v>@[Failed to parse Buck report: %s@]@;@]" message
  in
  let beam_list_path = ResultsDir.get_path Temporary ^/ "beams.list" in
  save_beams_from_report ~project_root ~report beam_list_path ;
  L.progress "translating generated beam files@." ;
  process_beams ~project_root beam_list_path
