(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

let max_command_line_length = 50

let store_args_in_file ~identifier args =
  let rec exceed_length ~max = function
    | _ when max < 0 ->
        true
    | [] ->
        false
    | h :: t ->
        exceed_length ~max:(max - String.length h) t
  in
  if exceed_length ~max:max_command_line_length args then (
    let file = IFilename.temp_file ~in_dir:(ResultsDir.get_path Temporary) identifier ".txt" in
    let write_args outc = Out_channel.output_string outc (String.concat ~sep:"\n" args) in
    Utils.with_file_out file ~f:write_args ;
    L.debug Capture Quiet "Buck targets options stored in file '%s'@\n" file ;
    [Printf.sprintf "@%s" file] )
  else args


let infer_vars_to_kill =
  [ CommandLineOption.infer_cwd_env_var
  ; CommandLineOption.args_env_var
  ; CommandLineOption.strict_mode_env_var
  ; CommandLineOption.infer_top_results_dir_env_var
  ; CommandDoc.inferconfig_env_var ]


(** Wrap a call to buck while (i) logging standard error to our standard error in real time; (ii)
    redirecting standard out to a file, the contents of which are returned. *)
let wrap_buck_call ~label cmd =
  let stdout_file =
    let prefix = Printf.sprintf "buck2_%s" label in
    IFilename.temp_file ~in_dir:(ResultsDir.get_path Temporary) prefix ".stdout"
  in
  let cmd =
    match (cmd, Config.buck2_isolation_dir) with
    | buck2 :: rest, Some isolation_dir ->
        buck2 :: ("--isolation-dir=" ^ isolation_dir) :: rest
    | _ ->
        cmd
  in
  let command =
    let escaped_cmd = List.map ~f:Escape.escape_shell cmd |> String.concat ~sep:" " in
    let cmd_with_output = Printf.sprintf "exec %s > '%s'" escaped_cmd stdout_file in
    cmd_with_output
  in
  let env = `Override (List.map infer_vars_to_kill ~f:(fun var -> (var, None))) in
  L.debug Capture Quiet "Running buck command '%s'@." command ;
  let IUnix.Process_info.{stdin; stdout; stderr; pid} =
    IUnix.create_process_env ~prog:"sh" ~args:["-c"; command] ~env
  in
  let buck_stderr = Unix.in_channel_of_descr stderr in
  let buck_logger = L.progress "BUCK2: %s@\n" in
  Utils.with_channel_in buck_stderr ~f:buck_logger ;
  Unix.close stdin ;
  Unix.close stdout ;
  In_channel.close buck_stderr ;
  match IUnix.waitpid pid with
  | Ok () -> (
    match Utils.read_file stdout_file with
    | Ok lines ->
        lines
    | Error err ->
        L.die ExternalError "*** failed to read output of buck command %s: %s" command err )
  | Error _ as err ->
      L.die ExternalError "*** failed to execute buck command %s: %s" command
        (IUnix.Exit_or_signal.to_string_hum err)


let parse_target_string =
  let alias_target_regexp = Str.regexp "^[^/:]+\\(#.*\\)?$" in
  let pattern_target_regexp = Str.regexp "^[^/]*//\\(\\.\\.\\.\\|.*\\(:\\|/\\.\\.\\.\\)\\)$" in
  let normal_target_regexp = Str.regexp "^[^/]*//[^/].*:.+$" in
  let noname_target_regexp = Str.regexp "^[^/]*//.*$" in
  let parse_with_retry s ~retry =
    (* do not consider --buck-options as targets *)
    if String.equal s "" || Char.equal s.[0] '-' || Char.equal s.[0] '@' then `NotATarget s
    else if Str.string_match alias_target_regexp s 0 then `AliasTarget s
    else if Str.string_match pattern_target_regexp s 0 then `PatternTarget s
    else if Str.string_match normal_target_regexp s 0 then `NormalTarget s
    else if Str.string_match noname_target_regexp s 0 then
      let name = String.split s ~on:'/' |> List.last_exn in
      if Char.equal name.[0] ':' then `NormalTarget s else `NormalTarget (F.sprintf "%s:%s" s name)
    else retry s
  in
  fun s ->
    parse_with_retry s ~retry:(fun s ->
        parse_with_retry ("//" ^ s) ~retry:(fun s ->
            L.(die InternalError) "Do not know how to parse buck command line argument '%s'" s ) )


module Query = struct
  type expr =
    | Deps of {depth: int option; expr: expr}
    | Kind of {pattern: string; expr: expr}
    | Set of string list
    | Target of string
    | Union of expr list
    | Labelfilter of {label: string; expr: expr}

  exception NotATarget

  let quote_if_needed =
    let no_quote_needed_regexp = Str.regexp "^[a-zA-Z0-9/:_*][-a-zA-Z0-9/:._*]*$" in
    fun s ->
      if Str.string_match no_quote_needed_regexp s 0 then s
      else s |> Escape.escape_double_quotes |> F.sprintf "\"%s\""


  let target string = Target (quote_if_needed string)

  let kind ~pattern expr = Kind {pattern= quote_if_needed pattern; expr}

  let deps depth expr = Deps {depth; expr}

  let set exprs =
    match List.rev_map exprs ~f:(function Target t -> t | _ -> raise NotATarget) with
    | targets ->
        Set targets
    | exception NotATarget ->
        Union exprs


  let label_filter ~label expr = Labelfilter {label; expr}

  let rec pp fmt = function
    | Target s ->
        F.pp_print_string fmt s
    | Kind {pattern; expr} ->
        F.fprintf fmt "kind(%s, %a)" pattern pp expr
    | Deps {depth= None; expr} ->
        F.fprintf fmt "deps(%a)" pp expr (* full depth *)
    | Deps {depth= Some depth; expr} ->
        F.fprintf fmt "deps(%a, %d)" pp expr depth
    | Set sl ->
        F.fprintf fmt "set(%a)" (Pp.seq F.pp_print_string) sl
    | Union exprs ->
        Pp.seq ~sep:" + " pp fmt exprs
    | Labelfilter {label; expr} ->
        F.fprintf fmt "attrfilter(labels, %s, %a)" label pp expr


  let exec expr =
    let query = F.asprintf "%a" pp expr in
    let bounded_args = store_args_in_file ~identifier:"buck_query_args" [query] in
    let extra_buck_args = Config.buck2_build_args in
    let cmd = "buck2 cquery" :: (extra_buck_args @ bounded_args) in
    wrap_buck_call ~label:"query" cmd
end

let accepted_buck_commands = ["build"]

let parameters_with_argument =
  [ "--buck-binary"
  ; "--build-report"
  ; "--build-state-file"
  ; "-c"
  ; "--config"
  ; "--config-file"
  ; "-j"
  ; "--just-build"
  ; "--num-threads"
  ; "--out"
  ; "--output-events-to-file"
  ; "--output-test-events-to-file"
  ; "--rulekeys-log-path"
  ; "--target-platforms"
  ; "-v"
  ; "--verbose" ]


let get_accepted_buck_kinds_pattern (mode : BuckMode.t) =
  match mode with
  | ClangCompilationDB _ ->
      "^(apple|cxx)_(binary|library|test)$"
  | Clang ->
      "^(apple|cxx)_(binary|library)$"
  | Erlang ->
      L.die InternalError "Not used"
  | Java ->
      "^(java|android)_library$"
  | Python ->
      "^python_(binary|library|test)$"


let resolve_pattern_targets (buck_mode : BuckMode.t) targets =
  let target_set = List.rev_map targets ~f:Query.target |> Query.set in
  let deps_query =
    match buck_mode with
    | Clang | Python ->
        if Config.buck2_query_deps then Query.deps Config.buck_dependency_depth else Fn.id
    | ClangCompilationDB NoDependencies | Erlang ->
        Fn.id
    | Java ->
        Query.deps Config.buck_dependency_depth
    | ClangCompilationDB DepsAllDepths ->
        Query.deps None
    | ClangCompilationDB (DepsUpToDepth depth) ->
        Query.deps (Some depth)
  in
  let accepted_patterns_filter = Query.kind ~pattern:(get_accepted_buck_kinds_pattern buck_mode) in
  let buck2_java_infer_enabled_filter =
    match buck_mode with Java -> Query.label_filter ~label:"infer_enabled" | _ -> Fn.id
  in
  let results =
    target_set |> deps_query |> accepted_patterns_filter |> buck2_java_infer_enabled_filter
    |> Query.exec
  in
  match buck_mode with Java -> List.rev_map results ~f:(fun s -> s ^ "_infer") | _ -> results


type parsed_args =
  { rev_not_targets': string list
  ; normal_targets: string list
  ; alias_targets: string list
  ; pattern_targets: string list }

let empty_parsed_args =
  {rev_not_targets'= []; normal_targets= []; alias_targets= []; pattern_targets= []}


let split_buck_command buck_cmd =
  match buck_cmd with
  | command :: args when List.mem ~equal:String.equal accepted_buck_commands command ->
      (command, args)
  | _ ->
      L.(die UserError)
        "ERROR: cannot parse buck command `%a`. Expected %a." (Pp.seq F.pp_print_string) buck_cmd
        (Pp.seq ~sep:" or " F.pp_print_string)
        accepted_buck_commands


let parse_command_and_targets =
  let buck_targets_block_list_regexp =
    if List.is_empty Config.buck_targets_block_list then None
    else
      Some
        (Str.regexp
           ("\\(" ^ String.concat ~sep:"\\)\\|\\(" Config.buck_targets_block_list ^ "\\)") )
  in
  fun (buck_mode : BuckMode.t) original_buck_args ->
    let expanded_buck_args = Utils.inline_argument_files original_buck_args in
    let command, args = split_buck_command expanded_buck_args in
    let rec parse_cmd_args parsed_args = function
      | [] ->
          parsed_args
      | param :: arg :: args when List.mem ~equal:String.equal parameters_with_argument param ->
          parse_cmd_args
            {parsed_args with rev_not_targets'= arg :: param :: parsed_args.rev_not_targets'}
            args
      | target :: args ->
          let parsed_args =
            match parse_target_string target with
            | `NotATarget s ->
                {parsed_args with rev_not_targets'= s :: parsed_args.rev_not_targets'}
            | `NormalTarget t ->
                {parsed_args with normal_targets= t :: parsed_args.normal_targets}
            | `AliasTarget a ->
                {parsed_args with alias_targets= a :: parsed_args.alias_targets}
            | `PatternTarget p ->
                {parsed_args with pattern_targets= p :: parsed_args.pattern_targets}
          in
          parse_cmd_args parsed_args args
    in
    let parsed_args = parse_cmd_args empty_parsed_args args in
    let targets =
      match (buck_mode, parsed_args) with
      | Clang, {pattern_targets; alias_targets; normal_targets} ->
          pattern_targets |> List.rev_append alias_targets |> List.rev_append normal_targets
      | _, {pattern_targets; alias_targets; normal_targets} ->
          pattern_targets |> List.rev_append alias_targets |> List.rev_append normal_targets
          |> resolve_pattern_targets buck_mode
    in
    let targets =
      Option.value_map ~default:targets
        ~f:(fun re -> List.filter ~f:(fun tgt -> not (Str.string_match re tgt 0)) targets)
        buck_targets_block_list_regexp
    in
    StatsLogging.log_count ~label:"buck_targets" ~value:(List.length targets) ;
    (command, parsed_args.rev_not_targets', targets)
