(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

type version = V1 | V2

let binary_of_version = function V1 -> "buck1" | V2 -> "buck2"

let max_command_line_length = 50

let buck_extra_java_args_env_var = "BUCK_EXTRA_JAVA_ARGS"

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
    let file = Filename.temp_file ~in_dir:(ResultsDir.get_path Temporary) identifier ".txt" in
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
    redirecting standard out to a file, the contents of which are returned; (iii) protect the child
    process from [SIGQUIT].

    We want to ihnibit [SIGQUIT] because the standard action of the JVM is to print a thread dump on
    stdout, polluting the output we want to collect (and normally does not lead to process death).
    To achieve this we need to do two things: (i) tell the JVM not to use signals, meaning it leaves
    the default handler for [SIGQUIT] in place; (ii) uninstall the default handler for [SIGQUIT]
    because now that the JVM doesn't touch it, it will lead to process death. *)
let wrap_buck_call ?(extend_env = []) version ~label cmd =
  let is_buck2 = match (version : version) with V1 -> false | V2 -> true in
  let stdout_file =
    let prefix = Printf.sprintf "%s_%s" (binary_of_version version) label in
    Filename.temp_file ~in_dir:(ResultsDir.get_path Temporary) prefix ".stdout"
  in
  let cmd =
    match (cmd, Config.buck2_isolation_dir) with
    | buck2 :: rest, Some isolation_dir when is_buck2 ->
        buck2 :: ("--isolation-dir=" ^ isolation_dir) :: rest
    | _ ->
        cmd
  in
  let command =
    let escaped_cmd = List.map ~f:Escape.escape_shell cmd |> String.concat ~sep:" " in
    let cmd_with_output = Printf.sprintf "exec %s > '%s'" escaped_cmd stdout_file in
    if is_buck2 then cmd_with_output
    else
      (* Uninstall the default handler for [SIGQUIT]. *)
      Printf.sprintf "trap '' SIGQUIT ; %s" cmd_with_output
  in
  let env_vars =
    if is_buck2 then []
    else
      let explicit_buck_java_heap_size =
        Option.map Config.buck_java_heap_size_gb ~f:(fun size -> Printf.sprintf "-Xmx%dG" size)
        |> Option.to_list
      in
      let existing_buck_extra_java_args =
        Sys.getenv buck_extra_java_args_env_var |> Option.to_list
      in
      let new_buck_extra_java_args =
        (* Instruct the JVM to avoid using signals. *)
        String.concat ~sep:" "
          (existing_buck_extra_java_args @ explicit_buck_java_heap_size @ ["-Xrs"])
      in
      L.environment_info "Buck: setting %s to '%s'@\n" buck_extra_java_args_env_var
        new_buck_extra_java_args ;
      (buck_extra_java_args_env_var, new_buck_extra_java_args) :: extend_env
  in
  let env =
    if is_buck2 then
      `Override
        ( List.map infer_vars_to_kill ~f:(fun var -> (var, None))
        @ List.map env_vars ~f:(fun (lhs, rhs) -> (lhs, Some rhs)) )
    else `Extend env_vars
  in
  L.debug Capture Quiet "Running buck command '%s'@." command ;
  let Unix.Process_info.{stdin; stdout; stderr; pid} =
    Unix.create_process_env ~prog:"sh" ~args:["-c"; command] ~env ()
  in
  let buck_stderr = Unix.in_channel_of_descr stderr in
  let buck_logger = if is_buck2 then L.progress "BUCK2: %s@\n" else L.progress "BUCK: %s@\n" in
  Utils.with_channel_in buck_stderr ~f:buck_logger ;
  Unix.close stdin ;
  Unix.close stdout ;
  In_channel.close buck_stderr ;
  match Unix.waitpid pid with
  | Ok () -> (
    match Utils.read_file stdout_file with
    | Ok lines ->
        lines
    | Error err ->
        L.die ExternalError "*** failed to read output of buck command %s: %s" command err )
  | Error _ as err ->
      L.die ExternalError "*** failed to execute buck command %s: %s" command
        (Unix.Exit_or_signal.to_string_hum err)


module Target = struct
  type t = {name: string; flavors: string list}

  let of_string target =
    match String.split target ~on:'#' with
    | [name; flavors_string] ->
        let flavors = String.split flavors_string ~on:',' in
        {name; flavors}
    | [name] ->
        {name; flavors= []}
    | _ ->
        L.(die ExternalError) "cannot parse target %s" target


  let to_string {name; flavors} = F.asprintf "%s#%a" name (Pp.comma_seq F.pp_print_string) flavors

  let add_flavor_internal target flavor =
    if List.mem ~equal:String.equal target.flavors flavor then
      (* there's already an infer flavor associated to the target, do nothing *)
      target
    else {target with flavors= flavor :: target.flavors}


  let add_flavor_v1 (mode : BuckMode.t) (command : InferCommand.t) ~extra_flavors target =
    let target = List.fold_left ~f:add_flavor_internal ~init:target extra_flavors in
    match (mode, command) with
    | ClangCompilationDB _, _ ->
        add_flavor_internal target "compilation-database"
    | Clang, Compile | Erlang, _ ->
        target
    | Clang, _ ->
        add_flavor_internal target "infer-capture-all"
    | Java, _ ->
        add_flavor_internal target "infer-java-capture"
end

let config =
  let clang_path =
    List.fold ["clang"; "install"; "bin"; "clang"] ~init:Config.fcp_dir ~f:Filename.concat
  in
  let get_java_flavor_config () =
    if Config.buck_java_suppress_config then []
    else ["infer.version=" ^ Version.versionString; "infer.binary=" ^ Config.infer_binary]
  in
  let get_clang_flavor_config () =
    [ "client.id=infer.clang"
    ; "*//cxx.pch_enabled=false"
    ; (* Infer doesn't support C++ modules yet (T35656509) *)
      "*//cxx.modules_default=false"
    ; "*//cxx.modules=false" ]
    @ ( if Config.buck_clang_use_toolchain_config then []
        else
          [ "*//infer.infer_bin=" ^ Config.bin_dir
          ; "*//infer.binary=" ^ Config.infer_binary
          ; "*//infer.clang_compiler=" ^ clang_path
          ; "*//infer.clang_plugin=" ^ Config.clang_plugin_path ] )
    @ ( match Config.xcode_developer_dir with
      | Some d ->
          [Printf.sprintf "apple.xcode_developer_dir=%s" d]
      | None ->
          [] )
    @
    if List.is_empty Config.buck_block_list then []
    else
      [ Printf.sprintf "*//infer.block_list_regex=(%s)"
          (String.concat ~sep:")|(" Config.buck_block_list) ]
  in
  fun buck_mode version ->
    let args =
      match ((buck_mode : BuckMode.t), (version : version)) with
      | Java, V1 ->
          get_java_flavor_config ()
      | Clang, V1 ->
          get_clang_flavor_config ()
      | _, _ ->
          []
    in
    List.fold args ~init:[] ~f:(fun acc f -> "--config" :: f :: acc)


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


  (* example query json output
     [
     {
       "//example/foo:bar" : {
         "srcs" : [ "example/foo/bar.cc", "example/foo/lib/lib.cc" ]
       }
       "//example/foo:main" : {}
     }
     ]
  *)
  let parse_query_output ~buck_mode (version : version) output =
    let get_target_srcs_assoc_list json =
      match json with
      | `Assoc fields ->
          fields
      | _ ->
          L.internal_error "Could not parse target json: %s@\n" (Yojson.Basic.to_string json) ;
          []
    in
    let get_json_field fieldname = function
      | `Assoc fields ->
          List.Assoc.find fields ~equal:String.equal fieldname
      | _ ->
          None
    in
    let is_valid_source source_path_json =
      match source_path_json with
      | `String source_path ->
          ( String.is_suffix ~suffix:".java" source_path
          || Config.kotlin_capture
             && Filename.check_suffix source_path Config.kotlin_source_extension )
          && not (String.is_suffix ~suffix:"MetagenRoot.java" source_path)
      | _ ->
          L.internal_error "Could not parse source path json: %s@\n"
            (Yojson.Basic.to_string source_path_json) ;
          false
    in
    let process_target acc (target, json) =
      match get_json_field "srcs" json with
      | Some (`List srcs) when List.exists srcs ~f:is_valid_source ->
          target :: acc
      | _ ->
          acc
    in
    match ((buck_mode : BuckMode.t), version) with
    | Java, V1 ->
        String.concat output |> Yojson.Basic.from_string |> get_target_srcs_assoc_list
        |> List.fold ~init:[] ~f:process_target
    | _ ->
        output


  let exec ~buck_mode version expr =
    let query_string_of_version = match version with V1 -> "query" | V2 -> "cquery" in
    let query = F.asprintf "%a" pp expr in
    let buck_config = config buck_mode version in
    let buck_output_options =
      match ((buck_mode : BuckMode.t), version) with
      | Java, V1 ->
          ["--output-format"; "json"; "--output-attribute"; "srcs"]
      | _ ->
          []
    in
    let bounded_args =
      store_args_in_file ~identifier:"buck_query_args" (buck_config @ buck_output_options @ [query])
    in
    let extra_buck_args =
      match version with V1 -> Config.buck_build_args_no_inline | V2 -> Config.buck2_build_args
    in
    let cmd =
      binary_of_version version :: query_string_of_version :: (extra_buck_args @ bounded_args)
    in
    wrap_buck_call ~label:"query" version cmd |> parse_query_output ~buck_mode version
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


let resolve_pattern_targets (buck_mode : BuckMode.t) version targets =
  let target_set = List.rev_map targets ~f:Query.target |> Query.set in
  let deps_query =
    match (buck_mode, version) with
    | Clang, V2 ->
        if Config.buck2_query_deps then Query.deps Config.buck_dependency_depth else Fn.id
    | Clang, V1 | ClangCompilationDB NoDependencies, _ | Erlang, _ ->
        Fn.id
    | Java, _ ->
        Query.deps Config.buck_dependency_depth
    | ClangCompilationDB DepsAllDepths, _ ->
        Query.deps None
    | ClangCompilationDB (DepsUpToDepth depth), _ ->
        Query.deps (Some depth)
  in
  let accepted_patterns_filter = Query.kind ~pattern:(get_accepted_buck_kinds_pattern buck_mode) in
  let buck2_java_infer_enabled_filter =
    match (buck_mode, version) with
    | Java, V2 ->
        Query.label_filter ~label:"infer_enabled"
    | _, _ ->
        Fn.id
  in
  let results =
    target_set |> deps_query |> accepted_patterns_filter |> buck2_java_infer_enabled_filter
    |> Query.exec ~buck_mode version
  in
  match (buck_mode, version) with
  | Java, V2 ->
      List.rev_map results ~f:(fun s -> s ^ "_infer")
  | _, _ ->
      results


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
  fun (buck_mode : BuckMode.t) (version : version) original_buck_args ->
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
      match (buck_mode, version, parsed_args) with
      | Clang, V1, {pattern_targets= []; alias_targets= []; normal_targets} ->
          normal_targets
      | Clang, V2, {pattern_targets; alias_targets; normal_targets} ->
          pattern_targets |> List.rev_append alias_targets |> List.rev_append normal_targets
      | _, _, {pattern_targets; alias_targets; normal_targets} ->
          pattern_targets |> List.rev_append alias_targets |> List.rev_append normal_targets
          |> resolve_pattern_targets buck_mode version
    in
    let targets =
      Option.value_map ~default:targets
        ~f:(fun re -> List.filter ~f:(fun tgt -> not (Str.string_match re tgt 0)) targets)
        buck_targets_block_list_regexp
    in
    ScubaLogging.log_count ~label:"buck_targets" ~value:(List.length targets) ;
    (command, parsed_args.rev_not_targets', targets)


let filter_compatible subcommand args =
  match subcommand with
  | `Targets ->
      let block_list = "--keep-going" in
      List.filter args ~f:(fun arg -> not (String.equal block_list arg))
  | _ ->
      args
