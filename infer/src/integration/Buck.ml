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


(** Wrap a call to buck while (i) logging standard error to our standard error in real time; (ii)
    redirecting standard out to a file, the contents of which are returned; (iii) protect the child
    process from [SIGQUIT].

    We want to ihnibit [SIGQUIT] because the standard action of the JVM is to print a thread dump on
    stdout, polluting the output we want to collect (and normally does not lead to process death).
    To achieve this we need to do two things: (i) tell the JVM not to use signals, meaning it leaves
    the default handler for [SIGQUIT] in place; (ii) uninstall the default handler for [SIGQUIT]
    because now that the JVM doesn't touch it, it will lead to process death. *)
let wrap_buck_call ?(extend_env = []) ~label cmd =
  let stdout_file =
    let prefix = Printf.sprintf "buck_%s" label in
    Filename.temp_file ~in_dir:(ResultsDir.get_path Temporary) prefix ".stdout"
  in
  let escaped_cmd = List.map ~f:Escape.escape_shell cmd |> String.concat ~sep:" " in
  let sigquit_protected_cmd =
    (* Uninstall the default handler for [SIGQUIT]. *)
    Printf.sprintf "trap '' SIGQUIT ; exec %s >'%s'" escaped_cmd stdout_file
  in
  let env =
    let explicit_buck_java_heap_size =
      Option.map Config.buck_java_heap_size_gb ~f:(fun size -> Printf.sprintf "-Xmx%dG" size)
      |> Option.to_list
    in
    let existing_buck_extra_java_args = Sys.getenv buck_extra_java_args_env_var |> Option.to_list in
    let new_buck_extra_java_args =
      (* Instruct the JVM to avoid using signals. *)
      String.concat ~sep:" "
        (existing_buck_extra_java_args @ explicit_buck_java_heap_size @ ["-Xrs"])
    in
    L.environment_info "Buck: setting %s to '%s'@\n" buck_extra_java_args_env_var
      new_buck_extra_java_args ;
    `Extend ((buck_extra_java_args_env_var, new_buck_extra_java_args) :: extend_env)
  in
  let Unix.Process_info.{stdin; stdout; stderr; pid} =
    Unix.create_process_env ~prog:"sh" ~args:["-c"; sigquit_protected_cmd] ~env ()
  in
  let buck_stderr = Unix.in_channel_of_descr stderr in
  Utils.with_channel_in buck_stderr ~f:(L.progress "BUCK: %s@\n") ;
  Unix.close stdin ;
  Unix.close stdout ;
  In_channel.close buck_stderr ;
  match Unix.waitpid pid with
  | Ok () -> (
    match Utils.read_file stdout_file with
    | Ok lines ->
        lines
    | Error err ->
        L.die ExternalError "*** failed to read output of buck command %s: %s" sigquit_protected_cmd
          err )
  | Error _ as err ->
      L.die ExternalError "*** failed to execute buck command %s: %s" sigquit_protected_cmd
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


  let add_flavor (mode : BuckMode.t) (command : InferCommand.t) ~extra_flavors target =
    let target = List.fold_left ~f:add_flavor_internal ~init:target extra_flavors in
    match (mode, command) with
    | ClangCompilationDB _, _ ->
        add_flavor_internal target "compilation-database"
    | ClangFlavors, Compile ->
        target
    | JavaFlavor, _ ->
        add_flavor_internal target "infer-java-capture"
    | ClangFlavors, _ ->
        add_flavor_internal target "infer-capture-all"
end

let config =
  let clang_path =
    List.fold ["clang"; "install"; "bin"; "clang"] ~init:Config.fcp_dir ~f:Filename.concat
  in
  let get_java_flavor_config () =
    if Config.buck_java_flavor_suppress_config then []
    else
      [ "infer_java.version=" ^ Version.versionString
      ; Printf.sprintf "infer_java.binary=%s/infer" Config.bin_dir ]
  in
  let get_flavors_config () =
    [ "client.id=infer.clang"
    ; Printf.sprintf "*//infer.infer_bin=%s" Config.bin_dir
    ; Printf.sprintf "*//infer.clang_compiler=%s" clang_path
    ; Printf.sprintf "*//infer.clang_plugin=%s" Config.clang_plugin_path
    ; "*//cxx.pch_enabled=false"
    ; (* Infer doesn't support C++ modules yet (T35656509) *)
      "*//cxx.modules_default=false"
    ; "*//cxx.modules=false" ]
    @ ( match Config.xcode_developer_dir with
      | Some d ->
          [Printf.sprintf "apple.xcode_developer_dir=%s" d]
      | None ->
          [] )
    @
    if List.is_empty Config.buck_blacklist then []
    else
      [ Printf.sprintf "*//infer.blacklist_regex=(%s)"
          (String.concat ~sep:")|(" Config.buck_blacklist) ]
  in
  fun buck_mode ->
    let args =
      match (buck_mode : BuckMode.t) with
      | JavaFlavor ->
          get_java_flavor_config ()
      | ClangFlavors ->
          get_flavors_config ()
      | ClangCompilationDB _ ->
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
      `NormalTarget (F.sprintf "%s:%s" s name)
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
  let parse_query_output ?buck_mode output =
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
          String.is_suffix ~suffix:".java" source_path
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
    match (buck_mode : BuckMode.t option) with
    | Some JavaFlavor ->
        String.concat output |> Yojson.Basic.from_string |> get_target_srcs_assoc_list
        |> List.fold ~init:[] ~f:process_target
    | _ ->
        output


  let exec ?buck_mode expr =
    let query = F.asprintf "%a" pp expr in
    let buck_config = Option.value_map buck_mode ~default:[] ~f:config in
    let buck_output_options =
      match (buck_mode : BuckMode.t option) with
      | Some JavaFlavor ->
          ["--output-format"; "json"; "--output-attribute"; "srcs"]
      | _ ->
          []
    in
    let bounded_args =
      store_args_in_file ~identifier:"buck_query_args" (buck_config @ buck_output_options @ [query])
    in
    let cmd = "buck" :: "query" :: (Config.buck_build_args_no_inline @ bounded_args) in
    wrap_buck_call ~label:"query" cmd |> parse_query_output ?buck_mode
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
  | ClangFlavors ->
      "^(apple|cxx)_(binary|library)$"
  | JavaFlavor ->
      "^(java|android)_library$"


let resolve_pattern_targets (buck_mode : BuckMode.t) targets =
  targets |> List.rev_map ~f:Query.target |> Query.set
  |> ( match buck_mode with
     | ClangFlavors | ClangCompilationDB NoDependencies ->
         Fn.id
     | ClangCompilationDB DepsAllDepths | JavaFlavor ->
         Query.deps None
     | ClangCompilationDB (DepsUpToDepth depth) ->
         Query.deps (Some depth) )
  |> Query.kind ~pattern:(get_accepted_buck_kinds_pattern buck_mode)
  |> Query.exec ~buck_mode


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


(** Given a list of arguments return the extended list of arguments where the args in a file have
    been extracted *)
let inline_argument_files buck_args =
  let expand_buck_arg buck_arg =
    if String.is_prefix ~prefix:"@" buck_arg then
      let file_name = String.chop_prefix_exn ~prefix:"@" buck_arg in
      if PolyVariantEqual.(Sys.file_exists file_name <> `Yes) then [buck_arg]
        (* Arguments that start with @ could mean something different than an arguments file in buck. *)
      else
        let expanded_args =
          try Utils.with_file_in file_name ~f:In_channel.input_lines
          with exn ->
            Logging.die UserError "Could not read from file '%s': %a@\n" file_name Exn.pp exn
        in
        expanded_args
    else [buck_arg]
  in
  List.concat_map ~f:expand_buck_arg buck_args


let parse_command_and_targets (buck_mode : BuckMode.t) original_buck_args =
  let expanded_buck_args = inline_argument_files original_buck_args in
  let command, args = split_buck_command expanded_buck_args in
  let buck_targets_blacklist_regexp =
    if List.is_empty Config.buck_targets_blacklist then None
    else
      Some
        (Str.regexp ("\\(" ^ String.concat ~sep:"\\)\\|\\(" Config.buck_targets_blacklist ^ "\\)"))
  in
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
    | ClangFlavors, {pattern_targets= []; alias_targets= []; normal_targets} ->
        normal_targets
    | ( (ClangFlavors | ClangCompilationDB _ | JavaFlavor)
      , {pattern_targets; alias_targets; normal_targets} ) ->
        pattern_targets |> List.rev_append alias_targets |> List.rev_append normal_targets
        |> resolve_pattern_targets buck_mode
  in
  let targets =
    Option.value_map ~default:targets
      ~f:(fun re -> List.filter ~f:(fun tgt -> not (Str.string_match re tgt 0)) targets)
      buck_targets_blacklist_regexp
  in
  ScubaLogging.log_count ~label:"buck_targets" ~value:(List.length targets) ;
  (command, parsed_args.rev_not_targets', targets)


let filter_compatible subcommand args =
  match subcommand with
  | `Targets ->
      let blacklist = "--keep-going" in
      List.filter args ~f:(fun arg -> not (String.equal blacklist arg))
  | _ ->
      args
