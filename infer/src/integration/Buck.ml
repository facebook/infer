(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

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


  let add_flavor ~extra_flavors target =
    let target = List.fold_left ~f:add_flavor_internal ~init:target extra_flavors in
    match (Config.buck_compilation_database, Config.command) with
    | Some _, _ ->
        add_flavor_internal target "compilation-database"
    | None, Compile ->
        target
    | None, _ ->
        add_flavor_internal target "infer-capture-all"
end

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
    let no_quote_needed_regexp = Str.regexp "^[a-zA-Z0-9/:_*][a-zA-Z0-9/:.-_*]*$" in
    fun s ->
      if Str.string_match no_quote_needed_regexp s 0 then s
      else s |> Escape.escape_double_quotes |> F.sprintf "\"%s\""


  let target string = Target (quote_if_needed string)

  let kind ~pattern expr = Kind {pattern= quote_if_needed pattern; expr}

  let deps ?depth expr = Deps {depth; expr}

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


  let exec expr =
    let query = F.asprintf "%a" pp expr in
    let cmd = ["buck"; "query"] @ List.rev_append Config.buck_build_args_no_inline [query] in
    let tmp_prefix = "buck_query_" in
    let debug = L.(debug Capture Medium) in
    Utils.with_process_lines ~debug ~cmd ~tmp_prefix ~f:Fn.id
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


let get_accepted_buck_kinds_pattern () =
  if Option.is_some Config.buck_compilation_database then "^(apple|cxx)_(binary|library|test)$"
  else "^(apple|cxx)_(binary|library)$"


let max_command_line_length = 50

let die_if_empty f = function [] -> f L.(die UserError) | l -> l

let resolve_pattern_targets ~filter_kind ~dep_depth targets =
  targets |> List.rev_map ~f:Query.target |> Query.set
  |> (match dep_depth with None -> Fn.id | Some depth -> Query.deps ?depth)
  |> (if filter_kind then Query.kind ~pattern:(get_accepted_buck_kinds_pattern ()) else Fn.id)
  |> Query.exec
  |> die_if_empty (fun die -> die "*** buck query returned no targets.")


let resolve_alias_targets aliases =
  let debug = L.(debug Capture Medium) in
  (* we could use buck query to resolve aliases but buck targets --resolve-alias is faster *)
  let cmd = "buck" :: "targets" :: "--resolve-alias" :: aliases in
  let tmp_prefix = "buck_targets_" in
  let on_result_lines =
    die_if_empty (fun die ->
        die "*** No alias found for: '%a'." (Pp.seq ~sep:"', '" F.pp_print_string) aliases )
  in
  Utils.with_process_lines ~debug ~cmd ~tmp_prefix ~f:on_result_lines


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


(** Given a list of arguments return the extended list of arguments where
the args in a file have been extracted *)
let inline_argument_files buck_args =
  let expand_buck_arg buck_arg =
    if String.is_prefix ~prefix:"@" buck_arg then
      let file_name = String.chop_prefix_exn ~prefix:"@" buck_arg in
      if Sys.file_exists file_name <> `Yes then [buck_arg]
        (* Arguments that start with @ could mean something different than an arguments file in buck. *)
      else
        let expanded_args =
          try Utils.with_file_in file_name ~f:In_channel.input_lines with exn ->
            Logging.die UserError "Could not read from file '%s': %a@." file_name Exn.pp exn
        in
        expanded_args
    else [buck_arg]
  in
  List.concat_map ~f:expand_buck_arg buck_args


type flavored_arguments = {command: string; rev_not_targets: string list; targets: string list}

let add_flavors_to_buck_arguments ~filter_kind ~dep_depth ~extra_flavors original_buck_args =
  let expanded_buck_args = inline_argument_files original_buck_args in
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
    match (filter_kind, dep_depth, parsed_args) with
    | (`No | `Auto), None, {pattern_targets= []; alias_targets= []; normal_targets} ->
        normal_targets
    | `No, None, {pattern_targets= []; alias_targets; normal_targets} ->
        alias_targets |> resolve_alias_targets |> List.rev_append normal_targets
    | (`Yes | `No | `Auto), _, {pattern_targets; alias_targets; normal_targets} ->
        let filter_kind = match filter_kind with `No -> false | `Yes | `Auto -> true in
        pattern_targets |> List.rev_append alias_targets |> List.rev_append normal_targets
        |> resolve_pattern_targets ~filter_kind ~dep_depth
  in
  match targets with
  | [] ->
      L.(die UserError)
        "ERROR: no targets found in Buck command `%a`." (Pp.seq F.pp_print_string)
        original_buck_args
  | _ ->
      let rev_not_targets = parsed_args.rev_not_targets' in
      let targets =
        List.rev_map targets ~f:(fun t ->
            Target.(t |> of_string |> add_flavor ~extra_flavors |> to_string) )
      in
      {command; rev_not_targets; targets}


let rec exceed_length ~max = function
  | _ when max < 0 ->
      true
  | [] ->
      false
  | h :: t ->
      exceed_length ~max:(max - String.length h) t


let store_args_in_file args =
  if exceed_length ~max:max_command_line_length args then (
    let file = Filename.temp_file "buck_targets" ".txt" in
    let write_args outc = Out_channel.output_string outc (String.concat ~sep:"\n" args) in
    let () = Utils.with_file_out file ~f:write_args in
    L.(debug Capture Quiet) "Buck targets options stored in file '%s'@\n" file ;
    [Printf.sprintf "@%s" file] )
  else args


let filter_compatible subcommand args =
  match subcommand with
  | `Targets ->
      let blacklist = "--keep-going" in
      List.filter args ~f:(fun arg -> not (String.equal blacklist arg))
  | _ ->
      args
