(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

type target = {name: string; flavors: string list}

let target_of_string target =
  match String.split target ~on:'#' with
  | [name; flavors_string]
   -> let flavors = String.split flavors_string ~on:',' in
      {name; flavors}
  | [name]
   -> {name; flavors= []}
  | _
   -> failwithf "cannot parse target %s" target

let string_of_target {name; flavors} =
  let pp_string fmt s = Format.fprintf fmt "%s" s in
  Format.asprintf "%s#%a" name (Pp.comma_seq pp_string) flavors

let is_target_string =
  let target_regexp = Str.regexp "[^/]*//[^/]+.*:.*" in
  fun s -> Str.string_match target_regexp s 0

let no_targets_found_error_and_exit buck_cmd =
  Process.print_error_and_exit
    "No targets found in Buck command %s.@\nOnly fully qualified Buck targets are supported. In particular, aliases are not allowed.@."
    (String.concat ~sep:" " buck_cmd)

let add_flavor_to_target target =
  let add flavor =
    if List.mem ~equal:String.equal target.flavors flavor then
      (* there's already an infer flavor associated to the target, do nothing *)
      target
    else {target with flavors= flavor :: target.flavors}
  in
  match (Config.buck_compilation_database, Config.analyzer) with
  | Some _, _
   -> add "compilation-database"
  | None, CompileOnly
   -> target
  | None, (BiAbduction | CaptureOnly | Checkers | Linters)
   -> add "infer-capture-all"
  | None, Crashcontext
   -> failwithf "Analyzer %s is Java-only; not supported with Buck flavors"
        (Config.string_of_analyzer Config.analyzer)

let add_flavors_to_buck_command build_cmd =
  let add_infer_if_target s (cmd, found_one_target) =
    if not (is_target_string s) then (s :: cmd, found_one_target)
    else (string_of_target (add_flavor_to_target (target_of_string s)) :: cmd, true)
  in
  let cmd', found_one_target =
    List.fold_right build_cmd ~f:add_infer_if_target ~init:([], false)
  in
  if not found_one_target then no_targets_found_error_and_exit build_cmd ;
  cmd'

let call_buck_query_for_dependencies targets =
  let build_deps_string targets =
    List.map targets ~f:(fun target -> Printf.sprintf "deps('%s')" target)
    |> String.concat ~sep:" union "
  in
  let buck_query =
    [ "buck"
    ; "query"
    ; ( "\"kind('(apple_binary|apple_library|apple_test|cxx_binary|cxx_library|cxx_test)', "
      ^ build_deps_string targets ^ ")\"" ) ]
  in
  let buck_query_cmd = String.concat buck_query ~sep:" " in
  let output, exit_or_signal = Utils.with_process_in buck_query_cmd In_channel.input_lines in
  match exit_or_signal with
  | Error _ as status
   -> Logging.(die ExternalError)
        "*** command failed:@\n*** %s@\n*** %s@." buck_query_cmd
        (Unix.Exit_or_signal.to_string_hum status)
  | Ok ()
   -> output

let get_dependency_targets args =
  let targets, no_targets = List.partition_tf ~f:is_target_string args in
  let targets = call_buck_query_for_dependencies targets in
  (targets, no_targets)
