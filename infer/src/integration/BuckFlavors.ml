(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module CLOpt = CommandLineOption
module L = Logging
module F = Format

type flavored_arguments = {command: string; rev_not_targets: string list; targets: string list}

let add_flavors_to_buck_arguments buck_mode ~extra_flavors original_buck_args =
  let command, rev_not_targets, targets =
    Buck.parse_command_and_targets buck_mode original_buck_args
  in
  let targets =
    List.rev_map targets ~f:(fun t ->
        Buck.Target.(
          t |> of_string |> add_flavor ~extra_flavors buck_mode Config.command |> to_string) )
  in
  {command; rev_not_targets; targets}


let capture_buck_args () =
  ("--show-output" :: (if Config.keep_going then ["--keep-going"] else []))
  @ (match Config.load_average with Some l -> ["-L"; Float.to_string l] | None -> [])
  @ Buck.config ClangFlavors @ Config.buck_build_args


let run_buck_build prog buck_build_args =
  L.debug Capture Verbose "%s %s@." prog (List.to_string ~f:Fn.id buck_build_args) ;
  let infer_args =
    Option.fold (Sys.getenv CommandLineOption.args_env_var) ~init:"--fcp-syntax-only"
      ~f:(fun acc arg -> Printf.sprintf "%s%c%s" acc CommandLineOption.env_var_sep arg)
  in
  let extend_env = [(CommandLineOption.args_env_var, infer_args)] in
  let lines = Buck.wrap_buck_call ~extend_env ~label:"build" (prog :: buck_build_args) in
  (* Process a line of buck stdout output, in this case the result of '--show-output'
     These paths (may) contain a 'infer-deps.txt' file, which we will later merge
  *)
  let process_buck_line acc line =
    L.debug Capture Verbose "BUCK OUT: %s@." line ;
    match String.lsplit2 ~on:' ' line with
    | Some (_, infer_deps_path) ->
        let full_path = Config.project_root ^/ infer_deps_path in
        let dirname = Filename.dirname full_path in
        let get_path results_dir = ResultsDirEntryName.get_path ~results_dir CaptureDependencies in
        (* Buck can either give the full path to infer-deps.txt ... *)
        if ISys.file_exists (get_path dirname) then get_path dirname :: acc
          (* ... or a folder which contains infer-deps.txt *)
        else if ISys.file_exists (get_path full_path) then get_path full_path :: acc
        else acc
    | _ ->
        L.internal_error "Couldn't parse buck target output: %s@\n" line ;
        acc
  in
  List.fold lines ~init:[] ~f:process_buck_line


let merge_deps_files depsfiles =
  let buck_out = Config.project_root ^/ Config.buck_out_gen in
  let depslines, depsfiles =
    match depsfiles with
    | [] when Config.keep_going || Config.buck_merge_all_deps ->
        let infouts =
          Utils.fold_folders ~init:[] ~path:buck_out ~f:(fun acc dir ->
              if
                String.is_substring dir ~substring:"infer-out"
                && ISys.file_exists (ResultsDirEntryName.get_path ~results_dir:dir CaptureDB)
              then Printf.sprintf "\t\t%s" dir :: acc
              else acc )
        in
        (infouts, [])
    | [] when Config.buck_merge_all_deps ->
        let files =
          Utils.find_files ~path:buck_out ~extension:ResultsDirEntryName.buck_infer_deps_file_name
        in
        ([], files)
    | _ ->
        ([], depsfiles)
  in
  depslines
  @ List.fold depsfiles ~init:[] ~f:(fun acc file ->
        List.rev_append acc (Utils.with_file_in file ~f:In_channel.input_lines) )
  |> List.dedup_and_sort ~compare:String.compare


let clang_flavor_capture ~prog ~buck_build_cmd =
  if Config.keep_going && not Config.continue_capture then
    Process.create_process_and_wait ~prog ~args:["clean"] ;
  let depsfiles = run_buck_build prog (buck_build_cmd @ capture_buck_args ()) in
  let deplines = merge_deps_files depsfiles in
  let infer_out_depsfile = ResultsDir.get_path CaptureDependencies in
  Utils.with_file_out infer_out_depsfile ~f:(fun out_chan ->
      Out_channel.output_lines out_chan deplines ) ;
  ()


let capture build_cmd =
  let prog, buck_args = (List.hd_exn build_cmd, List.tl_exn build_cmd) in
  (* let children infer processes know that they are inside Buck *)
  let infer_args_with_buck =
    String.concat
      ~sep:(String.of_char CLOpt.env_var_sep)
      (Option.to_list (Sys.getenv CLOpt.args_env_var) @ ["--buck"])
  in
  Unix.putenv ~key:CLOpt.args_env_var ~data:infer_args_with_buck ;
  let {command; rev_not_targets; targets} =
    add_flavors_to_buck_arguments ClangFlavors ~extra_flavors:[] buck_args
  in
  if not (List.is_empty targets) then (
    let all_args = List.rev_append rev_not_targets targets in
    let updated_buck_cmd =
      command
      :: ( Config.buck_build_args_no_inline
         @ Buck.store_args_in_file ~identifier:"clang_flavor_build" all_args )
    in
    L.debug Capture Quiet "Processed buck command '%a'@\n" (Pp.seq F.pp_print_string)
      updated_buck_cmd ;
    let prog, buck_build_cmd = (prog, updated_buck_cmd) in
    ResultsDir.RunState.set_merge_capture true ;
    clang_flavor_capture ~prog ~buck_build_cmd )
