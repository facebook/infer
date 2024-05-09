(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging
module F = Format
module Visited = HashSet.Make (String)

let traverse ~root visited acc target_path =
  if String.is_empty target_path then acc
  else
    let target_path =
      if Filename.is_absolute target_path then target_path else root ^/ target_path
    in
    let real_path = Utils.realpath target_path in
    if Visited.mem visited real_path then acc
    else (
      Visited.add real_path visited ;
      match Sys.is_directory target_path with
      | `Yes when ISys.file_exists (ResultsDirEntryName.get_path ~results_dir:target_path CaptureDB)
        ->
          (* we found a capture DB so add this as a target line *)
          Printf.sprintf "dummy\t-\t%s" target_path :: acc
      | _ ->
          acc )


let abs_buck2_root = Utils.realpath Config.buck2_root

let get_buck2_root_relative_changed_files () =
  match SourceFile.read_config_changed_files () with
  | None ->
      []
  | Some files ->
      let existing_absolute_paths =
        SourceFile.Set.fold
          (fun file acc ->
            let abs_file_path = SourceFile.to_abs_path file in
            match Sys.file_exists abs_file_path with `Yes -> abs_file_path :: acc | _ -> acc )
          files []
      in
      List.fold existing_absolute_paths ~init:[] ~f:(fun acc abs_file_path ->
          Utils.filename_to_relative ~root:abs_buck2_root abs_file_path
          |> Option.fold ~init:acc ~f:(fun acc path_rel_to_buck2_root ->
                 path_rel_to_buck2_root :: acc ) )


let run_capture buck2_build_cmd =
  L.debug Capture Quiet "Processed buck2 bxl command '%a'@\n" (Pp.seq F.pp_print_string)
    buck2_build_cmd ;
  let infer_deps_lines =
    Buck.wrap_buck_call ~extend_env:[] V2 ~label:"build" ("buck2" :: buck2_build_cmd)
    |> List.fold ~init:[] ~f:(traverse ~root:Config.buck2_root (Visited.create 11))
    |> List.dedup_and_sort ~compare:String.compare
  in
  let infer_deps = ResultsDir.get_path CaptureDependencies in
  Utils.with_file_out infer_deps ~f:(fun out_channel ->
      Out_channel.output_lines out_channel infer_deps_lines )


let capture build_cmd =
  let bxl_target =
    match Config.buck2_bxl_target with
    | None ->
        L.die UserError "A BXL script must be provided when capturing with buck2/clang.@\n"
    | Some target ->
        target
  in
  let _prog, buck2_args = (List.hd_exn build_cmd, List.tl_exn build_cmd) in
  let _command, rev_not_targets, targets = Buck.parse_command_and_targets Clang V2 buck2_args in
  let buck2_root_relative_paths = get_buck2_root_relative_changed_files () in
  if List.is_empty targets && List.is_empty buck2_root_relative_paths then (
    L.user_warning "No targets nor files found to capture.@\n" ;
    (* write an empty infer-deps.txt to avoid crashes later in the flow *)
    let infer_deps = ResultsDir.get_path CaptureDependencies in
    Utils.with_file_out infer_deps ~f:(fun out_channel -> Out_channel.output_lines out_channel []) )
  else
    let targets_with_arg =
      List.fold targets ~init:[] ~f:(fun acc target -> "--target" :: target :: acc)
    in
    let files_with_arg =
      List.fold buck2_root_relative_paths ~init:[] ~f:(fun acc path -> "--file" :: path :: acc)
    in
    let block_files =
      List.fold Config.buck2_bxl_capture_file_block_list ~init:[] ~f:(fun acc b ->
          "--block-file" :: b :: acc )
    in
    let args_to_store =
      ["--"]
      @ Option.value_map Config.buck_dependency_depth ~default:[] ~f:(fun depth ->
            [Printf.sprintf "--depth=%i" depth] )
      @ Option.value_map Config.buck2_infertoolchain_target ~default:[] ~f:(fun target ->
            ["--infer-toolchain"; target] )
      @ Option.value_map Config.buck2_inferconfig_target ~default:[] ~f:(fun target ->
            ["--inferconfig"; target] )
      @ block_files @ files_with_arg @ targets_with_arg
    in
    let buck2_build_cmd =
      ["bxl"; bxl_target; "--console=simple"]
      @ List.rev rev_not_targets @ Config.buck2_build_args_no_inline
      @ Buck.store_args_in_file ~identifier:"buck2_bxl" args_to_store
    in
    run_capture buck2_build_cmd
