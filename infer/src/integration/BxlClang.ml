(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging
module F = Format

let rec traverse ~root acc target_path =
  let target_path = if Filename.is_absolute target_path then target_path else root ^/ target_path in
  match Sys.is_directory target_path with
  | `Yes when ISys.file_exists (ResultsDirEntryName.get_path ~results_dir:target_path CaptureDB) ->
      (* we found a capture DB so add this as a target line *)
      Printf.sprintf "dummy\t-\t%s" target_path :: acc
  | `Yes ->
      (* recurse into non-infer-out directory *)
      Sys.readdir target_path
      |> Array.fold ~init:acc ~f:(fun acc entry ->
             traverse ~root acc (Filename.concat target_path entry) )
  | _ ->
      acc


let capture build_cmd =
  let bxl_target =
    match Config.buck2_bxl_target with
    | None ->
        L.die UserError "A BXL script must be provided when capturing with buck2/clang.@\n"
    | Some target ->
        target
  in
  let prog, buck2_args = (List.hd_exn build_cmd, List.tl_exn build_cmd) in
  let _command, rev_not_targets, targets = Buck.parse_command_and_targets Clang V2 buck2_args in
  if List.is_empty targets then L.user_warning "No targets found to capture.@\n"
  else
    let targets_with_arg =
      List.fold targets ~init:[] ~f:(fun acc target -> "--target" :: target :: acc)
    in
    let buck2_build_cmd =
      ["bxl"; bxl_target] @ List.rev rev_not_targets @ Config.buck2_build_args_no_inline @ ["--"]
      @ Option.value_map Config.buck_dependency_depth ~default:[] ~f:(fun depth ->
            ["--depth"; Int.to_string depth] )
      @ Buck.store_args_in_file ~identifier:"clang_buck2_bxl" targets_with_arg
    in
    L.debug Capture Quiet "Processed buck2 bxl command '%a'@\n" (Pp.seq F.pp_print_string)
      buck2_build_cmd ;
    ResultsDir.RunState.set_merge_capture true ;
    let infer_deps_lines =
      Buck.wrap_buck_call ~extend_env:[] V2 ~label:"build" (prog :: buck2_build_cmd)
      |> List.fold ~init:[] ~f:(traverse ~root:Config.buck2_root)
      |> List.dedup_and_sort ~compare:String.compare
    in
    let infer_deps = ResultsDir.get_path CaptureDependencies in
    Utils.with_file_out infer_deps ~f:(fun out_channel ->
        Out_channel.output_lines out_channel infer_deps_lines )
