(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

let capture build_cmd =
  let prog, buck_cmd = (List.hd_exn build_cmd, List.tl_exn build_cmd) in
  L.progress "Querying buck for java flavor capture targets...@." ;
  let time0 = Mtime_clock.counter () in
  let BuckFlavors.{command; rev_not_targets; targets} =
    BuckFlavors.add_flavors_to_buck_arguments Java ~extra_flavors:[] buck_cmd
  in
  L.progress "Found %d java flavor capture targets in %a.@." (List.length targets) Mtime.Span.pp
    (Mtime_clock.count time0) ;
  let all_args = List.rev_append rev_not_targets targets in
  let build_report_file =
    Filename.temp_file ~in_dir:(ResultsDir.get_path Temporary) "buck_build_report" ".json"
  in
  let updated_buck_cmd =
    (* make buck tell us where in buck-out are the capture directories for merging *)
    (prog :: command :: "--build-report" :: build_report_file :: Buck.config Java V1)
    @ Config.buck_build_args_no_inline
    @ Buck.store_args_in_file ~identifier:"java_flavor_build" all_args
  in
  L.(debug Capture Quiet)
    "Processed buck command '%a'@." (Pp.seq F.pp_print_string) updated_buck_cmd ;
  let infer_deps_lines =
    if List.is_empty targets then (
      L.external_warning "WARNING: found no buck targets to analyze.@." ;
      [] )
    else
      let time0 = Mtime_clock.counter () in
      Buck.wrap_buck_call ~label:"build" V1 updated_buck_cmd |> ignore ;
      let lines = BuckBuildReport.parse_infer_deps ~root:Config.project_root ~build_report_file in
      L.progress "Java flavor capture took %a.@." Mtime.Span.pp (Mtime_clock.count time0) ;
      lines
  in
  let infer_deps = ResultsDir.get_path CaptureDependencies in
  Utils.with_file_out infer_deps ~f:(fun out_channel ->
      Out_channel.output_lines out_channel infer_deps_lines )
