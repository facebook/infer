(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

let reportdiff ~current_report:current_report_fname ~previous_report:previous_report_fname
    ~current_costs:current_costs_fname ~previous_costs:previous_costs_fname =
  let load_aux ~f filename_opt =
    Option.value_map ~f:(fun filename -> f (In_channel.read_all filename)) ~default:[] filename_opt
  in
  let load_report = load_aux ~f:Jsonbug_j.report_of_string in
  let load_costs = load_aux ~f:Jsonbug_j.costs_report_of_string in
  let current_report = load_report current_report_fname in
  let previous_report = load_report previous_report_fname in
  let current_costs = load_costs current_costs_fname in
  let previous_costs = load_costs previous_costs_fname in
  let diff =
    let unfiltered_diff =
      Differential.of_reports ~current_report ~previous_report ~current_costs ~previous_costs
    in
    if Config.filtering then
      let file_renamings =
        match Config.file_renamings with
        | Some f ->
            DifferentialFilters.FileRenamings.from_json_file f
        | None ->
            DifferentialFilters.FileRenamings.empty
      in
      let interesting_paths =
        Option.map
          ~f:(fun fname ->
            List.map ~f:(SourceFile.create ~warn_on_error:false) (In_channel.read_lines fname) )
          Config.differential_filter_files
      in
      DifferentialFilters.do_filter unfiltered_diff file_renamings
        ~skip_duplicated_types:Config.skip_duplicated_types ~interesting_paths
    else unfiltered_diff
  in
  let out_path = Config.results_dir ^/ "differential" in
  Unix.mkdir_p out_path ;
  Differential.to_files diff out_path
