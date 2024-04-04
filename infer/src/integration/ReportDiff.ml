(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let reportdiff ~report_current:current_report_fname ~report_previous:previous_report_fname
    ~costs_current:current_costs_fname ~costs_previous:previous_costs_fname
    ~config_impact_current:current_config_impact_fname
    ~config_impact_previous:previous_config_impact_fname =
  let load_aux ~f filename_opt =
    Option.value_map
      ~f:(fun filename -> Atdgen_runtime.Util.Json.from_file f filename)
      ~default:[] filename_opt
  in
  let load_report = load_aux ~f:Jsonbug_j.read_report in
  let load_costs = load_aux ~f:Jsoncost_j.read_report in
  let load_config_impact = load_aux ~f:Jsonconfigimpact_j.read_report in
  let current_report = load_report current_report_fname in
  let previous_report = load_report previous_report_fname in
  let current_costs = load_costs current_costs_fname in
  let previous_costs = load_costs previous_costs_fname in
  let current_config_impact = load_config_impact current_config_impact_fname in
  let previous_config_impact = load_config_impact previous_config_impact_fname in
  let diff =
    let unfiltered_diff =
      Differential.issues_of_reports ~current_report ~previous_report ~current_costs ~previous_costs
        ~current_config_impact ~previous_config_impact
    in
    (* FIXME(T54950303) replace use of filtering with deduplicate *)
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
            List.map ~f:(SourceFile.create ~check_abs_path:false) (In_channel.read_lines fname) )
          Config.differential_filter_files
      in
      DifferentialFilters.do_filter unfiltered_diff file_renamings
        ~skip_duplicated_types:Config.skip_duplicated_types ~interesting_paths
    else unfiltered_diff
  in
  let out_path = ResultsDir.get_path Differential in
  Unix.mkdir_p out_path ;
  Differential.to_files diff out_path
