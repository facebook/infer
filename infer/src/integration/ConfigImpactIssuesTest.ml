(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let pp_custom_of_config_impact_report fmt report =
  let pp_custom_of_config_impact_issue fmt (config_impact_item : Jsonconfigimpact_t.item) =
    let open Jsonbug_t in
    F.fprintf fmt "%s%s, %s, {%a}@\n"
      (match config_impact_item.mode with `Normal -> "" | `Strict -> "[STRICT] ")
      config_impact_item.procedure_name config_impact_item.loc.file
      ConfigImpactAnalysis.UncheckedCallees.pp_without_location
      (ConfigImpactAnalysis.UncheckedCallees.decode config_impact_item.unchecked_callees)
  in
  List.iter ~f:(pp_custom_of_config_impact_issue fmt) report


let config_impact_tests_jsonbug_compare (x : Jsonconfigimpact_t.item) (y : Jsonconfigimpact_t.item)
    =
  let open Jsonbug_t in
  [%compare: string * string * string]
    (x.loc.file, x.procedure_id, x.hash)
    (y.loc.file, y.procedure_id, y.hash)


let write_from_json ~json_path ~out_path =
  Utils.with_file_out out_path ~f:(fun outf ->
      let config_impact_report =
        Atdgen_runtime.Util.Json.from_file Jsonconfigimpact_j.read_report json_path
      in
      let sorted_config_impact_report =
        List.sort ~compare:config_impact_tests_jsonbug_compare config_impact_report
      in
      pp_custom_of_config_impact_report (F.formatter_of_out_channel outf)
        sorted_config_impact_report )
