(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let pp_custom_of_cost_report fmt report cost_fields =
  let pp_custom_of_cost_issue fmt (cost_item : Jsoncost_t.item) =
    let open Jsonbug_t in
    let comma_separator index = if index > 0 then ", " else "" in
    let pp_cost_field index cost_field =
      match (cost_field : CostIssuesTestField.t) with
      | Procedure ->
          F.fprintf fmt "%s%s" (comma_separator index) cost_item.procedure_id
      | File ->
          F.fprintf fmt "%s%s" (comma_separator index) cost_item.loc.file
      | Cost ->
          F.fprintf fmt "%s%s" (comma_separator index) cost_item.exec_cost.hum.hum_polynomial
      | IsOnUIThread ->
          F.fprintf fmt "%s OnUIThread:%b" (comma_separator index) cost_item.is_on_ui_thread
      | Trace ->
          let trace = cost_item.exec_cost.trace in
          IssuesTest.pp_trace fmt trace (comma_separator index)
    in
    List.iteri ~f:pp_cost_field cost_fields ;
    F.fprintf fmt "@."
  in
  List.iter ~f:(pp_custom_of_cost_issue fmt) report


let cost_tests_jsonbug_compare (cost1 : Jsoncost_t.item) (cost2 : Jsoncost_t.item) =
  let open Jsonbug_t in
  [%compare: string * string * string * Caml.Digest.t * bool]
    ( cost1.loc.file
    , cost1.procedure_id
    , cost1.exec_cost.hum.hum_polynomial
    , cost1.hash
    , cost1.is_on_ui_thread )
    ( cost2.loc.file
    , cost2.procedure_id
    , cost2.exec_cost.hum.hum_polynomial
    , cost2.hash
    , cost2.is_on_ui_thread )


let write_from_json ~json_path ~out_path cost_issues_tests_fields =
  Utils.with_file_out out_path ~f:(fun outf ->
      let cost_report = Atdgen_runtime.Util.Json.from_file Jsoncost_j.read_report json_path in
      let sorted_cost_report = List.sort ~compare:cost_tests_jsonbug_compare cost_report in
      pp_custom_of_cost_report (F.formatter_of_out_channel outf) sorted_cost_report
        cost_issues_tests_fields )
