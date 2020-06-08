(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let log_issue_deprecated_using_state proc_desc err_log ?node ?loc ?ltr exn =
  if !BiabductionConfig.footprint then
    let node =
      let node = match node with None -> AnalysisState.get_node_exn () | Some node -> node in
      Errlog.BackendNode {node}
    in
    let session = AnalysisState.get_session () in
    let loc = match loc with None -> AnalysisState.get_loc_exn () | Some loc -> loc in
    let ltr = match ltr with None -> State.get_loc_trace () | Some ltr -> ltr in
    let issue_to_report = Exceptions.recognize_exception exn in
    Reporting.log_issue_from_summary proc_desc err_log ~node ~session ~loc ~ltr Biabduction
      issue_to_report


let log_issue_using_state proc_desc err_log exn =
  if !BiabductionConfig.footprint then
    let node' =
      match AnalysisState.get_node () with Some n -> n | None -> Procdesc.get_start_node proc_desc
    in
    let node = Errlog.BackendNode {node= node'} in
    let session = AnalysisState.get_session () in
    let loc =
      match AnalysisState.get_loc () with Some l -> l | None -> Procdesc.Node.get_loc node'
    in
    let ltr = State.get_loc_trace () in
    let issue_to_report = Exceptions.recognize_exception exn in
    Reporting.log_issue_from_summary proc_desc err_log ~node ~session ~loc ~ltr Biabduction
      issue_to_report
