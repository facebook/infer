(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for Eradicate-based user-defined checkers. *)

let report_error {IntraproceduralAnalysis.proc_desc; tenv; err_log} checker kind loc
    ?(field_name = None) ?(exception_kind = fun k d -> Exceptions.Checkers (k, d)) ~severity
    description =
  let suppressed = Reporting.is_suppressed tenv proc_desc kind ~field_name in
  if suppressed then Logging.debug Analysis Medium "Reporting is suppressed!@\n"
  else
    let localized_description = Localise.verbatim_desc description in
    let exn = exception_kind kind localized_description in
    let trace = [Errlog.make_trace_element 0 loc description []] in
    let node = AnalysisState.get_node_exn () in
    let session = AnalysisState.get_session () in
    Reporting.log_issue_from_summary severity proc_desc err_log
      ~node:(BackendNode {node})
      ~session ~loc ~ltr:trace checker exn
