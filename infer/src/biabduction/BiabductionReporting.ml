(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let log_issue_deprecated_using_state severity proc_name ?node ?loc ?ltr exn =
  if !BiabductionConfig.footprint then
    match Summary.OnDisk.get proc_name with
    | Some summary ->
        let node =
          let node = match node with None -> State.get_node_exn () | Some node -> node in
          Errlog.BackendNode {node}
        in
        let session = State.get_session () in
        let loc = match loc with None -> State.get_loc_exn () | Some loc -> loc in
        let ltr = match ltr with None -> State.get_loc_trace () | Some ltr -> ltr in
        Reporting.log_issue_from_summary severity (Summary.get_attributes summary)
          summary.Summary.err_log ~node ~session ~loc ~ltr exn
    | None ->
        L.(die InternalError)
          "Trying to report error on procedure %a, but cannot because no summary exists for this \
           procedure. Did you mean to log the error on the caller of %a instead?"
          Procname.pp proc_name Procname.pp proc_name


let log_error_using_state proc_desc err_log exn =
  if !BiabductionConfig.footprint then
    let node' =
      match State.get_node () with Some n -> n | None -> Procdesc.get_start_node proc_desc
    in
    let node = Errlog.BackendNode {node= node'} in
    let session = State.get_session () in
    let loc = match State.get_loc () with Some l -> l | None -> Procdesc.Node.get_loc node' in
    let ltr = State.get_loc_trace () in
    let attrs = Procdesc.get_attributes proc_desc in
    Reporting.log_issue_from_summary Exceptions.Error attrs err_log ~node ~session ~loc ~ltr exn
