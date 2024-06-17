(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

(** Simplified html node printer for checkers *)

(** Mark the node visited and return the new session number *)
let new_session node =
  let pname = Procdesc.Node.get_proc_name node in
  let node_id = (Procdesc.Node.get_id node :> int) in
  match Summary.OnDisk.get ~lazy_payloads:true AnalysisRequest.all pname with
  | None ->
      0
  | Some summary ->
      Summary.Stats.add_visited summary.stats node_id ;
      summary.Summary.sessions <- summary.Summary.sessions + 1 ;
      summary.Summary.sessions


let kind_to_string = function
  | `ComputePre ->
      "compute pre"
  | `ExecNode ->
      "exec"
  | `ExecNodeNarrowing ->
      "exec NARROWING"
  | `WTO ->
      "WEAK TOPOLOGICAL ORDER"


let with_kind pp_name kind f = Format.fprintf f "[%s] %t" (kind_to_string kind) pp_name

(* turned off in preanalysis *)
let print_html = ref true

let with_session ?kind ~pp_name node ~f =
  AnalysisState.set_node node ;
  if Config.write_html && !print_html then (
    L.reset_delayed_prints () ;
    let session = new_session node in
    AnalysisState.set_session session ;
    let pp_name = Option.fold kind ~init:pp_name ~f:with_kind in
    Printer.node_start_session ~pp_name node session ;
    Exception.try_finally ~f ~finally:(fun () -> Printer.node_finish_session node) )
  else f ()
