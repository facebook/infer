(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Simplified html node printer for checkers *)

(** Mark the node visited and return the new session number *)
let new_session node =
  let pname = Procdesc.Node.get_proc_name node in
  let node_id = (Procdesc.Node.get_id node :> int) in
  match Summary.get pname with
  | None ->
      0
  | Some summary ->
      Summary.Stats.add_visited summary.stats node_id ;
      incr summary.Summary.sessions ;
      !(summary.Summary.sessions)


let start_session ~pp_name node =
  if Config.write_html then
    let session = new_session node in
    Printer.node_start_session ~pp_name node session


let finish_session node = if Config.write_html then Printer.node_finish_session node
