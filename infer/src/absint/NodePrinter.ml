(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module L = Logging
module F = Format

(** Simplified html node printer for checkers *)

(** Mark the node visited and return the new session number *)
let new_session node =
  let pname = Procdesc.Node.get_proc_name node in
  let node_id = (Procdesc.Node.get_id node :> int) in
  match Specs.get_summary pname with
  | None
   -> 0
  | Some summary
   -> (summary.stats).nodes_visited_fp <- IntSet.add node_id summary.stats.nodes_visited_fp ;
      incr summary.Specs.sessions ;
      !(summary.Specs.sessions)

let start_session node =
  if Config.write_html then
    let session = new_session node in
    Printer.node_start_session node session

let finish_session node = if Config.write_html then Printer.node_finish_session node
