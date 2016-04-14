(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module L = Logging
module F = Format
open Dataflow

(** Simple check for dead code. *)

let verbose = false

module State = struct
  type t =
    {
      visited : Cfg.NodeSet.t;
    }

  let initial =
    {
      visited = Cfg.NodeSet.empty;
    }

  let equal t1 t2 =
    Cfg.NodeSet.equal t1.visited t2.visited

  let join t1 t2 =
    {
      visited = Cfg.NodeSet.union t1.visited t2.visited
    }

  let add_visited node t =
    {
      visited = Cfg.NodeSet.add node t.visited;
    }

  let get_visited t =
    t.visited

  let pp fmt t =
    F.fprintf fmt "visited: %a"
      (pp_seq Cfg.Node.pp) (Cfg.NodeSet.elements t.visited)

  let num_visited t =
    Cfg.NodeSet.cardinal t.visited
end

let do_node _ node (s : State.t) : (State.t list) * (State.t list) =
  let s' = State.add_visited node s in
  if verbose then L.stderr "  N:%a (#visited: %a)@."
      Cfg.Node.pp node
      State.pp s';
  [s'], [s']


(** Report an error. *)
let report_error description pn pd loc =
  if verbose then L.stderr "ERROR: %s@." description;
  Checkers.ST.report_error pn pd "CHECKERS_DEAD_CODE" loc description


(** Check the final state at the end of the analysis. *)
let check_final_state proc_name proc_desc final_s =
  let proc_nodes = Cfg.Procdesc.get_nodes proc_desc in
  let tot_nodes = IList.length proc_nodes in
  let tot_visited = State.num_visited final_s in
  if verbose then L.stderr "TOT nodes: %d (visited: %n)@." tot_nodes tot_visited;
  if tot_nodes <> tot_visited then
    begin
      let not_visited =
        IList.filter (fun n -> not (Cfg.NodeSet.mem n (State.get_visited final_s))) proc_nodes in
      let do_node n =
        let loc = Cfg.Node.get_loc n in
        let description = Format.sprintf "Node not visited: %d" (Cfg.Node.get_id n :> int) in
        let report = match Cfg.Node.get_kind n with
          | Cfg.Node.Join_node -> false
          | k when k = Cfg.Node.exn_sink_kind -> false
          | _ -> true in
        if report
        then report_error description proc_name proc_desc loc in
      IList.iter do_node not_visited
    end

(** Simple check for dead code. *)
let callback_check_dead_code { Callbacks.proc_desc; proc_name; tenv } =

  let module DFDead = MakeDF(struct
      type t = State.t
      let equal = State.equal
      let join = State.join
      let do_node = do_node
      let proc_throws _ = DontKnow
    end) in

  let do_check () =
    begin
      if verbose then L.stderr "@.--@.PROC: %a@." Procname.pp proc_name;
      let transitions = DFDead.run tenv proc_desc State.initial in
      let exit_node = Cfg.Procdesc.get_exit_node proc_desc in
      match transitions exit_node with
      | DFDead.Transition (pre_final_s, _, _) ->
          let final_s = State.add_visited exit_node pre_final_s in
          check_final_state proc_name proc_desc final_s
      | DFDead.Dead_state -> ()
    end in

  do_check ()
