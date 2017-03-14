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
open Dataflow

(** Simple check for dead code. *)

let verbose = false

module State = struct
  type t =
    {
      visited : Procdesc.NodeSet.t;
    }

  let initial =
    {
      visited = Procdesc.NodeSet.empty;
    }

  let equal t1 t2 =
    Procdesc.NodeSet.equal t1.visited t2.visited

  let join t1 t2 =
    {
      visited = Procdesc.NodeSet.union t1.visited t2.visited
    }

  let add_visited node t =
    {
      visited = Procdesc.NodeSet.add node t.visited;
    }

  let get_visited t =
    t.visited

  let pp fmt t =
    F.fprintf fmt "visited: %a"
      (Pp.seq Procdesc.Node.pp) (Procdesc.NodeSet.elements t.visited)

  let num_visited t =
    Procdesc.NodeSet.cardinal t.visited
end

let do_node _ node (s : State.t) : (State.t list) * (State.t list) =
  let s' = State.add_visited node s in
  if verbose then L.stderr "  N:%a (#visited: %a)@."
      Procdesc.Node.pp node
      State.pp s';
  [s'], [s']


(** Report an error. *)
let report_error tenv description pn pd loc =
  if verbose then L.stderr "ERROR: %s@." description;
  Checkers.ST.report_error tenv pn pd Localise.checkers_dead_code loc description


(** Check the final state at the end of the analysis. *)
let check_final_state tenv proc_name proc_desc final_s =
  let proc_nodes = Procdesc.get_nodes proc_desc in
  let tot_nodes = List.length proc_nodes in
  let tot_visited = State.num_visited final_s in
  if verbose then L.stderr "TOT nodes: %d (visited: %n)@." tot_nodes tot_visited;
  if tot_nodes <> tot_visited then
    begin
      let not_visited =
        List.filter
          ~f:(fun n -> not (Procdesc.NodeSet.mem n (State.get_visited final_s)))
          proc_nodes in
      let do_node n =
        let loc = Procdesc.Node.get_loc n in
        let description = Format.sprintf "Node not visited: %d" (Procdesc.Node.get_id n :> int) in
        let report = match Procdesc.Node.get_kind n with
          | Procdesc.Node.Join_node -> false
          | k when Procdesc.Node.equal_nodekind k Procdesc.Node.exn_sink_kind -> false
          | _ -> true in
        if report
        then report_error tenv description proc_name proc_desc loc in
      List.iter ~f:do_node not_visited
    end

(** Simple check for dead code. *)
let callback_check_dead_code { Callbacks.proc_desc; tenv } =
  let proc_name = Procdesc.get_proc_name proc_desc in

  let module DFDead = MakeDF(struct
      type t = State.t
      let equal = State.equal
      let join = State.join
      let do_node = do_node
      let proc_throws _ = DontKnow
    end) in

  let do_check () =
    begin
      if verbose then L.stderr "@.--@.PROC: %a@." Typ.Procname.pp proc_name;
      let transitions = DFDead.run tenv proc_desc State.initial in
      let exit_node = Procdesc.get_exit_node proc_desc in
      match transitions exit_node with
      | DFDead.Transition (pre_final_s, _, _) ->
          let final_s = State.add_visited exit_node pre_final_s in
          check_final_state tenv proc_name proc_desc final_s
      | DFDead.Dead_state -> ()
    end in

  do_check ();
  Specs.get_summary_unsafe "callback_check_dead_code" proc_name
