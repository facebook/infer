(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

type throws =
  | DontKnow  (** May or may not throw an exception. *)
  | Throws  (** Definitely throws an exception. *)
  | DoesNotThrow  (** Does not throw an exception. *)

(** Module type used to define the state component for a dataflow algorithm. *)
module type DFStateType = sig
  (** Type for state. *)
  type t

  val equal : t -> t -> bool
  (** Equality between states. *)

  val join : t -> t -> t
  (** Join two states (the old one is the first parameter). *)

  val do_node : Tenv.t -> Procdesc.Node.t -> t -> t list * t list
  (** Perform a state transition on a node. *)

  val proc_throws : Typ.Procname.t -> throws
  (** Can proc throw an exception? *)
end

(** Type for the dataflow API. *)
module type DF = sig
  type t

  type state

  type transition = Dead_state | Transition of state * state list * state list

  val join : state list -> state -> state

  val run : Tenv.t -> Procdesc.t -> state -> Procdesc.Node.t -> transition
end

(** Determine if the node can throw an exception. *)
let node_throws pdesc node (proc_throws : Typ.Procname.t -> throws) : throws =
  let instr_throws instr =
    let is_return pvar =
      let ret_pvar = Procdesc.get_ret_var pdesc in
      Pvar.equal pvar ret_pvar
    in
    match instr with
    | Sil.Store (Exp.Lvar pvar, _, Exp.Exn _, _) when is_return pvar ->
        (* assignment to return variable is an artifact of a throw instruction *)
        Throws
    | Sil.Call (_, Exp.Const (Const.Cfun callee_pn), _, _, _)
      when BuiltinDecl.is_declared callee_pn ->
        if Typ.Procname.equal callee_pn BuiltinDecl.__cast then DontKnow else DoesNotThrow
    | Sil.Call (_, Exp.Const (Const.Cfun callee_pn), _, _, _) ->
        proc_throws callee_pn
    | _ ->
        DoesNotThrow
  in
  let res = ref DoesNotThrow in
  let update_res throws =
    match (!res, throws) with
    | DontKnow, DontKnow ->
        res := DontKnow
    | Throws, _ | _, Throws ->
        res := Throws
    | DoesNotThrow, t | t, DoesNotThrow ->
        res := t
  in
  let do_instr instr = update_res (instr_throws instr) in
  Instrs.iter ~f:do_instr (Procdesc.Node.get_instrs node) ;
  !res


(** Create an instance of the dataflow algorithm given a state parameter. *)
module MakeDF (St : DFStateType) : DF with type state = St.t = struct
  module S = Procdesc.NodeSet
  module H = Procdesc.NodeHash

  type worklist = S.t

  type statemap = St.t H.t

  type statelistmap = St.t list H.t

  type t =
    { mutable worklist: worklist
    ; pre_states: statemap
    ; post_states: statelistmap
    ; exn_states: statelistmap
    ; proc_desc: Procdesc.t }

  type state = St.t

  type transition = Dead_state | Transition of state * state list * state list

  let join states initial_state = List.fold ~f:St.join ~init:initial_state states

  (** Propagate [new_state] to all the nodes immediately reachable. *)
  let propagate t node states_succ states_exn (throws : throws) =
    let propagate_to_dest new_state dest_node =
      let push_state s =
        H.replace t.pre_states dest_node s ;
        t.worklist <- S.add dest_node t.worklist
      in
      try
        let dest_state = H.find t.pre_states dest_node in
        let dest_joined = St.join dest_state new_state in
        if not (St.equal dest_state dest_joined) then push_state dest_joined
      with Caml.Not_found -> push_state new_state
    in
    let succ_nodes = Procdesc.Node.get_succs node in
    let exn_nodes = Procdesc.Node.get_exn node in
    if throws <> Throws then
      List.iter ~f:(fun s -> List.iter ~f:(propagate_to_dest s) succ_nodes) states_succ ;
    if throws <> DoesNotThrow then
      List.iter ~f:(fun s -> List.iter ~f:(propagate_to_dest s) exn_nodes) states_exn ;
    H.replace t.post_states node states_succ ;
    H.replace t.exn_states node states_exn


  (** Run the worklist-based dataflow algorithm. *)
  let run tenv proc_desc state =
    let t =
      let start_node = Procdesc.get_start_node proc_desc in
      let init_set = S.singleton start_node in
      let init_statemap =
        let m = H.create 1 in
        H.replace m start_node state ; m
      in
      { worklist= init_set
      ; pre_states= init_statemap
      ; post_states= H.create 0
      ; exn_states= H.create 0
      ; proc_desc }
    in
    let () =
      while not (S.is_empty t.worklist) do
        let node = S.min_elt t.worklist in
        t.worklist <- S.remove node t.worklist ;
        try
          let state = H.find t.pre_states node in
          let states_succ, states_exn = St.do_node tenv node state in
          propagate t node states_succ states_exn (node_throws proc_desc node St.proc_throws)
        with Caml.Not_found -> ()
      done
    in
    let transitions node =
      try Transition (H.find t.pre_states node, H.find t.post_states node, H.find t.exn_states node)
      with Caml.Not_found -> Dead_state
    in
    transitions
end

(* MakeDF *)

(** Example dataflow callback: compute the the distance from a node to the start node. *)
let _callback_test_dataflow {Callbacks.proc_desc; tenv; summary} =
  let verbose = false in
  let module DFCount = MakeDF (struct
    type t = int

    let equal = Int.equal

    let join n m = if Int.equal n 0 then m else n

    let do_node _ n s =
      if verbose then
        L.(debug Analysis Verbose) "visiting node %a with state %d@." Procdesc.Node.pp n s ;
      ([s + 1], [s + 1])


    let proc_throws _ = DoesNotThrow
  end) in
  let transitions = DFCount.run tenv proc_desc 0 in
  let do_node node =
    match transitions node with DFCount.Transition _ -> () | DFCount.Dead_state -> ()
  in
  List.iter ~f:do_node (Procdesc.get_nodes proc_desc) ;
  summary
