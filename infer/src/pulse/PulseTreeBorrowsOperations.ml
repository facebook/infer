(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface
module Operand = PulseTreeBorrows.Operand

let get_var_repr astate v = Formula.get_var_repr astate.AbductiveDomain.path_condition v

let succs_of_heap ~get_var_repr heap av =
  match UnsafeMemory.find_opt av heap with
  | None ->
      []
  | Some edges ->
      UnsafeMemory.Edges.fold edges ~init:[] ~f:(fun acc (access, (target, _hist)) ->
          match (access : PulseAccess.t) with
          | Dereference ->
              acc
          | FieldAccess _ | ArrayAccess _ ->
              get_var_repr target :: acc )
      |> List.dedup_and_sort ~compare:AbstractValue.compare


let succs_of astate =
  let get_var_repr = get_var_repr astate in
  let post = (astate.AbductiveDomain.post :> BaseDomain.t) in
  fun av -> succs_of_heap ~get_var_repr post.heap av


let operand_of_exp astate exp : Operand.t =
  let get_var_repr = get_var_repr astate in
  let post = (astate.AbductiveDomain.post :> BaseDomain.t) in
  let value_of_var v =
    Option.map (UnsafeStack.find_opt v post.stack) ~f:(fun vo ->
        get_var_repr (ValueOrigin.value vo) )
  in
  let field_cell parent fld =
    Option.map (UnsafeMemory.find_edge_opt ~get_var_repr parent (FieldAccess fld) post.heap)
      ~f:(fun (t, _) -> get_var_repr t )
  in
  let elem_cell arr idx_exp =
    let index_matches iav (idx_exp : Exp.t) =
      match idx_exp with
      | Exp.Var id -> (
        match value_of_var (Var.of_id id) with
        | Some iv ->
            AbstractValue.equal (get_var_repr iav) iv
        | None ->
            false )
      | Exp.Const (Cint n) -> (
        match Formula.as_constant_q astate.AbductiveDomain.path_condition iav with
        | Some q ->
            Q.equal q (Q.of_bigint (IntLit.to_big_int n))
        | None ->
            false )
      | _ ->
          false
    in
    Option.bind (UnsafeMemory.find_opt arr post.heap) ~f:(fun edges ->
        UnsafeMemory.Edges.fold edges ~init:None ~f:(fun acc (access, (target, _hist)) ->
            match acc with
            | Some _ ->
                acc
            | None -> (
              match (access : PulseAccess.t) with
              | ArrayAccess (_, iav) when index_matches iav idx_exp ->
                  Some (get_var_repr target)
              | _ ->
                  None ) ) )
  in
  let rec walk (e : Exp.t) : Operand.t option =
    match e with
    | Exp.Lvar pvar ->
        Option.map (value_of_var (Var.of_pvar pvar)) ~f:Operand.of_place
    | Exp.Var ident ->
        Some (Operand.of_temp ident (value_of_var (Var.of_id ident)))
    | Exp.Cast (_, e) ->
        walk e
    | Exp.Lfield ({exp= base}, fld, _) ->
        Option.bind (walk base) ~f:(fun op ->
            Option.map
              (Option.bind (Operand.last_cell op) ~f:(fun p -> field_cell p fld))
              ~f:(Operand.extend op) )
    | Exp.Lindex (base, idx_exp) ->
        Option.bind (walk base) ~f:(fun op ->
            Option.map
              (Option.bind (Operand.last_cell op) ~f:(fun a -> elem_cell a idx_exp))
              ~f:(Operand.extend op) )
    | _ ->
        None
  in
  match walk exp with Some op -> op | None -> Operand.untracked


let exec_load ~id:_ ~e:_ ~typ:_ ~loc:_ (astate : AbductiveDomain.t) = astate

let exec_store ~lhs:_ ~rhs:_ ~typ:_ ~loc:_ (astate : AbductiveDomain.t) = astate

let exec_retag ~dst_exp ~src_exp ~is_mut (astate : AbductiveDomain.t) =
  let dst = operand_of_exp astate dst_exp in
  let src = operand_of_exp astate src_exp in
  AbductiveDomain.set_tree_borrows
    (PulseTreeBorrows.exec_retag ~dst ~src ~is_mut ~protected:false ~succs:(succs_of astate)
       (AbductiveDomain.get_tree_borrows astate) )
    astate
