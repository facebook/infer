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

let canonicalize_tb astate =
  PulseTreeBorrows.canonicalize ~f:(get_var_repr astate) (AbductiveDomain.get_tree_borrows astate)


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


let init_formals formals ~tree_borrows (astate : AbductiveDomain.t) =
  let get_var_repr = get_var_repr astate in
  let post = (astate.AbductiveDomain.post :> BaseDomain.t) in
  let cell_of pvar =
    Option.map
      (UnsafeStack.find_opt (Var.of_pvar pvar) post.stack)
      ~f:(fun vo -> get_var_repr (fst (ValueOrigin.addr_hist vo)))
  in
  let pointee_of pvar =
    Option.bind
      (UnsafeStack.find_opt (Var.of_pvar pvar) post.stack)
      ~f:(fun vo ->
        UnsafeMemory.find_edge_opt ~get_var_repr
          (fst (ValueOrigin.addr_hist vo))
          Dereference post.heap
        |> Option.map ~f:(fun (p, _) -> get_var_repr p) )
  in
  AbductiveDomain.set_tree_borrows
    (PulseTreeBorrows.init_formals formals ~cell_of ~borrowed_cell_of:pointee_of ~tree_borrows
       ~succs:(succs_of astate) (canonicalize_tb astate) )
    astate


let exec_load ~id ~e ~typ ~loc (astate : AbductiveDomain.t) =
  let src = operand_of_exp astate e in
  AbductiveDomain.set_tree_borrows
    (PulseTreeBorrows.exec_load ~id ~typ ~src ~succs:(succs_of astate) ~loc (canonicalize_tb astate))
    astate


let exec_store ~lhs ~rhs ~typ ~loc (astate : AbductiveDomain.t) =
  let lhs = operand_of_exp astate lhs in
  let rhs = operand_of_exp astate rhs in
  AbductiveDomain.set_tree_borrows
    (PulseTreeBorrows.exec_store ~lhs ~rhs ~typ ~succs:(succs_of astate) ~loc
       (canonicalize_tb astate) )
    astate


let callee_entry_edges callee_summary callee_pdesc =
  let pre = AbductiveDomain.Summary.get_pre callee_summary in
  let succs av = succs_of_heap ~get_var_repr:Fn.id pre.BaseDomain.heap av in
  let roots =
    Procdesc.get_pvar_formals callee_pdesc
    |> List.filter_map ~f:(fun (pvar, _) ->
        Option.bind
          (UnsafeStack.find_opt (Var.of_pvar pvar) pre.BaseDomain.stack)
          ~f:(fun vo ->
            UnsafeMemory.find_edge_opt ~get_var_repr:Fn.id
              (fst (ValueOrigin.addr_hist vo))
              Dereference pre.BaseDomain.heap
            |> Option.map ~f:fst ) )
  in
  let rec go visited acc frontier =
    match frontier with
    | [] ->
        List.rev acc
    | a :: rest ->
        let children = succs a |> List.filter ~f:(fun c -> not (AbstractValue.Set.mem c visited)) in
        let visited = List.fold children ~init:visited ~f:(fun v c -> AbstractValue.Set.add c v) in
        let acc = List.fold children ~init:acc ~f:(fun acc c -> (a, c) :: acc) in
        go visited acc (rest @ children)
  in
  go (AbstractValue.Set.of_list roots) [] roots


let actual_operands actuals astate = List.map actuals ~f:(fun e -> operand_of_exp astate e)

let precondition_of_actuals actuals astate =
  PulseTreeBorrows.precondition_of_actuals (canonicalize_tb astate) (actual_operands actuals astate)


let exec_call ~callee_summary ~callee_pdesc
    ~(subst_map : (AbstractValue.t * ValueHistory.t) AbstractValue.Map.t) ~args ~ret_id ~loc astate
    =
  let get_var_repr = get_var_repr astate in
  let subst av =
    AbstractValue.Map.find_opt av subst_map
    |> Option.map ~f:(fun (caller_av, _hist) -> get_var_repr caller_av)
  in
  let callee_state = AbductiveDomain.Summary.get_tree_borrows callee_summary in
  let callee_edges = callee_entry_edges callee_summary callee_pdesc in
  let callee_ret_cell =
    let post = AbductiveDomain.Summary.get_post callee_summary in
    UnsafeStack.find_opt (Var.of_pvar (Procdesc.get_ret_var callee_pdesc)) post.BaseDomain.stack
    |> Option.map ~f:(fun vo -> fst (ValueOrigin.addr_hist vo))
  in
  AbductiveDomain.set_tree_borrows
    (PulseTreeBorrows.exec_call ~callee_state ~callee_pdesc ~subst ~callee_edges ~callee_ret_cell
       ~args:(actual_operands args astate) ~ret_id ~succs:(succs_of astate) ~loc
       (canonicalize_tb astate) )
    astate


let compute_specialization ~(formals : (Pvar.t * Typ.t) list) (actuals : Exp.t list) astate :
    Specialization.Pulse.t option =
  let tb_pre = precondition_of_actuals actuals astate in
  let needs_spec =
    (not (List.is_empty tb_pre.Specialization.Pulse.TreeBorrows.rels))
    || PulseTreeBorrows.perm_spec_needed ~formals tb_pre
  in
  if needs_spec then Some {Specialization.Pulse.bottom with tree_borrows= tb_pre} else None


let graft_call ~callee_summary ~callee_pname ~tb_arg_exps ~subst_map ~ret_id ~loc ~caller post =
  let caller_pre = precondition_of_actuals tb_arg_exps caller in
  let callee_pre =
    PulseTreeBorrows.entry_pre (AbductiveDomain.Summary.get_tree_borrows callee_summary)
  in
  if not (PulseTreeBorrows.spec_fits ~caller_pre ~callee_pre) then post
  else
    match Procdesc.load callee_pname with
    | None ->
        post
    | Some callee_pdesc ->
        exec_call ~callee_summary ~callee_pdesc ~subst_map ~args:tb_arg_exps ~ret_id ~loc post


let report_errors proc_desc err_log summary =
  PulseTreeBorrows.report_errors proc_desc err_log
    (AbductiveDomain.Summary.get_tree_borrows summary)


let exec_retag ~dst_exp ~src_exp ~is_mut ~loc (astate : AbductiveDomain.t) =
  let dst = operand_of_exp astate dst_exp in
  let src = operand_of_exp astate src_exp in
  AbductiveDomain.set_tree_borrows
    (PulseTreeBorrows.exec_retag ~dst ~src ~is_mut ~protected:false ~succs:(succs_of astate) ~loc
       (canonicalize_tb astate) )
    astate
