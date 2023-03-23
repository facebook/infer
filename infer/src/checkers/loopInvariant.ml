(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open AbstractDomain.Types
module L = Logging
module InvariantVars = AbstractDomain.FiniteSet (Var)
module VarsInLoop = AbstractDomain.FiniteSet (Var)
module InvalidatedVars = AbstractDomain.FiniteSet (Var)
module LoopNodes = AbstractDomain.FiniteSet (Procdesc.Node)
module VarSet = AbstractDomain.FiniteSet (Var)

let debug fmt = L.(debug Analysis Medium) fmt

(** Map loop header node -> all nodes in the loop *)
module LoopHeadToLoopNodes = Procdesc.NodeMap

let is_defined_outside loop_nodes reaching_defs var =
  ReachingDefs.ReachingDefsMap.find_opt var reaching_defs
  |> Option.map ~f:(fun def_nodes -> LoopNodes.inter def_nodes loop_nodes |> LoopNodes.is_empty)
  |> Option.value ~default:true


let is_not_modeled tenv callee_pname =
  match PurityModels.ProcName.dispatch tenv callee_pname with Some _ -> false | None -> true


let get_purity tenv ~is_pure_by_default ~get_callee_purity callee_pname =
  (* Take into account purity behavior of modeled functions *)
  match PurityModels.ProcName.dispatch tenv callee_pname with
  | Some callee_purity ->
      callee_purity
  | None -> (
      debug "No model for %a \n" Procname.pp callee_pname ;
      (* If there is no model, invoke purity analysis to see if function is pure *)
      match get_callee_purity callee_pname with
      | Some purity_summary ->
          purity_summary
      | _ ->
          if is_pure_by_default then PurityDomain.pure else PurityDomain.impure_global )


let is_non_primitive typ = Typ.is_pointer typ || Typ.is_struct typ

(* check if the def of var is unique and invariant *)
let is_def_unique_and_satisfy tenv var (loop_nodes : LoopNodes.t) ~is_pure_by_default
    ~get_callee_purity is_exp_invariant =
  let equals_var id = Var.equal var (Var.of_id id) in
  match LoopNodes.is_singleton_or_more loop_nodes with
  | IContainer.Singleton node ->
      Procdesc.Node.get_instrs node
      |> Instrs.exists ~f:(function
           | Sil.Load {id; e= exp_rhs} when equals_var id && is_exp_invariant exp_rhs ->
               true
           | Sil.Store {e1= exp_lhs; e2= exp_rhs}
             when Exp.equal exp_lhs (Var.to_exp var) && is_exp_invariant exp_rhs ->
               true
           | Sil.Call ((id, _), Const (Cfun callee_pname), args, _, _) when equals_var id ->
               PurityDomain.is_pure
                 (get_purity tenv ~is_pure_by_default ~get_callee_purity callee_pname)
               && (* check if all params are invariant *)
               List.for_all ~f:(fun (exp, _) -> is_exp_invariant exp) args
           | _ ->
               false )
  | _ ->
      false


let is_exp_invariant inv_vars invalidated_vars loop_nodes reaching_defs exp =
  Var.get_all_vars_in_exp exp
  |> Sequence.for_all ~f:(fun var ->
         (not (InvalidatedVars.mem var invalidated_vars))
         && (InvariantVars.mem var inv_vars || is_defined_outside loop_nodes reaching_defs var) )


let get_vars_in_loop loop_nodes =
  LoopNodes.fold
    (fun node acc ->
      Procdesc.Node.get_instrs node
      |> Instrs.fold
           ~f:(fun acc instr ->
             match instr with
             | Sil.Load {id; e= exp_rhs} ->
                 Var.get_all_vars_in_exp exp_rhs
                 |> Sequence.fold
                      ~init:(VarsInLoop.add (Var.of_id id) acc)
                      ~f:(fun acc var -> VarsInLoop.add var acc)
             | Sil.Store {e1= exp_lhs; e2= exp_rhs} ->
                 Var.get_all_vars_in_exp exp_rhs
                 |> Sequence.append (Var.get_all_vars_in_exp exp_lhs)
                 |> Sequence.fold ~init:acc ~f:(fun acc var -> VarsInLoop.add var acc)
             | Sil.Call ((ret_id, _), _, args, _, _) ->
                 List.fold
                   ~init:(VarsInLoop.add (Var.of_id ret_id) acc)
                   ~f:(fun acc (arg_exp, _) ->
                     Var.get_all_vars_in_exp arg_exp
                     |> Sequence.fold ~init:acc ~f:(fun acc var -> VarsInLoop.add var acc) )
                   args
             | Sil.Prune (exp, _, _, _) ->
                 Var.get_all_vars_in_exp exp
                 |> Sequence.fold ~init:acc ~f:(fun acc var -> VarsInLoop.add var acc)
             | _ ->
                 acc )
           ~init:acc )
    loop_nodes VarsInLoop.empty


module ProcessedPair = struct
  type t = Var.t * Procdesc.Node.t [@@deriving compare]
end

module ProcessedPairSet = Caml.Set.Make (ProcessedPair)

(* get all the ptr variables (and their dependencies) occurring on the
   RHS of the definition of a given variable. *)
let get_ptr_vars_in_defn_path node loop_head var =
  let rec aux node var processed_pairs acc =
    if ProcessedPairSet.mem (var, node) processed_pairs then acc
    else
      let invalidate_exp exp_rhs init : InvalidatedVars.t =
        let processed_pairs' = ProcessedPairSet.add (var, node) processed_pairs in
        Var.get_all_vars_in_exp exp_rhs
        |> Sequence.fold ~init ~f:(fun acc rhs_var ->
               aux node rhs_var processed_pairs' (InvalidatedVars.add rhs_var acc) )
      in
      let acc =
        Procdesc.Node.get_instrs node
        |> Instrs.fold ~init:acc ~f:(fun acc instr ->
               match instr with
               | Sil.Load {id; e= exp_rhs; typ}
                 when Var.equal var (Var.of_id id) && is_non_primitive typ ->
                   invalidate_exp exp_rhs acc
               | Sil.Store {e1= Exp.Lvar pvar; typ; e2= exp_rhs}
                 when Var.equal var (Var.of_pvar pvar) && is_non_primitive typ ->
                   invalidate_exp exp_rhs acc
               | _ ->
                   acc )
      in
      if Procdesc.Node.equal node loop_head then acc
      else
        Procdesc.Node.get_preds node
        |> List.fold_left ~init:acc ~f:(fun acc node_pre ->
               match Procdesc.Node.get_kind node_pre with
               | Stmt_node _ ->
                   aux node_pre var (ProcessedPairSet.add (var, node) processed_pairs) acc
               | _ ->
                   acc )
  in
  aux node var ProcessedPairSet.empty InvalidatedVars.empty


let get_vars_to_invalidate node loop_head args modified_params invalidated_vars : InvalidatedVars.t
    =
  List.foldi ~init:invalidated_vars
    ~f:(fun i acc (arg_exp, typ) ->
      if PurityDomain.ModifiedParamIndices.mem i modified_params then (
        debug "Invalidate %a \n" Exp.pp arg_exp ;
        Var.get_all_vars_in_exp arg_exp
        |> Sequence.fold ~init:acc ~f:(fun acc var ->
               if is_non_primitive typ then
                 let dep_vars = get_ptr_vars_in_defn_path node loop_head var in
                 InvalidatedVars.union dep_vars (InvalidatedVars.add var acc)
               else acc ) )
      else acc )
    args


let is_pure get_callee_purity callee_pname =
  match get_callee_purity callee_pname with
  | Some purity_summary ->
      PurityDomain.is_pure purity_summary
  | None ->
      false


let all_unmodeled_modified tenv loop_nodes ~get_callee_purity =
  LoopNodes.fold
    (fun node acc ->
      Procdesc.Node.get_instrs node
      |> Instrs.fold ~init:acc ~f:(fun acc instr ->
             match instr with
             | Sil.Call ((id, _), Const (Cfun callee_pname), _, _, _)
               when is_not_modeled tenv callee_pname && not (is_pure get_callee_purity callee_pname)
               ->
                 debug "Invalidate unmodeled %a \n" Ident.pp id ;
                 InvalidatedVars.add (Var.of_id id) acc
             | _ ->
                 acc ) )
    loop_nodes InvalidatedVars.empty


(* If there is a call to an impure function in the loop, invalidate
   all its non-primitive arguments. Once invalidated, it should be
   never added again. *)
let get_invalidated_vars_in_loop tenv loop_head ~is_pure_by_default ~get_callee_purity loop_nodes =
  let all_unmodeled_modified = lazy (all_unmodeled_modified tenv loop_nodes ~get_callee_purity) in
  LoopNodes.fold
    (fun node acc ->
      Procdesc.Node.get_instrs node
      |> Instrs.fold ~init:acc ~f:(fun acc instr ->
             match instr with
             | Sil.Call ((id, _), Const (Cfun callee_pname), args, _, _) -> (
                 let purity = get_purity tenv ~is_pure_by_default ~get_callee_purity callee_pname in
                 PurityDomain.(
                   match purity with
                   | Top ->
                       (* modified global *)
                       (* if one of the callees modifies a global static
                          variable, invalidate all unmodeled function calls + args *)
                       let all_params = PurityDomain.all_params_modified args in
                       let invalidated_args =
                         get_vars_to_invalidate node loop_head args all_params
                           (InvalidatedVars.add (Var.of_id id) acc)
                       in
                       InvalidatedVars.union invalidated_args (force all_unmodeled_modified)
                   | NonTop modified_params ->
                       if ModifiedParamIndices.is_empty modified_params then (*pure*)
                         acc
                       else
                         get_vars_to_invalidate node loop_head args modified_params
                           (InvalidatedVars.add (Var.of_id id) acc) ) )
             | _ ->
                 acc ) )
    loop_nodes InvalidatedVars.empty


(* A variable is invariant if
     - its reaching definition is outside of the loop
     - o.w. its definition is constant or invariant itself *)
let get_inv_vars_in_loop tenv reaching_defs_invariant_map ~is_pure_by_default ~get_callee_purity
    loop_head loop_nodes =
  let process_var_once var inv_vars invalidated_vars =
    (* if a variable is marked invariant once, it can't be invalidated
       (i.e. invariance is monotonic) *)
    if InvariantVars.mem var inv_vars || Var.is_none var || InvalidatedVars.mem var invalidated_vars
    then (inv_vars, false)
    else
      let loop_head_id = Procdesc.Node.get_id loop_head in
      ReachingDefs.extract_post loop_head_id reaching_defs_invariant_map
      |> Option.map ~f:(fun reaching_defs ->
             ReachingDefs.ReachingDefsMap.find_opt var reaching_defs
             |> Option.map ~f:(fun def_nodes ->
                    let in_loop_defs = LoopNodes.inter loop_nodes def_nodes in
                    (* reaching definition is outside of the loop *)
                    if LoopNodes.is_empty in_loop_defs then (InvariantVars.add var inv_vars, true)
                    else if
                      (* its definition is unique and invariant *)
                      is_def_unique_and_satisfy tenv var def_nodes ~is_pure_by_default
                        ~get_callee_purity
                        (is_exp_invariant inv_vars invalidated_vars loop_nodes reaching_defs)
                    then (InvariantVars.add var inv_vars, true)
                    else (inv_vars, false) )
             |> Option.value (* if a var is not declared, it must be invariant *)
                  ~default:(inv_vars, false) )
      |> Option.value ~default:(inv_vars, false)
  in
  let vars_in_loop = get_vars_in_loop loop_nodes in
  (* until there are no changes to inv_vars, keep repeatedly
     processing all the variables that occur in the loop nodes *)
  let invalidated_vars =
    get_invalidated_vars_in_loop tenv loop_head ~is_pure_by_default ~get_callee_purity loop_nodes
  in
  let rec find_fixpoint inv_vars =
    let inv_vars', modified =
      InvariantVars.fold
        (fun var (inv_vars, is_mod) ->
          let inv_vars', is_mod' = process_var_once var inv_vars invalidated_vars in
          (inv_vars', is_mod || is_mod') )
        vars_in_loop (inv_vars, false)
    in
    if modified then find_fixpoint inv_vars' else inv_vars'
  in
  debug "\n>>> Invalidated vars: %a\n" InvalidatedVars.pp invalidated_vars ;
  find_fixpoint InvariantVars.empty


(** Map loop head -> invariant vars in loop *)
module LoopHeadToInvVars = Procdesc.NodeMap

type invariant_map = VarSet.t LoopHeadToInvVars.t

(** This is invoked by cost analysis, hence assume that unmodeled calls are pure by default *)
let get_loop_inv_var_map tenv get_callee_purity reaching_defs_invariant_map loop_head_to_loop_nodes
    : invariant_map =
  LoopHeadToLoopNodes.fold
    (fun loop_head loop_nodes inv_map ->
      let inv_vars_in_loop =
        get_inv_vars_in_loop tenv reaching_defs_invariant_map loop_head loop_nodes
          ~is_pure_by_default:true ~get_callee_purity
      in
      L.(debug Analysis Medium)
        "@\n>>> loop head: %a --> inv vars: %a @\n" Procdesc.Node.pp loop_head InvariantVars.pp
        inv_vars_in_loop ;
      LoopHeadToInvVars.add loop_head inv_vars_in_loop inv_map )
    loop_head_to_loop_nodes LoopHeadToInvVars.empty
