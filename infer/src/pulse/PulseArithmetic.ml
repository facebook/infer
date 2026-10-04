(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
module AbductiveDomain = PulseAbductiveDomain
module AccessResult = PulseAccessResult

let map_path_condition_common ~f astate =
  let open SatUnsat.Import in
  let* phi, new_eqs = f astate.AbductiveDomain.path_condition in
  let astate = AbductiveDomain.set_path_condition phi astate in
  let astate =
    AbductiveDomain.map_loop_header_formulas astate ~f:(fun phi ->
        match f phi with Sat (phi, _) -> phi | Unsat _ -> phi )
  in
  let+ result =
    AbductiveDomain.incorporate_new_eqs new_eqs astate >>| AccessResult.of_abductive_result
  in
  (result, new_eqs)


let map_path_condition ~f astate =
  let open SatUnsat.Import in
  map_path_condition_common ~f astate >>| fst


let map_path_condition_with_ret ~f astate ret =
  let open SatUnsat.Import in
  let+ result, new_eqs = map_path_condition_common ~f astate in
  PulseResult.map result ~f:(fun result ->
      (result, AbductiveDomain.incorporate_new_eqs_on_val new_eqs ret) )


let literal_zero = Formula.ConstOperand (Cint IntLit.zero)

let and_nonnegative v astate =
  map_path_condition astate ~f:(fun phi ->
      Formula.and_less_equal literal_zero (AbstractValueOperand v) phi )


let and_positive v astate =
  map_path_condition astate ~f:(fun phi ->
      Formula.and_less_than literal_zero (AbstractValueOperand v) phi )


let and_eq_const v c astate =
  map_path_condition astate ~f:(fun phi ->
      Formula.and_equal (AbstractValueOperand v) (ConstOperand c) phi )


let and_eq_int v i astate = and_eq_const v (Cint i) astate

type operand = Formula.operand =
  | AbstractValueOperand of AbstractValue.t
  | ConstOperand of Const.t
  | FunctionApplicationOperand of {f: PulseFormula.function_symbol; actuals: AbstractValue.t list}

let and_equal lhs rhs astate =
  map_path_condition astate ~f:(fun phi -> Formula.and_equal lhs rhs phi)


let and_not_equal lhs rhs astate =
  map_path_condition astate ~f:(fun phi -> Formula.and_not_equal lhs rhs phi)


let eval_binop ret binop lhs rhs astate =
  map_path_condition_with_ret astate ret ~f:(fun phi ->
      Formula.and_equal_binop ret binop lhs rhs phi )


let eval_binop_absval ret binop lhs rhs astate =
  eval_binop ret binop (AbstractValueOperand lhs) (AbstractValueOperand rhs) astate


let eval_unop ret unop v astate =
  map_path_condition_with_ret astate ret ~f:(fun phi ->
      Formula.and_equal_unop ret unop (AbstractValueOperand v) phi )


let prune_binop ?depth ~negated binop ?ifkind:(need_atom = false) lhs rhs astate =
  map_path_condition astate ~f:(fun phi ->
      Formula.prune_binop ?depth ~negated binop ~need_atom lhs rhs phi )


let and_equal_string_concat ret lhs rhs astate =
  map_path_condition astate ~f:(fun phi -> Formula.and_equal_string_concat ret lhs rhs phi)


let literal_zero = ConstOperand (Cint IntLit.zero)

let literal_one = ConstOperand (Cint IntLit.one)

let prune_eq_zero v astate =
  prune_binop ~negated:false Eq (AbstractValueOperand v) literal_zero astate


let prune_ne_zero v astate =
  prune_binop ~negated:false Ne (AbstractValueOperand v) literal_zero astate


let prune_nonnegative ?depth v astate =
  prune_binop ?depth ~negated:false Ge (AbstractValueOperand v) literal_zero astate


let prune_positive v astate =
  prune_binop ~negated:false Gt (AbstractValueOperand v) literal_zero astate


let prune_gt_one v astate =
  prune_binop ~negated:false Gt (AbstractValueOperand v) literal_one astate


let prune_eq_one v astate =
  prune_binop ~negated:false Eq (AbstractValueOperand v) literal_one astate


let is_known_zero astate v = Formula.is_known_zero astate.AbductiveDomain.path_condition v

let is_allocated summary v =
  AbductiveDomain.Summary.is_heap_allocated summary v
  || AbductiveDomain.Summary.get_must_be_valid v summary |> Option.is_some


let is_manifest summary =
  Formula.is_manifest
    (AbductiveDomain.Summary.get_path_condition summary)
    ~is_allocated:(is_allocated summary)
  && not (AbductiveDomain.Summary.pre_heap_has_assumptions summary)


module PreAccessPath = struct
  type t = Var.t * Access.t list [@@deriving compare]
end

module PreAccessPathMap = Stdlib.Map.Make (PreAccessPath)

(** the access paths from the variables of the precondition to each value of the precondition *)
let pre_access_paths (summary : AbductiveDomain.Summary.t) =
  let add root_var paths v rev_accesses =
    (* values reached with no accesses are the addresses of the variables themselves, or array
       indices that the traversal restarts from *)
    if List.is_empty rev_accesses then paths
    else
      AbstractValue.Map.update v
        (fun paths_opt -> Some ((root_var, rev_accesses) :: Option.value paths_opt ~default:[]))
        paths
  in
  AbductiveDomain.fold_all
    (summary :> AbductiveDomain.t)
    `Pre ~init:AbstractValue.Map.empty ~finish:Fn.id
    ~f:(fun root_var paths v rev_accesses ->
      Continue_or_stop.Continue (add root_var paths v rev_accesses) )
    ~f_revisit:add


let is_manifest_disjunction summaries =
  let summaries = List.map summaries ~f:(fun summary -> (summary, pre_access_paths summary)) in
  (* give up if a precondition assumes that two access paths are aliases (cells equal to the same
     constant are not, see [Summary.pre_heap_has_assumptions]) or if several values have the same
     access path, which can happen below array indices since the traversal of the precondition
     restarts from them *)
  let has_unique_paths (summary, paths) =
    let phi = AbductiveDomain.Summary.get_path_condition summary in
    AbstractValue.Map.for_all
      (fun v paths ->
        List.length paths <= 1 || Option.is_some (Formula.get_constant_condition_depth phi v) )
      paths
    && not
         ( AbstractValue.Map.fold (fun _ paths all_paths -> paths @ all_paths) paths []
         |> List.contains_dup ~compare:PreAccessPath.compare )
  in
  List.for_all summaries ~f:has_unique_paths
  &&
  (* name the values of the preconditions after their access paths so that the conditions of
     different summaries can be related *)
  let canonical_vars =
    List.fold summaries ~init:PreAccessPathMap.empty ~f:(fun canonical_vars (_, paths) ->
        AbstractValue.Map.fold
          (fun _ paths canonical_vars ->
            List.fold paths ~init:canonical_vars ~f:(fun canonical_vars path ->
                if PreAccessPathMap.mem path canonical_vars then canonical_vars
                else PreAccessPathMap.add path (AbstractValue.mk_fresh ()) canonical_vars ) )
          paths canonical_vars )
  in
  (* the conditions of [summary] that are compatible with it being manifest, and all of its
     conditions together with the assumptions encoded by restricted values (see [is_manifest]) *)
  let conditions (summary, paths) =
    let phi = AbductiveDomain.Summary.get_path_condition summary in
    let names v =
      match AbstractValue.Map.find_opt (Formula.get_var_repr phi v) paths with
      | None ->
          [v]
      | Some paths ->
          List.map paths ~f:(fun path -> PreAccessPathMap.find path canonical_vars)
    in
    (* a value with several access paths is a constant, so a condition on that value stands for the
       same condition on each of these access paths *)
    let instantiate atom =
      let vars =
        Formula.Atom.fold_variables atom ~init:AbstractValue.Set.empty
          ~f:(Fn.flip AbstractValue.Set.add)
      in
      AbstractValue.Set.fold
        (fun v substs ->
          List.concat_map (names v) ~f:(fun name ->
              List.map substs ~f:(AbstractValue.Map.add v name) ) )
        vars [AbstractValue.Map.empty]
      |> List.map ~f:(fun subst ->
          Formula.Atom.map_variables atom ~f:(fun v ->
              AbstractValue.Map.find_opt v subst |> Option.value ~default:v ) )
    in
    let manifest, latent =
      Formula.partition_conditions_by_manifest ~is_allocated:(is_allocated summary) phi
    in
    let pre_values =
      AbstractValue.Map.fold
        (fun v _ vs -> AbstractValue.Set.add v vs)
        paths AbstractValue.Set.empty
    in
    let nonnegative vs =
      AbstractValue.Set.filter AbstractValue.is_restricted vs
      |> AbstractValue.Set.elements
      |> List.map ~f:Formula.Atom.nonnegative
    in
    (* restricted values of the precondition are assumptions (see [pre_heap_has_assumptions]),
       other restricted variables are nonnegative by construction *)
    let assumed = nonnegative pre_values in
    let internal =
      List.fold (manifest @ latent) ~init:AbstractValue.Set.empty ~f:(fun vs atom ->
          Formula.Atom.fold_variables atom ~init:vs ~f:(Fn.flip AbstractValue.Set.add) )
      |> Fn.flip AbstractValue.Set.diff pre_values
      |> nonnegative
    in
    let manifest = List.concat_map (manifest @ internal) ~f:instantiate in
    (manifest, manifest @ List.concat_map (latent @ assumed) ~f:instantiate)
  in
  let conditions = List.map summaries ~f:conditions in
  Formula.disjunction_is_valid
    ~assuming:
      (List.map conditions ~f:fst |> List.dedup_and_sort ~compare:[%compare: Formula.Atom.t list])
    (List.map conditions ~f:snd)


let and_is_int v ikind astate =
  map_path_condition astate ~f:(fun phi -> Formula.and_is_int v ikind phi)


let and_equal_instanceof v1 v2 t ?(nullable = false) astate =
  map_path_condition astate ~f:(fun phi -> Formula.and_equal_instanceof v1 v2 t ~nullable phi)


let and_dynamic_type_is v t ?source_file astate =
  map_path_condition astate ~f:(fun phi -> Formula.and_dynamic_type v t ?source_file phi)


let get_dynamic_type v astate = Formula.get_dynamic_type v astate.AbductiveDomain.path_condition

(* this is just to ease migration of previous calls to PulseOperations.add_dynamic_type, which can't fail *)
let and_dynamic_type_is_unsafe v t ?source_file location astate =
  let phi =
    Formula.add_dynamic_type_unsafe v t ?source_file location astate.AbductiveDomain.path_condition
  in
  AbductiveDomain.set_path_condition phi astate


let copy_type_constraints v_src v_target astate =
  let phi = Formula.copy_type_constraints v_src v_target astate.AbductiveDomain.path_condition in
  AbductiveDomain.set_path_condition phi astate


let absval_of_int astate i =
  let phi, v = Formula.absval_of_int astate.AbductiveDomain.path_condition i in
  let astate = AbductiveDomain.set_path_condition phi astate in
  (astate, v)


let absval_of_string astate s =
  let phi, v = Formula.absval_of_string astate.AbductiveDomain.path_condition s in
  let astate = AbductiveDomain.set_path_condition phi astate in
  (astate, v)


let as_constant_string astate v = Formula.as_constant_string astate.AbductiveDomain.path_condition v
