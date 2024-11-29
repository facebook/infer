(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
module AbductiveDomain = PulseAbductiveDomain
module BaseMemory = PulseBaseMemory
module PathContext = PulsePathContext
module Stack = AbductiveDomain.Stack

let join_histories (_hist : ValueHistory.t option) (_hist' : ValueHistory.t option) =
  (* TODO: something *) ValueHistory.epoch


let record_join_value _v_join _v1 _v2 formula =
  (* TODO: record equality [v_join \in {v1, v2}] *) formula


let join_values_hists (v_lhs, hist_lhs) (v_rhs, hist_rhs) formula_join =
  let formula_join, v_join =
    if AbstractValue.equal v_lhs v_rhs then
      (* [x↦v ⊔ x↦v = x↦v], use the fact that abstract values are unique even across disjuncts
         so their valuations can be shared, i.e. we can keep [v] as the representative in the
         joined state. *)
      (formula_join, v_lhs)
    else
      (* [x↦v ⊔ x↦v' = x↦v''], [v''] fresh *)
      let v_join = AbstractValue.mk_fresh () in
      let formula_join = record_join_value v_join v_lhs v_rhs formula_join in
      (formula_join, v_join)
  in
  let hist_join = join_histories (Some hist_lhs) (Some hist_rhs) in
  (formula_join, (v_join, hist_join))


let join_values_hists_opts (astate_lhs, v_hist_lhs_opt) (astate_rhs, v_hist_rhs_opt) formula_join =
  match (astate_lhs, v_hist_lhs_opt, astate_rhs, v_hist_rhs_opt) with
  | _, None, _, None ->
      (* we assume at least one side has a value otherwise what are we even joining? *)
      assert false
  | _, Some (_v_one_sided, hist_one_sided), _, None | _, None, _, Some (_v_one_sided, hist_one_sided)
    ->
      (* [x↦v ⊔ emp = x↦v'], [v'] fresh. [x↦v ⊔ emp] is equivalent (in pulse) to [x↦v ⊔ x↦w], [w]
         fresh, unless [x] in invalid, so this gives [v'=v ∨ v'=w]. Since [w] is fresh, this is
         equivalent to [v'=w], and since [v'] is fresh this is equivalent to just [true] (i.e. don't
         add any equalities on [v']). *)
      let v_join = AbstractValue.mk_fresh () in
      let hist_join = join_histories (Some hist_one_sided) None in
      (formula_join, (v_join, hist_join))
  | _, Some v_hist_lhs, _, Some v_hist_rhs
    when AbstractValue.equal (fst v_hist_lhs) (fst v_hist_rhs) ->
      (* [x↦v ⊔ x↦v = x↦v], use the fact that abstract values are unique even across disjuncts
         so their valuations can be shared, i.e. we can keep [v] as the representative in the
         joined state. *)
      (formula_join, v_hist_lhs)
  | _, Some v_hist_lhs, _, Some v_hist_rhs (* [v_lhs ≠ v_rhs] *) ->
      (* [x↦v ⊔ x↦v' = x↦v''], [v''] fresh *)
      join_values_hists v_hist_lhs v_hist_rhs formula_join


let join_value_origins (astate_lhs, vo_lhs_opt) (astate_rhs, vo_rhs_opt) formula_join =
  (* TODO: actually join value origins, not just the underlying values and histories, i.e. do better
       than [Unknown] here *)
  let formula_join, v_hist_join =
    join_values_hists_opts
      (astate_lhs, Option.map ~f:ValueOrigin.addr_hist vo_lhs_opt)
      (astate_rhs, Option.map ~f:ValueOrigin.addr_hist vo_rhs_opt)
      formula_join
  in
  (formula_join, ValueOrigin.unknown v_hist_join)


let join_stacks astate_lhs astate_rhs =
  let join_stack_values (formula_join, heap_join) _var vo_lhs_opt vo_rhs_opt =
    let formula_join, vo_join =
      join_value_origins (astate_lhs, vo_lhs_opt) (astate_rhs, vo_rhs_opt) formula_join
    in
    (* TODO: compute the disjunction of the heap in [astate_lhs] rooted at [vo_lhs_opt] and the heap
       at [astate_rhs] rooted in [vo_rhs_opt] *)
    ((formula_join, heap_join), Some vo_join)
  in
  let (formula_join, heap_pre_join), stack_pre_join =
    Stack.fold_merge `Pre astate_lhs astate_rhs
      ~init:(astate_lhs.AbductiveDomain.path_condition, BaseMemory.empty)
      ~f:join_stack_values
  in
  let (formula_join, heap_post_join), stack_post_join =
    Stack.fold_merge `Post astate_lhs astate_rhs ~init:(formula_join, BaseMemory.empty)
      ~f:join_stack_values
  in
  AbductiveDomain.mk_join_state ~pre:(stack_pre_join, heap_pre_join)
    ~post:(stack_post_join, heap_post_join) formula_join


let join_attributes _astate_lhs _astate_rhs join_astate =
  (* TODO *)
  join_astate


let join_formulas _astate_lhs _astate_rhs join_astate =
  (* TODO *)
  join_astate


let join_abductive astate_lhs astate_rhs =
  let join_astate = join_stacks astate_lhs astate_rhs in
  let join_astate = join_attributes astate_lhs astate_rhs join_astate in
  let join_astate = join_formulas astate_lhs astate_rhs join_astate in
  join_astate


let join (astate_lhs, path_lhs) (astate_rhs, path_rhs) =
  (join_abductive astate_lhs astate_rhs, PathContext.join path_lhs path_rhs)
