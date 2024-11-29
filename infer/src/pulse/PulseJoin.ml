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
module Memory = AbductiveDomain.Memory
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
         add any equalities on [v']).

         TODO: record a "weak invalidation" attribute (currently doesn't exist) when one of the
         pointers that the joined values originated from is invalid: [x↦v ⊔ Invalid(x) = x↦v *
         WeakInvalid(x)]. *)
      let v_join = AbstractValue.mk_fresh () in
      let hist_join = join_histories (Some hist_one_sided) None in
      (formula_join, (v_join, hist_join))
  | _, Some v_hist_lhs, _, Some v_hist_rhs
    when AbstractValue.equal (fst v_hist_lhs) (fst v_hist_rhs) ->
      (* [x↦v ⊔ x↦v = x↦v], use the fact that abstract values are unique even across disjuncts
         so their valuations can be shared, i.e. we can keep [v] as the representative in the
         joined state. *)
      (formula_join, v_hist_lhs)
  | _, Some v_hist_lhs, _, Some v_hist_rhs (* [v(v_hist_lhs) ≠ v(v_hist_rhs)] *) ->
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


(* TODO: keep track of previous [(v_lhs_opt, v_rhs_opt) -> v_join] mappings so we can reuse the same
   [v_join] for the same pair every time; need to keep track of mapping and visited set separately
   as [visited] is reset between pre and post but the mapping is constant across both *)
module Visited = Stdlib.Set.Make (struct
  type t = AbstractValue.t option * AbstractValue.t option [@@deriving compare]
end)

let rec join_heaps_from pre_or_post (formula_join, heap_join, visited, v_hist_join)
    (astate_lhs, vh_lhs_opt) (astate_rhs, vh_rhs_opt) =
  let visited_key = (Option.map ~f:fst vh_lhs_opt, Option.map ~f:fst vh_rhs_opt) in
  if Visited.mem visited_key visited then (formula_join, heap_join, visited)
  else
    let visited = Visited.add visited_key visited in
    let join_heap_values (formula_join, heap_join, visited) _access v_hist_lhs_opt v_hist_rhs_opt =
      let formula_join, v_hist_join =
        join_values_hists_opts (astate_lhs, v_hist_lhs_opt) (astate_rhs, v_hist_rhs_opt)
          formula_join
      in
      let join_state =
        join_heaps_from pre_or_post
          (formula_join, heap_join, visited, v_hist_join)
          (astate_lhs, v_hist_lhs_opt) (astate_rhs, v_hist_rhs_opt)
      in
      (join_state, Some v_hist_join)
    in
    let (formula_join, heap_join, visited), edges_join =
      Memory.fold_merge_edges pre_or_post (astate_lhs, vh_lhs_opt) (astate_rhs, vh_rhs_opt)
        ~init:(formula_join, heap_join, visited)
        ~f:join_heap_values
    in
    let heap_join = PulseBaseMemory.add (fst v_hist_join) edges_join heap_join in
    (formula_join, heap_join, visited)


let join_stacks astate_lhs astate_rhs =
  let join_stack_values pre_or_post (formula_join, heap_join, visited) _var vo_lhs_opt vo_rhs_opt =
    let formula_join, vo_join =
      join_value_origins (astate_lhs, vo_lhs_opt) (astate_rhs, vo_rhs_opt) formula_join
    in
    let v_hist_lhs_opt = Option.map ~f:ValueOrigin.addr_hist vo_lhs_opt in
    let v_hist_rhs_opt = Option.map ~f:ValueOrigin.addr_hist vo_rhs_opt in
    let join_state =
      join_heaps_from pre_or_post
        (formula_join, heap_join, visited, ValueOrigin.addr_hist vo_join)
        (astate_lhs, v_hist_lhs_opt) (astate_rhs, v_hist_rhs_opt)
    in
    (join_state, Some vo_join)
  in
  let (formula_join, heap_pre_join, _visited), stack_pre_join =
    Stack.fold_merge `Pre astate_lhs astate_rhs
      ~init:(astate_lhs.AbductiveDomain.path_condition, BaseMemory.empty, Visited.empty)
      ~f:(join_stack_values `Pre)
  in
  let (formula_join, heap_post_join, _visited), stack_post_join =
    Stack.fold_merge `Post astate_lhs astate_rhs
      ~init:(formula_join, BaseMemory.empty, Visited.empty)
      ~f:(join_stack_values `Post)
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


let join_summaries (summary_lhs : AbductiveDomain.Summary.t)
    (summary_rhs : AbductiveDomain.Summary.t) =
  join_abductive (summary_lhs :> AbductiveDomain.t) (summary_rhs :> AbductiveDomain.t)
  |> AbductiveDomain.Summary.unsafe_from_join
