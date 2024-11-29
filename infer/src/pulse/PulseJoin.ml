(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
module AbductiveDomain = PulseAbductiveDomain
module AddressAttributes = PulseBaseAddressAttributes
module BaseMemory = PulseBaseMemory
module Decompiler = PulseDecompiler
module Formula = PulseFormula
module Memory = AbductiveDomain.Memory
module PathContext = PulsePathContext
module Stack = AbductiveDomain.Stack

type join_key = AbstractValue.t option * AbstractValue.t option [@@deriving compare]

(* keep track of previous [(v_lhs_opt, v_rhs_opt) -> v_join] mappings so we can reuse the same
   [v_join] for the same pair every time *)
module Subst = Stdlib.Map.Make (struct
  type t = join_key [@@deriving compare]
end)

module RevSubst = AbstractValue.Map

module Visited = Stdlib.Set.Make (struct
  type t = join_key [@@deriving compare]
end)

type join_state =
  {subst: AbstractValue.t Subst.t; rev_subst: join_key RevSubst.t; visited: Visited.t}

let empty_join_state = {subst= Subst.empty; rev_subst= RevSubst.empty; visited= Visited.empty}

let join_histories (_hist : ValueHistory.t option) (_hist' : ValueHistory.t option) =
  (* TODO: something *) ValueHistory.epoch


let record_join_value v_join v1 v2 join_state =
  (* record the substitution and reverse sustitution [v_join <-> (Some v1, Some v2)]. A later path
     will add [v_join∈{v1, v2}] to the formula in the join abstract state. *)
  let subst = Subst.add (Some v1, Some v2) v_join join_state.subst in
  let rev_subst = RevSubst.add v_join (Some v1, Some v2) join_state.rev_subst in
  {join_state with subst; rev_subst}


(** case where both sides have a value available at the same memory location *)
let join_values_hists join_state (v_lhs, hist_lhs) (v_rhs, hist_rhs) =
  let join_state, v_join =
    if AbstractValue.equal v_lhs v_rhs then
      (* [x↦v ⊔ x↦v = x↦v], use the fact that abstract values are unique even across disjuncts
         so their valuations can be shared, i.e. we can keep [v] as the representative in the
         joined state. *)
      (join_state, v_lhs)
    else
      (* [x↦v ⊔ x↦v' = x↦v''], [v''] fresh *)
      let v_join = AbstractValue.mk_fresh () in
      let join_state = record_join_value v_join v_lhs v_rhs join_state in
      (join_state, v_join)
  in
  let hist_join = join_histories (Some hist_lhs) (Some hist_rhs) in
  (join_state, (v_join, hist_join))


(** uncached version *)
let join_values_hists_opts_aux formula_join (astate_lhs, v_hist_lhs_opt) (astate_rhs, v_hist_rhs_opt)
    =
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
      join_values_hists formula_join v_hist_lhs v_hist_rhs


(** like [join_values_hists_opts_aux] but uses the [Subst.t] cache provided *)
let join_values_hists_opts join_state (astate_lhs, v_hist_lhs_opt) (astate_rhs, v_hist_rhs_opt) =
  let subst_key = (Option.map ~f:fst v_hist_lhs_opt, Option.map ~f:fst v_hist_rhs_opt) in
  match Subst.find_opt subst_key join_state.subst with
  | Some v_join ->
      let hist_join =
        join_histories (Option.map ~f:snd v_hist_lhs_opt) (Option.map ~f:snd v_hist_rhs_opt)
      in
      (join_state, (v_join, hist_join))
  | None ->
      let join_state, v_hist_join =
        join_values_hists_opts_aux join_state (astate_lhs, v_hist_lhs_opt)
          (astate_rhs, v_hist_rhs_opt)
      in
      let join_state =
        {join_state with subst= Subst.add subst_key (fst v_hist_join) join_state.subst}
      in
      (join_state, v_hist_join)


let join_value_origins join_state (astate_lhs, vo_lhs_opt) (astate_rhs, vo_rhs_opt) =
  (* TODO: actually join value origins, not just the underlying values and histories, i.e. do better
       than [Unknown] here *)
  let join_state, v_hist_join =
    join_values_hists_opts join_state
      (astate_lhs, Option.map ~f:ValueOrigin.addr_hist vo_lhs_opt)
      (astate_rhs, Option.map ~f:ValueOrigin.addr_hist vo_rhs_opt)
  in
  (join_state, ValueOrigin.unknown v_hist_join)


let rec join_heaps_from pre_or_post (join_state, heap_join, v_hist_join) (astate_lhs, vh_lhs_opt)
    (astate_rhs, vh_rhs_opt) =
  let visited_key = (Option.map ~f:fst vh_lhs_opt, Option.map ~f:fst vh_rhs_opt) in
  if Visited.mem visited_key join_state.visited then (join_state, heap_join)
  else
    let join_state = {join_state with visited= Visited.add visited_key join_state.visited} in
    let join_heap_values (join_state, heap_join) _access v_hist_lhs_opt v_hist_rhs_opt =
      let join_state, v_hist_join =
        join_values_hists_opts join_state (astate_lhs, v_hist_lhs_opt) (astate_rhs, v_hist_rhs_opt)
      in
      let join_state =
        join_heaps_from pre_or_post
          (join_state, heap_join, v_hist_join)
          (astate_lhs, v_hist_lhs_opt) (astate_rhs, v_hist_rhs_opt)
      in
      (join_state, Some v_hist_join)
    in
    let (join_state, heap_join), edges_join =
      Memory.fold_merge_edges pre_or_post (astate_lhs, vh_lhs_opt) (astate_rhs, vh_rhs_opt)
        ~init:(join_state, heap_join) ~f:join_heap_values
    in
    let heap_join = PulseBaseMemory.add (fst v_hist_join) edges_join heap_join in
    (join_state, heap_join)


let join_stacks astate_lhs astate_rhs =
  let join_stack_values pre_or_post (join_state, heap_join) _var vo_lhs_opt vo_rhs_opt =
    let join_state, vo_join =
      join_value_origins join_state (astate_lhs, vo_lhs_opt) (astate_rhs, vo_rhs_opt)
    in
    let v_hist_lhs_opt = Option.map ~f:ValueOrigin.addr_hist vo_lhs_opt in
    let v_hist_rhs_opt = Option.map ~f:ValueOrigin.addr_hist vo_rhs_opt in
    let join_state =
      join_heaps_from pre_or_post
        (join_state, heap_join, ValueOrigin.addr_hist vo_join)
        (astate_lhs, v_hist_lhs_opt) (astate_rhs, v_hist_rhs_opt)
    in
    (join_state, Some vo_join)
  in
  let (join_state, heap_pre_join), stack_pre_join =
    Stack.fold_merge `Pre astate_lhs astate_rhs
      ~init:(empty_join_state, BaseMemory.empty)
      ~f:(join_stack_values `Pre)
  in
  let join_state = {join_state with visited= Visited.empty} in
  let (join_state, heap_post_join), stack_post_join =
    Stack.fold_merge `Post astate_lhs astate_rhs ~init:(join_state, BaseMemory.empty)
      ~f:(join_stack_values `Post)
  in
  (join_state, (stack_pre_join, heap_pre_join), (stack_post_join, heap_post_join))


let join_attributes _join_state _astate_lhs _astate_rhs =
  (* TODO *)
  (AddressAttributes.empty, AddressAttributes.empty)


let join_formulas _join_state _astate_lhs _astate_rhs =
  (* TODO *)
  Formula.ttrue


let join_abductive astate_lhs astate_rhs =
  let join_state, (stack_pre_join, heap_pre_join), (stack_post_join, heap_post_join) =
    join_stacks astate_lhs astate_rhs
  in
  let attrs_pre_join, attrs_post_join = join_attributes join_state astate_lhs astate_rhs in
  let formula = join_formulas join_state astate_lhs astate_rhs in
  AbductiveDomain.mk_join_state
    ~pre:(stack_pre_join, heap_pre_join, attrs_pre_join)
    ~post:(stack_post_join, heap_post_join, attrs_post_join)
    formula (* TODO: everything past this *) Decompiler.empty
    ~need_dynamic_type_specialization:AbstractValue.Set.empty (PulseTopl.start ())
    TransitiveInfo.bottom PulseMutualRecursion.Set.empty SkippedCalls.empty


let join (astate_lhs, path_lhs) (astate_rhs, path_rhs) =
  (join_abductive astate_lhs astate_rhs, PathContext.join path_lhs path_rhs)


let join_summaries (summary_lhs : AbductiveDomain.Summary.t)
    (summary_rhs : AbductiveDomain.Summary.t) =
  join_abductive (summary_lhs :> AbductiveDomain.t) (summary_rhs :> AbductiveDomain.t)
  |> AbductiveDomain.Summary.unsafe_from_join
