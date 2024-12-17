(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
module L = Logging
module AbductiveDomain = PulseAbductiveDomain
module AddressAttributes = PulseBaseAddressAttributes
module Attributes = Attribute.Attributes
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
  { subst: AbstractValue.t Subst.t
  ; rev_subst: join_key RevSubst.t
        (** Record the provenance of each abstract value in the join state in terms of the two
            joined states. This is used, e.g., when joining attributes. Other, more efficient
            possibilities (that don't need to record this intermediate data) include handling
            attributes as we go. *)
  ; visited: Visited.t }

let empty_join_state = {subst= Subst.empty; rev_subst= RevSubst.empty; visited= Visited.empty}

let join_histories hist1 hist2 =
  if ValueHistory.equal hist1 hist2 then hist1 else (* TODO: something *) ValueHistory.epoch


let join_histories_opts hist1_opt hist2_opt =
  match (hist1_opt, hist2_opt) with
  | Some hist1, Some hist2 ->
      join_histories hist1 hist2
  | Some _, None | None, Some _ | None, None ->
      (* TODO: something *) ValueHistory.epoch


let join_traces trace1 trace2 =
  if Trace.equal trace1 trace2 then trace1 else (* TODO: something *) trace1


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
      let rev_subst = RevSubst.add v_lhs (Some v_lhs, Some v_rhs) join_state.rev_subst in
      ({join_state with rev_subst}, v_lhs)
    else
      (* [x↦v ⊔ x↦v' = x↦v''], [v''] fresh *)
      let v_join = AbstractValue.mk_fresh () in
      let join_state = record_join_value v_join v_lhs v_rhs join_state in
      (join_state, v_join)
  in
  let hist_join = join_histories_opts (Some hist_lhs) (Some hist_rhs) in
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
      let hist_join = join_histories_opts (Some hist_one_sided) None in
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
        join_histories_opts (Option.map ~f:snd v_hist_lhs_opt) (Option.map ~f:snd v_hist_rhs_opt)
      in
      (join_state, (v_join, hist_join))
  | None ->
      let join_state, v_hist_join =
        join_values_hists_opts_aux join_state (astate_lhs, v_hist_lhs_opt)
          (astate_rhs, v_hist_rhs_opt)
      in
      let subst = Subst.add subst_key (fst v_hist_join) join_state.subst in
      let rev_subst = RevSubst.add (fst v_hist_join) subst_key join_state.rev_subst in
      let join_state = {join_state with subst; rev_subst} in
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


let join_one_sided_attribute (attr : Attribute.t) =
  let weaken attr =
    (* TODO: add weaker versions of some of the attributes so that we can distinguish when we have
       lost precision on attributes or not (eg the pointer is null in all branches vs null in some
       branches only. For now let's just pretend we always have the strong version. *)
    attr
  in
  match attr with
  | AlwaysReachable
  (* maybe we want to be more forgiving, at least we get a closure to call even if it comes from
     just one branch? *)
  | Closure _
  | CSharpResourceReleased
  | DictContainConstKeys
  | HackConstinitCalled
  | Initialized
  | JavaResourceReleased
  (* could move these two to the "keep if one sided" case if this creates too many FPs *)
  | ReturnedFromUnknown _
  | UnknownEffect _
  (* could be more forgiving about one-sided [StaticType] too *)
  | StaticType _
  | StdVectorReserve
  (* harsh, but biased towards reporting more taint errors *)
  | TaintSanitized _
  | UnreachableAt _ ->
      None
  | Allocated _
  | AwaitedAwaitable
  | CopiedInto _
  | CopiedReturn _
  | InReportedRetainCycle
  | LastLookup _
  | SourceOriginOfCopy _
  | StdMoved
  | UsedAsBranchCond _
  | WrittenTo _ ->
      Some attr
  | AddressOfCppTemporary _
  | AddressOfStackVariable _
  | ConfigUsage _
  | DictReadConstKeys _
  | EndOfCollection
  | HackBuilder _
  | Invalid _
  | MustBeInitialized _
  | MustBeValid _
  | MustNotBeTainted _
  | PropagateTaintFrom _
  | Tainted _
  | Uninitialized _ ->
      Some (weaken attr)


let join_two_sided_attribute join_state (attr1 : Attribute.t) (attr2 : Attribute.t) :
    Attribute.t option =
  let mk_from_joined_values v1 v2 ~f =
    if AbstractValue.equal v1 v2 then Some attr1
    else
      match Subst.find_opt (Some v1, Some v2) join_state.subst with
      | Some v_join ->
          Some (f v_join)
      | None ->
          None
  in
  match (attr1, attr2) with
  | AlwaysReachable, AlwaysReachable
  | CSharpResourceReleased, CSharpResourceReleased
  | DictContainConstKeys, DictContainConstKeys
  | EndOfCollection, EndOfCollection
  | HackConstinitCalled, HackConstinitCalled
  | InReportedRetainCycle, InReportedRetainCycle
  | Initialized, Initialized
  | JavaResourceReleased, JavaResourceReleased
  | StdMoved, StdMoved
  | StdVectorReserve, StdVectorReserve ->
      Some attr1
  | AddressOfCppTemporary (var1, hist1), AddressOfCppTemporary (var2, hist2) ->
      if Var.equal var1 var2 then
        let hist = join_histories hist1 hist2 in
        Some (AddressOfCppTemporary (var1, hist))
      else None
  | AddressOfStackVariable (var1, _, _), AddressOfStackVariable (var2, _, _) ->
      (* TODO: join histories, etc. *)
      if Var.equal var1 var2 then Some attr1 else None
  | Allocated (allocator1, _), Allocated (allocator2, _) ->
      (* TODO: join traces *)
      if Attribute.equal_allocator allocator1 allocator2 then Some attr1 else None
  | Closure proc_name1, Closure proc_name2 ->
      if Procname.equal proc_name1 proc_name2 then Some attr1 else None
  | ConfigUsage cu1, ConfigUsage cu2 ->
      if Attribute.ConfigUsage.equal cu1 cu2 then Some attr1 else None
  | CopiedInto ci1, CopiedInto ci2 ->
      if Attribute.CopiedInto.equal ci1 ci2 then Some attr1 else None
  | ( CopiedReturn {source= source1; is_const_ref= is_const_ref1; from= from1; copied_location}
    , CopiedReturn {source= source2; is_const_ref= is_const_ref2; from= from2} ) ->
      if Bool.equal is_const_ref1 is_const_ref2 && Attribute.CopyOrigin.equal from1 from2 then
        mk_from_joined_values source1 source2 ~f:(fun source ->
            CopiedReturn {source; is_const_ref= is_const_ref1; from= from1; copied_location} )
      else None
  | DictReadConstKeys keys1, DictReadConstKeys keys2 ->
      (* TODO: chose [inter] here but to be more over-approx we should do the union! can't help
         shooting for under-approximation... *)
      let keys = Attribute.ConstKeys.inter keys1 keys2 in
      if Attribute.ConstKeys.is_empty keys then None else Some (DictReadConstKeys keys)
  | HackBuilder builder1, HackBuilder builder2 ->
      if Attribute.Builder.equal builder1 builder2 then Some attr1 else None
  | Invalid (invalidation1, trace1), Invalid (invalidation2, trace2) ->
      if Invalidation.equal invalidation1 invalidation2 then
        Some (Invalid (invalidation1, join_traces trace1 trace2))
      else None
  | LastLookup v1, LastLookup v2 ->
      mk_from_joined_values v1 v2 ~f:(fun v -> LastLookup v)
  | MustBeInitialized _, MustBeInitialized _ ->
      (* doesn't really matter which branch is doing the reading for now *) Some attr1
  | MustBeValid _, MustBeValid _ ->
      (* TODO: join must_be_valid_reason *) Some attr1
  | MustNotBeTainted sinks1, MustNotBeTainted sinks2 ->
      let sinks = Attribute.TaintSinkMap.union (fun _ sink1 _sink2 -> Some sink1) sinks1 sinks2 in
      Some (MustNotBeTainted sinks)
  | PropagateTaintFrom (reason1, taint_in1), PropagateTaintFrom (_reason2, taint_in2) ->
      (* TODO: merge reasons? or allow the attribute to have several reasons and their associated
         [taint_in]s *)
      let taint_in1 = List.sort ~compare:Attribute.compare_taint_in taint_in1 in
      let taint_in2 = List.sort ~compare:Attribute.compare_taint_in taint_in2 in
      let taint_in = List.merge ~compare:Attribute.compare_taint_in taint_in1 taint_in2 in
      Some (PropagateTaintFrom (reason1, taint_in))
  | ReturnedFromUnknown vs1, ReturnedFromUnknown vs2 ->
      let vs1 = List.sort ~compare:AbstractValue.compare vs1 in
      let vs2 = List.sort ~compare:AbstractValue.compare vs2 in
      let vs = List.merge ~compare:AbstractValue.compare vs1 vs2 in
      Some (ReturnedFromUnknown vs)
  | ( SourceOriginOfCopy {source= source1; is_const_ref= is_const_ref1}
    , SourceOriginOfCopy {source= source2; is_const_ref= is_const_ref2} ) ->
      if Bool.equal is_const_ref1 is_const_ref2 then
        mk_from_joined_values source1 source2 ~f:(fun source ->
            SourceOriginOfCopy {source; is_const_ref= is_const_ref1} )
      else None
  | StaticType t1, StaticType t2 ->
      if Typ.Name.equal t1 t2 then Some attr1 else None
  | Tainted sources1, Tainted sources2 ->
      let sources = Attribute.TaintedSet.union sources1 sources2 in
      Some (Tainted sources)
  | TaintSanitized sanitizers1, TaintSanitized sanitizers2 ->
      let sanitizers = Attribute.TaintSanitizedSet.union sanitizers1 sanitizers2 in
      Some (TaintSanitized sanitizers)
  | Uninitialized t1, Uninitialized t2 ->
      if Attribute.UninitializedTyp.equal t1 t2 then Some attr1 else None
  | UnknownEffect _, UnknownEffect _ ->
      (* arbitrary, doesn't matter much which side *) Some attr1
  | UnreachableAt loc1, UnreachableAt loc2 ->
      let loc = if Location.compare loc1 loc2 > 0 then loc1 else loc2 in
      Some (UnreachableAt loc)
  | UsedAsBranchCond _, UsedAsBranchCond _ ->
      (* arbitrary, doesn't matter much which side *) Some attr1
  | WrittenTo (ts1, _), WrittenTo (ts2, _) ->
      (* pick the most recent event *)
      if Timestamp.compare ts1 ts2 >= 0 then Some attr1 else Some attr2
  | _, _ ->
      (* impossible unless this is called with attributes of different ranks *)
      L.die InternalError "joining attributes with different kinds"


let join_attribute join_state (attr1_opt : Attribute.t option) (attr2_opt : Attribute.t option) :
    Attribute.t option =
  match (attr1_opt, attr2_opt) with
  | None, None ->
      assert false
  | Some attr, None | None, Some attr ->
      join_one_sided_attribute attr
  | Some attr1, Some attr2 ->
      if Attribute.equal attr1 attr2 then attr1_opt
      else join_two_sided_attribute join_state attr1 attr2


let join_address_attributes join_state attrs_lhs attrs_rhs =
  Attributes.merge attrs_lhs attrs_rhs ~f:(join_attribute join_state)


let join_base_attributes pre_or_post join_state astate_lhs astate_rhs =
  RevSubst.fold
    (fun v_join (v_lhs_opt, v_rhs_opt) attrs_join ->
      let open IOption.Let_syntax in
      let attrs_lhs =
        (let* v_lhs = v_lhs_opt in
         AbductiveDomain.AddressAttributes.find_opt pre_or_post v_lhs astate_lhs )
        |> Option.value ~default:Attributes.empty
      in
      let attrs_rhs =
        (let* v_rhs = v_rhs_opt in
         AbductiveDomain.AddressAttributes.find_opt pre_or_post v_rhs astate_rhs )
        |> Option.value ~default:Attributes.empty
      in
      let attrs = join_address_attributes join_state attrs_lhs attrs_rhs in
      AddressAttributes.add v_join attrs attrs_join )
    join_state.rev_subst AddressAttributes.empty


let join_attributes join_state astate_lhs astate_rhs =
  let pre = join_base_attributes `Pre join_state astate_lhs astate_rhs in
  let post = join_base_attributes `Post join_state astate_lhs astate_rhs in
  (pre, post)


let join_formulas _join_state astate_lhs astate_rhs =
  let formula =
    Formula.join astate_lhs.AbductiveDomain.path_condition astate_rhs.AbductiveDomain.path_condition
  in
  (* TODO: add info from [join_state.rev_subst] *)
  formula


let join_abductive astate_lhs astate_rhs =
  let join_state, (stack_pre_join, heap_pre_join), (stack_post_join, heap_post_join) =
    join_stacks astate_lhs astate_rhs
  in
  let attrs_pre_join, attrs_post_join = join_attributes join_state astate_lhs astate_rhs in
  let formula = join_formulas join_state astate_lhs astate_rhs in
  let transitive_info =
    TransitiveInfo.join astate_lhs.AbductiveDomain.transitive_info
      astate_rhs.AbductiveDomain.transitive_info
  in
  AbductiveDomain.mk_join_state
    ~pre:(stack_pre_join, heap_pre_join, attrs_pre_join)
    ~post:(stack_post_join, heap_post_join, attrs_post_join)
    formula (* TODO: almost everything past this *) Decompiler.empty
    ~need_dynamic_type_specialization:AbstractValue.Set.empty (PulseTopl.start ()) transitive_info
    PulseMutualRecursion.Set.empty SkippedCalls.empty


let join (astate_lhs, path_lhs) (astate_rhs, path_rhs) =
  (join_abductive astate_lhs astate_rhs, PathContext.join path_lhs path_rhs)


let join_summaries (summary_lhs : AbductiveDomain.Summary.t)
    (summary_rhs : AbductiveDomain.Summary.t) =
  join_abductive (summary_lhs :> AbductiveDomain.t) (summary_rhs :> AbductiveDomain.t)
  |> AbductiveDomain.Summary.unsafe_from_join
