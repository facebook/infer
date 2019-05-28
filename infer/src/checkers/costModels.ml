(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module BasicCost = CostDomain.BasicCost
open BufferOverrunUtils.ModelEnv

type model = model_env -> ret:Ident.t * Typ.t -> BufferOverrunDomain.Mem.t -> BasicCost.t

module Collections = struct
  let eval_collection_length coll_exp loc inferbo_mem ~of_function =
    let upper_bound =
      let itv =
        BufferOverrunModels.Collection.eval_collection_length coll_exp inferbo_mem
        |> BufferOverrunDomain.Val.get_itv
      in
      match itv with Bottom -> Bounds.Bound.pinf | NonBottom itv_pure -> Itv.ItvPure.ub itv_pure
    in
    Bounds.NonNegativeBound.of_modeled_function of_function loc upper_bound


  let n_log_n b =
    let n = BasicCost.of_non_negative_bound b in
    let log_n = BasicCost.of_non_negative_bound ~degree_kind:Polynomials.DegreeKind.Log b in
    BasicCost.mult n log_n


  let sort coll_exp {location} ~ret:_ inferbo_mem =
    let length = eval_collection_length coll_exp location ~of_function:"List.length" inferbo_mem in
    n_log_n length


  let of_length_bound ~degree_kind coll_exp ~of_function {location} ~ret:_ inferbo_mem =
    eval_collection_length coll_exp location inferbo_mem ~of_function
    |> BasicCost.of_non_negative_bound ~degree_kind


  let linear = of_length_bound ~degree_kind:Polynomials.DegreeKind.Linear

  let logarithmic = of_length_bound ~degree_kind:Polynomials.DegreeKind.Log
end

let provider_get {pname; location} ~ret:(_, ret_typ) _ : BasicCost.t =
  let callsite = CallSite.make pname location in
  let path = Symb.SymbolPath.of_callsite ~ret_typ callsite in
  let v =
    let itv = Itv.of_modeled_path path in
    match itv with Bottom -> Bounds.Bound.pinf | NonBottom itv_pure -> Itv.ItvPure.ub itv_pure
  in
  Bounds.NonNegativeBound.of_modeled_function "Provider.get" location v
  |> BasicCost.of_non_negative_bound


module String = struct
  let substring_aux ~begin_idx ~end_v {integer_type_widths; location} inferbo_mem =
    let upper_bound =
      let begin_v = BufferOverrunSemantics.eval integer_type_widths begin_idx inferbo_mem in
      let substring_itv =
        Itv.minus (BufferOverrunDomain.Val.get_itv end_v) (BufferOverrunDomain.Val.get_itv begin_v)
      in
      match substring_itv with
      | Bottom ->
          Bounds.Bound.pinf
      | NonBottom itv_pure ->
          Itv.ItvPure.ub itv_pure
    in
    Bounds.NonNegativeBound.of_modeled_function "String.substring" location upper_bound
    |> BasicCost.of_non_negative_bound


  let substring exp begin_idx model_env ~ret:_ inferbo_mem =
    substring_aux ~begin_idx
      ~end_v:(BufferOverrunModels.eval_string_len exp inferbo_mem)
      model_env inferbo_mem


  let substring_no_end begin_idx end_idx ({integer_type_widths} as model_env) ~ret:_ inferbo_mem =
    substring_aux ~begin_idx
      ~end_v:(BufferOverrunSemantics.eval integer_type_widths end_idx inferbo_mem)
      model_env inferbo_mem
end

module Call = struct
  let dispatch : (Tenv.t, model) ProcnameDispatcher.Call.dispatcher =
    let open ProcnameDispatcher.Call in
    make_dispatcher
      [ +PatternMatch.implements_collections &:: "sort" $ capt_exp $+...$--> Collections.sort
      ; +PatternMatch.implements_list &:: "contains" <>$ capt_exp
        $+...$--> Collections.linear ~of_function:"List.contains"
      ; +PatternMatch.implements_collections
        &:: "binarySearch" <>$ capt_exp
        $+...$--> Collections.logarithmic ~of_function:"Collections.binarySearch"
      ; +PatternMatch.implements_collections
        &:: "copy" <>$ capt_exp
        $+...$--> Collections.linear ~of_function:"Collections.copy"
      ; +PatternMatch.implements_collections
        &:: "fill" <>$ capt_exp
        $+...$--> Collections.linear ~of_function:"Collections.fill"
      ; +PatternMatch.implements_collections
        &:: "reverse" <>$ capt_exp
        $+...$--> Collections.linear ~of_function:"Collections.reverse"
      ; +PatternMatch.implements_collections
        &:: "shuffle" <>$ capt_exp
        $+...$--> Collections.linear ~of_function:"Collections.shuffle"
      ; +PatternMatch.implements_lang "String"
        &:: "substring" <>$ capt_exp $+ capt_exp $--> String.substring
      ; +PatternMatch.implements_lang "String"
        &:: "substring"
        $ any_arg_of_typ (+PatternMatch.implements_lang "String")
        $+ capt_exp $+ capt_exp $--> String.substring_no_end
      ; +PatternMatch.implements_inject "Provider" &:: "get" <>--> provider_get ]
end
