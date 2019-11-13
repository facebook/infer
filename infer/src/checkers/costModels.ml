(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module BasicCost = CostDomain.BasicCost
open BufferOverrunUtils.ModelEnv

let unit_cost_model _model_env ~ret:_ _inferbo_mem = BasicCost.one

let cost_of_exp exp ~degree_kind ~of_function {integer_type_widths; location} ~ret:_ inferbo_mem =
  let itv =
    BufferOverrunSemantics.eval integer_type_widths exp inferbo_mem
    |> BufferOverrunDomain.Val.get_itv
  in
  CostUtils.of_itv ~itv ~degree_kind ~of_function location


let linear = cost_of_exp ~degree_kind:Polynomials.DegreeKind.Linear

let log = cost_of_exp ~degree_kind:Polynomials.DegreeKind.Log

let modeled ~of_function {pname; location} ~ret:(_, ret_typ) _ : BasicCost.t =
  let callsite = CallSite.make pname location in
  let path = Symb.SymbolPath.of_callsite ~ret_typ callsite in
  let itv = Itv.of_modeled_path path in
  CostUtils.of_itv ~itv ~degree_kind:Polynomials.DegreeKind.Linear ~of_function location


module BoundsOf (Container : CostUtils.S) = struct
  let of_length exp {location} ~ret:_ mem ~of_function ~degree_kind =
    let itv = Container.length exp mem |> BufferOverrunDomain.Val.get_itv in
    CostUtils.of_itv ~itv ~degree_kind ~of_function location


  let linear_length = of_length ~degree_kind:Polynomials.DegreeKind.Linear

  let logarithmic_length = of_length ~degree_kind:Polynomials.DegreeKind.Log

  let n_log_n_length exp env ~ret mem ~of_function =
    let log_n = logarithmic_length exp ~of_function env mem ~ret in
    let n = linear_length exp ~of_function env mem ~ret in
    BasicCost.mult n log_n
end

module JavaString = struct
  let substring_aux ~begin_idx ~end_v {integer_type_widths; location} inferbo_mem =
    let begin_v = BufferOverrunSemantics.eval integer_type_widths begin_idx inferbo_mem in
    let itv =
      Itv.minus (BufferOverrunDomain.Val.get_itv end_v) (BufferOverrunDomain.Val.get_itv begin_v)
    in
    CostUtils.of_itv ~itv ~degree_kind:Polynomials.DegreeKind.Linear ~of_function:"String.substring"
      location


  let substring exp begin_idx model_env ~ret:_ inferbo_mem =
    substring_aux ~begin_idx
      ~end_v:(BufferOverrunModels.JavaString.get_length model_env exp inferbo_mem)
      model_env inferbo_mem


  let substring_no_end begin_idx end_idx ({integer_type_widths} as model_env) ~ret:_ inferbo_mem =
    substring_aux ~begin_idx
      ~end_v:(BufferOverrunSemantics.eval integer_type_widths end_idx inferbo_mem)
      model_env inferbo_mem


  (** O(|m|) where m is the given string and |.| is the length function *)
  let indexOf_char exp ({location} as model_env) ~ret:_ inferbo_mem =
    let itv = CostUtils.string_len_range_itv model_env exp ~from:None inferbo_mem in
    CostUtils.of_itv ~itv ~degree_kind:Polynomials.DegreeKind.Linear ~of_function:"String.indexOf"
      location


  (** O(|m|-|n|) where m is the given string and n is the index to start searching from *)
  let indexOf_char_starting_from exp start_exp ({integer_type_widths; location} as model_env) ~ret:_
      inferbo_mem =
    let itv =
      CostUtils.string_len_range_itv model_env exp
        ~from:(Some (start_exp, integer_type_widths))
        inferbo_mem
    in
    CostUtils.of_itv ~itv ~degree_kind:Polynomials.DegreeKind.Linear ~of_function:"String.indexOf"
      location


  (** O(|m|.|n|) where m and n are the given strings *)
  let indexOf_str exp index_exp ({location} as model_env) ~ret:_ inferbo_mem =
    let itv =
      BufferOverrunModels.JavaString.get_length model_env exp inferbo_mem
      |> BufferOverrunDomain.Val.get_itv
    in
    let index_itv =
      BufferOverrunModels.JavaString.get_length model_env index_exp inferbo_mem
      |> BufferOverrunDomain.Val.get_itv
    in
    let n =
      CostUtils.of_itv ~itv ~degree_kind:Polynomials.DegreeKind.Linear ~of_function:"String.indexOf"
        location
    in
    let m =
      CostUtils.of_itv ~itv:index_itv ~degree_kind:Polynomials.DegreeKind.Linear
        ~of_function:"String.indexOf" location
    in
    BasicCost.mult n m
end

module BoundsOfCollection = BoundsOf (CostUtils.Collection)
module BoundsOfArray = BoundsOf (CostUtils.Array)

module ImmutableSet = struct
  let construct = linear ~of_function:"ImmutableSet.construct"

  let choose_table_size = log ~of_function:"ImmutableSet.chooseTableSize"
end

module Call = struct
  let dispatch : (Tenv.t, CostUtils.model) ProcnameDispatcher.Call.dispatcher =
    let open ProcnameDispatcher.Call in
    let int_typ = Typ.mk (Typ.Tint Typ.IInt) in
    let dispatcher =
      make_dispatcher
        [ +PatternMatch.implements_collections
          &:: "sort" $ capt_exp
          $+...$--> BoundsOfCollection.n_log_n_length ~of_function:"Collections.sort"
        ; +PatternMatch.implements_list &:: "sort" $ capt_exp
          $+...$--> BoundsOfCollection.n_log_n_length ~of_function:"List.sort"
        ; +PatternMatch.implements_arrays &:: "sort" $ capt_exp
          $+...$--> BoundsOfArray.n_log_n_length ~of_function:"Arrays.sort"
        ; +PatternMatch.implements_list &:: "contains" <>$ capt_exp
          $+...$--> BoundsOfCollection.linear_length ~of_function:"List.contains"
        ; +PatternMatch.implements_collections
          &:: "binarySearch" <>$ capt_exp
          $+...$--> BoundsOfCollection.logarithmic_length ~of_function:"Collections.binarySearch"
        ; +PatternMatch.implements_arrays &:: "binarySearch" <>$ capt_exp
          $+...$--> BoundsOfArray.logarithmic_length ~of_function:"Arrays.binarySearch"
        ; +PatternMatch.implements_arrays &:: "copyOf" <>$ any_arg $+ capt_exp
          $+...$--> linear ~of_function:"Arrays.copyOf"
        ; +PatternMatch.implements_collections
          &:: "copy" <>$ capt_exp
          $+...$--> BoundsOfCollection.linear_length ~of_function:"Collections.copy"
        ; +PatternMatch.implements_collections
          &:: "fill" <>$ capt_exp
          $+...$--> BoundsOfCollection.linear_length ~of_function:"Collections.fill"
        ; +PatternMatch.implements_arrays &:: "fill" <>$ capt_exp
          $+...$--> BoundsOfArray.linear_length ~of_function:"Arrays.fill"
        ; +PatternMatch.implements_collections
          &:: "reverse" <>$ capt_exp
          $+...$--> BoundsOfCollection.linear_length ~of_function:"Collections.reverse"
        ; +PatternMatch.implements_collections
          &:: "max" <>$ capt_exp
          $+...$--> BoundsOfCollection.linear_length ~of_function:"Collections.max"
        ; +PatternMatch.implements_collections
          &:: "min" <>$ capt_exp
          $+...$--> BoundsOfCollection.linear_length ~of_function:"Collections.min"
        ; +PatternMatch.implements_collections
          &:: "shuffle" <>$ capt_exp
          $+...$--> BoundsOfCollection.linear_length ~of_function:"Collections.shuffle"
        ; +PatternMatch.implements_lang "String"
          &:: "substring" <>$ capt_exp $+ capt_exp $--> JavaString.substring
        ; +PatternMatch.implements_lang "String"
          &:: "indexOf" <>$ capt_exp
          $+ capt_exp_of_typ (+PatternMatch.implements_lang "String")
          $--> JavaString.indexOf_str
        ; +PatternMatch.implements_lang "String"
          &:: "indexOf" <>$ capt_exp $+ any_arg_of_prim_typ int_typ $+ capt_exp
          $--> JavaString.indexOf_char_starting_from
        ; +PatternMatch.implements_lang "String"
          &:: "indexOf" <>$ capt_exp $+ any_arg_of_prim_typ int_typ $--> JavaString.indexOf_char
        ; +PatternMatch.implements_lang "String"
          &:: "substring"
          $ any_arg_of_typ (+PatternMatch.implements_lang "String")
          $+ capt_exp $+ capt_exp $--> JavaString.substring_no_end
        ; +PatternMatch.implements_inject "Provider"
          &:: "get"
          <>--> modeled ~of_function:"Provider.get"
        ; +PatternMatch.implements_xmob_utils "IntHashMap" &:: "<init>" <>--> unit_cost_model
        ; +PatternMatch.implements_xmob_utils "IntHashMap" &:: "getElement" <>--> unit_cost_model
        ; +PatternMatch.implements_xmob_utils "IntHashMap" &:: "put" <>--> unit_cost_model
        ; +PatternMatch.implements_xmob_utils "IntHashMap" &:: "remove" <>--> unit_cost_model
        ; +PatternMatch.implements_google "common.collect.ImmutableSet"
          &:: "chooseTableSize" <>$ capt_exp $+...$--> ImmutableSet.choose_table_size
        ; +PatternMatch.implements_google "common.collect.ImmutableSet"
          &:: "construct" <>$ capt_exp_of_prim_typ int_typ $+...$--> ImmutableSet.construct
        ; +PatternMatch.implements_google "common.collect.ImmutableSet"
          &:: "construct" <>$ any_arg $+ capt_exp_of_prim_typ int_typ
          $+...$--> ImmutableSet.construct ]
    in
    merge_dispatchers dispatcher FbCostModels.Call.dispatch
end
