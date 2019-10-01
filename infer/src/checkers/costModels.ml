(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module BasicCost = CostDomain.BasicCost
open BufferOverrunUtils.ModelEnv

type model = model_env -> ret:Ident.t * Typ.t -> BufferOverrunDomain.Mem.t -> BasicCost.t

module type S = sig
  val length : Exp.t -> BufferOverrunDomain.Mem.t -> BufferOverrunDomain.Val.t
end

module Array : S = struct
  let length arr_exp inferbo_mem =
    BufferOverrunSemantics.eval_array_locs_length
      (BufferOverrunSemantics.eval_locs arr_exp inferbo_mem)
      inferbo_mem
end

module Collection : S = struct
  let length coll_exp inferbo_mem =
    BufferOverrunModels.Collection.eval_collection_length coll_exp inferbo_mem
end

let of_itv ~(itv : Itv.t) ~degree_kind ~of_function loc =
  let upper_bound =
    match itv with Bottom -> Bounds.Bound.pinf | NonBottom itv_pure -> Itv.ItvPure.ub itv_pure
  in
  Bounds.NonNegativeBound.of_modeled_function of_function loc upper_bound
  |> BasicCost.of_non_negative_bound ~degree_kind


let linear exp ~of_function {integer_type_widths; location} ~ret:_ inferbo_mem =
  let itv =
    BufferOverrunSemantics.eval integer_type_widths exp inferbo_mem
    |> BufferOverrunDomain.Val.get_itv
  in
  of_itv ~itv ~degree_kind:Polynomials.DegreeKind.Linear ~of_function location


let modeled ~of_function {pname; location} ~ret:(_, ret_typ) _ : BasicCost.t =
  let callsite = CallSite.make pname location in
  let path = Symb.SymbolPath.of_callsite ~ret_typ callsite in
  let itv = Itv.of_modeled_path path in
  of_itv ~itv ~degree_kind:Polynomials.DegreeKind.Linear ~of_function location


(** Given a string of length n and an optional starting index i (0 by
   default), return itv [0, n_u-i_l] *)
let string_len_range_itv model_env exp ~from mem =
  let itv =
    BufferOverrunModels.JavaString.get_length model_env exp mem |> BufferOverrunDomain.Val.get_itv
  in
  Option.value_map from ~default:itv ~f:(fun (start_exp, integer_type_widths) ->
      let start_itv =
        BufferOverrunSemantics.eval integer_type_widths start_exp mem
        |> BufferOverrunDomain.Val.get_itv
      in
      Itv.minus itv start_itv )
  |> Itv.set_lb_zero


module BoundsOf (Container : S) = struct
  let of_length exp {location} ~ret:_ mem ~of_function ~degree_kind =
    let itv = Container.length exp mem |> BufferOverrunDomain.Val.get_itv in
    of_itv ~itv ~degree_kind ~of_function location


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
    of_itv ~itv ~degree_kind:Polynomials.DegreeKind.Linear ~of_function:"String.substring" location


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
    let itv = string_len_range_itv model_env exp ~from:None inferbo_mem in
    of_itv ~itv ~degree_kind:Polynomials.DegreeKind.Linear ~of_function:"String.indexOf" location


  (** O(|m|-|n|) where m is the given string and n is the index to start searching from *)
  let indexOf_char_starting_from exp start_exp ({integer_type_widths; location} as model_env)
      ~ret:_ inferbo_mem =
    let itv =
      string_len_range_itv model_env exp ~from:(Some (start_exp, integer_type_widths)) inferbo_mem
    in
    of_itv ~itv ~degree_kind:Polynomials.DegreeKind.Linear ~of_function:"String.indexOf" location


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
      of_itv ~itv ~degree_kind:Polynomials.DegreeKind.Linear ~of_function:"String.indexOf" location
    in
    let m =
      of_itv ~itv:index_itv ~degree_kind:Polynomials.DegreeKind.Linear
        ~of_function:"String.indexOf" location
    in
    BasicCost.mult n m
end

module BoundsOfCollection = BoundsOf (Collection)
module BoundsOfArray = BoundsOf (Array)

module Call = struct
  let dispatch : (Tenv.t, model) ProcnameDispatcher.Call.dispatcher =
    let open ProcnameDispatcher.Call in
    let int_typ = Typ.mk (Typ.Tint Typ.IInt) in
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
        <>--> modeled ~of_function:"Provider.get" ]
end
