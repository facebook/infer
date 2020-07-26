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

let provider_get {pname; location} ~ret:(_, ret_typ) _ : BasicCost.t =
  let callsite = CallSite.make pname location in
  let path = Symb.SymbolPath.of_callsite ~ret_typ callsite in
  let itv = Itv.of_modeled_path ~is_expensive:true path in
  CostUtils.of_itv ~itv ~degree_kind:Polynomials.DegreeKind.Linear ~of_function:"Provider.get"
    location


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

module IntHashMap = struct
  let keys {ProcnameDispatcher.Call.FuncArg.exp; typ} {location} ~ret:_ inferbo_mem =
    let locs = BufferOverrunSemantics.eval_locs exp inferbo_mem in
    match AbsLoc.PowLoc.is_singleton_or_more locs with
    | Singleton this_loc -> (
      match (AbsLoc.Loc.get_path this_loc, Typ.strip_ptr typ |> Typ.name) with
      | Some path, Some typ_name ->
          let path = Symb.SymbolPath.append_field path (Fieldname.make typ_name "size") in
          let itv = Itv.of_normal_path ~unsigned:true path in
          CostUtils.of_itv ~itv ~degree_kind:Linear ~of_function:"IntHashMap.keys" location
      | _, _ ->
          BasicCost.top )
    | Empty | More ->
        BasicCost.top
end

module JavaString = struct
  let substring_aux ~begin_idx ~end_v {integer_type_widths; location} inferbo_mem =
    let begin_v = BufferOverrunSemantics.eval integer_type_widths begin_idx inferbo_mem in
    let begin_itv = BufferOverrunDomain.Val.get_itv begin_v in
    let end_itv = BufferOverrunDomain.Val.get_itv end_v in
    let itv =
      if
        Boolean.is_true (Itv.le_sem Itv.zero begin_itv)
        && Boolean.is_true (Itv.le_sem begin_itv end_itv)
      then Itv.minus end_itv begin_itv
      else
        (* in practice, either we have two symbolic bounds that are semantically
           incomparable at this point or there is an out of bounds exception. In
           both cases, we don't want to give negative cost so we
           behave as if there is no model and give unit cost. *)
        Itv.of_int 1
    in
    CostUtils.of_itv ~itv ~degree_kind:Polynomials.DegreeKind.Linear ~of_function:"String.substring"
      location


  let substring_no_end exp begin_idx model_env ~ret:_ inferbo_mem =
    substring_aux ~begin_idx
      ~end_v:(BufferOverrunModels.JavaString.get_length model_env exp inferbo_mem)
      model_env inferbo_mem


  let substring begin_idx end_idx ({integer_type_widths} as model_env) ~ret:_ inferbo_mem =
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
module BoundsOfCString = BoundsOf (CostUtils.CString)

module NSString = struct
  let get_length str ~of_function ({location} as model_env) ~ret:_ mem =
    let itv =
      BufferOverrunModels.NSString.get_length model_env str mem |> BufferOverrunDomain.Val.get_itv
    in
    CostUtils.of_itv ~itv ~degree_kind:Polynomials.DegreeKind.Linear ~of_function location


  let op_on_two_str cost_op ~of_function str1 str2 model_env ~ret mem =
    let get_length str = get_length str ~of_function model_env ~ret mem in
    cost_op (get_length str1) (get_length str2)


  let substring_from_index = JavaString.substring_no_end
end

module ImmutableSet = struct
  let construct = linear ~of_function:"ImmutableSet.construct"

  let choose_table_size = log ~of_function:"ImmutableSet.chooseTableSize"
end

module Call = struct
  let dispatch : (Tenv.t, CostUtils.model, unit) ProcnameDispatcher.Call.dispatcher =
    let open ProcnameDispatcher.Call in
    let int_typ = Typ.mk (Typ.Tint Typ.IInt) in
    let dispatcher =
      make_dispatcher
        [ -"google" &:: "StrLen" <>$ capt_exp
          $--> BoundsOfCString.linear_length ~of_function:"google::StrLen"
        ; -"NSString" &:: "stringWithUTF8String:" <>$ capt_exp
          $--> BoundsOfCString.linear_length ~of_function:"NSString.stringWithUTF8String:"
        ; -"NSString" &:: "stringByAppendingString:" <>$ capt_exp $+ capt_exp
          $--> NSString.op_on_two_str BasicCost.plus
                 ~of_function:"NSString.stringByAppendingString:"
        ; -"NSString" &:: "stringByAppendingPathComponent:" <>$ capt_exp $+ capt_exp
          $--> NSString.op_on_two_str BasicCost.plus
                 ~of_function:"NSString.stringByAppendingPathComponent:"
        ; -"NSString" &:: "isEqualToString:" <>$ capt_exp $+ capt_exp
          $--> NSString.op_on_two_str BasicCost.min_default_left
                 ~of_function:"NSString.isEqualToString:"
        ; -"NSString" &:: "hasPrefix:" <>$ capt_exp $+ capt_exp
          $--> NSString.op_on_two_str BasicCost.min_default_left ~of_function:"NSString.hasPrefix:"
        ; -"NSString" &:: "substringFromIndex:" <>$ capt_exp $+ capt_exp
          $!--> NSString.substring_from_index
        ; -"NSString" &:: "rangeOfString:" <>$ capt_exp $+ capt_exp
          $!--> NSString.op_on_two_str BasicCost.mult ~of_function:"NSString.rangeOfString:"
        ; -"NSMutableString" &:: "appendString:" <>$ any_arg $+ capt_exp
          $--> NSString.get_length ~of_function:"NSMutableString.appendString:"
        ; -"NSString" &:: "componentsSeparatedByString:" <>$ capt_exp $+ capt_exp
          $--> NSString.op_on_two_str BasicCost.mult
                 ~of_function:"NSString.componentsSeparatedByString:"
        ; +PatternMatch.implements_collections
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
        ; +PatternMatch.implements_collection
          &:: "addAll" <>$ any_arg $+ any_arg $+ capt_exp
          $--> BoundsOfCollection.linear_length ~of_function:"Collection.addAll"
        ; +PatternMatch.implements_collection
          &:: "addAll" <>$ any_arg $+ capt_exp
          $--> BoundsOfCollection.linear_length ~of_function:"Collection.addAll"
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
          &:: "substring" <>$ capt_exp $+ capt_exp $--> JavaString.substring_no_end
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
          $+ capt_exp $+ capt_exp $--> JavaString.substring
        ; +PatternMatch.implements_inject "Provider" &:: "get" <>--> provider_get
        ; +PatternMatch.implements_xmob_utils "IntHashMap" &:: "<init>" <>--> unit_cost_model
        ; +PatternMatch.implements_xmob_utils "IntHashMap" &:: "getElement" <>--> unit_cost_model
        ; +PatternMatch.implements_xmob_utils "IntHashMap"
          &:: "keys" <>$ capt_arg $--> IntHashMap.keys
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
