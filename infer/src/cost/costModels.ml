(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module BasicCost = CostDomain.BasicCost
open BufferOverrunUtils.ModelEnv
open CostUtils.CostModelEnv
open ProcnameDispatcher.Call.FuncArg

let unit_cost_model _model_env ~ret:_ _inferbo_mem = BasicCost.one

let cost_of_exp exp ~degree_kind ~of_function {model_env= {integer_type_widths; location}} ~ret:_
    inferbo_mem =
  let itv =
    BufferOverrunSemantics.eval integer_type_widths exp inferbo_mem
    |> BufferOverrunDomain.Val.get_itv
  in
  CostUtils.cost_of_itv ~itv ~degree_kind ~of_function location


let linear = cost_of_exp ~degree_kind:Polynomials.DegreeKind.Linear

let log = cost_of_exp ~degree_kind:Polynomials.DegreeKind.Log

module Iterator = struct
  let get_iter_itv begin_exp end_exp inferbo_mem =
    let begin_v =
      BufferOverrunDomain.Mem.find_set
        (BufferOverrunSemantics.eval_locs begin_exp inferbo_mem)
        inferbo_mem
    in
    let begin_itv = BufferOverrunDomain.Val.get_itv begin_v in
    let end_v =
      BufferOverrunDomain.Mem.find_set
        (BufferOverrunSemantics.eval_locs end_exp inferbo_mem)
        inferbo_mem
    in
    let end_itv = BufferOverrunDomain.Val.get_itv end_v in
    let () =
      Logging.d_printfln_escaped "length of begin_iter=%a and end_iter=%a"
        BufferOverrunDomain.Val.pp begin_v BufferOverrunDomain.Val.pp end_v
    in
    Itv.minus end_itv begin_itv
end

module BoundsOf (Container : CostUtils.S) = struct
  let of_length exp ~of_function {model_env= {location}} ~ret:_ mem ~degree_kind =
    let itv = Container.length exp mem |> BufferOverrunDomain.Val.get_itv in
    CostUtils.cost_of_itv ~itv ~degree_kind ~of_function location


  let of_length_itv itv ~of_function {model_env= {location}} ~ret:_ ~degree_kind =
    CostUtils.cost_of_itv ~itv ~degree_kind ~of_function location


  let linear_length = of_length ~degree_kind:Polynomials.DegreeKind.Linear

  let linear_length_itv itv = of_length_itv itv ~degree_kind:Polynomials.DegreeKind.Linear

  let logarithmic_length_itv itv = of_length_itv itv ~degree_kind:Polynomials.DegreeKind.Log

  let logarithmic_length = of_length ~degree_kind:Polynomials.DegreeKind.Log

  let n_log_n_length exp ~of_function cost_model_env ~ret mem =
    let log_n = logarithmic_length exp ~of_function cost_model_env mem ~ret in
    let n = linear_length exp ~of_function cost_model_env mem ~ret in
    BasicCost.mult n log_n


  let n_log_n_length_itv itv ~of_function cost_model_env ~ret =
    let log_n = logarithmic_length_itv itv ~of_function cost_model_env ~ret in
    let n = linear_length_itv itv ~of_function cost_model_env ~ret in
    BasicCost.mult n log_n


  let op_on_two_lengths exp1 exp2 ~f ~of_function cost_model_env ~ret mem =
    let n = linear_length exp1 ~of_function cost_model_env mem ~ret in
    let m = linear_length exp2 ~of_function cost_model_env mem ~ret in
    f n m
end

module IntHashMap = struct
  let keys {ProcnameDispatcher.Call.FuncArg.exp; typ} {model_env= {location}} ~ret:_ inferbo_mem =
    let locs = BufferOverrunSemantics.eval_locs exp inferbo_mem in
    match AbsLoc.PowLoc.is_singleton_or_more locs with
    | Singleton this_loc -> (
      match (AbsLoc.Loc.get_path this_loc, Typ.strip_ptr typ |> Typ.name) with
      | Some path, Some typ_name ->
          let path = Symb.SymbolPath.append_field path (Fieldname.make typ_name "size") in
          let itv = Itv.of_normal_path ~unsigned:true path in
          CostUtils.cost_of_itv ~itv ~degree_kind:Linear ~of_function:"IntHashMap.keys" location
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
    CostUtils.cost_of_itv ~itv ~degree_kind:Polynomials.DegreeKind.Linear
      ~of_function:"String.substring" location


  let substring_no_end exp begin_idx {model_env} ~ret:_ inferbo_mem =
    substring_aux ~begin_idx
      ~end_v:(BufferOverrunModels.JavaString.get_length model_env exp inferbo_mem)
      model_env inferbo_mem


  let substring begin_idx end_idx {model_env= {integer_type_widths} as model_env} ~ret:_ inferbo_mem
      =
    substring_aux ~begin_idx
      ~end_v:(BufferOverrunSemantics.eval integer_type_widths end_idx inferbo_mem)
      model_env inferbo_mem


  (** O(|m|) where m is the given string and |.| is the length function *)
  let indexOf_char exp {model_env= {location} as model_env} ~ret:_ inferbo_mem =
    let itv = CostUtils.string_len_range_itv model_env exp ~from:None inferbo_mem in
    CostUtils.cost_of_itv ~itv ~degree_kind:Polynomials.DegreeKind.Linear
      ~of_function:"String.indexOf" location


  (** O(|m|-|n|) where m is the given string and n is the index to start searching from *)
  let indexOf_char_starting_from exp start_exp
      {model_env= {integer_type_widths; location} as model_env} ~ret:_ inferbo_mem =
    let itv =
      CostUtils.string_len_range_itv model_env exp
        ~from:(Some (start_exp, integer_type_widths))
        inferbo_mem
    in
    CostUtils.cost_of_itv ~itv ~degree_kind:Polynomials.DegreeKind.Linear
      ~of_function:"String.indexOf" location


  (** O(|m|.|n|) where m and n are the given strings *)
  let indexOf_str exp index_exp {model_env= {location} as model_env} ~ret:_ inferbo_mem =
    let itv =
      BufferOverrunModels.JavaString.get_length model_env exp inferbo_mem
      |> BufferOverrunDomain.Val.get_itv
    in
    let index_itv =
      BufferOverrunModels.JavaString.get_length model_env index_exp inferbo_mem
      |> BufferOverrunDomain.Val.get_itv
    in
    let n =
      CostUtils.cost_of_itv ~itv ~degree_kind:Polynomials.DegreeKind.Linear
        ~of_function:"String.indexOf" location
    in
    let m =
      CostUtils.cost_of_itv ~itv:index_itv ~degree_kind:Polynomials.DegreeKind.Linear
        ~of_function:"String.indexOf" location
    in
    BasicCost.mult n m
end

module BoundsOfCollection = BoundsOf (CostUtils.Collection)
module BoundsOfContainer = BoundsOf (CostUtils.Container)
module BoundsOfNSCollection = BoundsOf (CostUtils.NSCollection)
module BoundsOfArray = BoundsOf (CostUtils.Array)
module BoundsOfCString = BoundsOf (CostUtils.CString)

module Algorithm = struct
  let binary_search begin_arg end_arg cost_model_env ~ret inferbo_mem ~of_function =
    let itv = Iterator.get_iter_itv begin_arg end_arg inferbo_mem in
    BoundsOfContainer.logarithmic_length_itv itv cost_model_env ~ret ~of_function


  let find begin_arg end_arg cost_model_env ~ret inferbo_mem ~of_function =
    let itv = Iterator.get_iter_itv begin_arg end_arg inferbo_mem in
    BoundsOfContainer.linear_length_itv itv cost_model_env ~ret ~of_function


  let erase begin_arg end_arg cost_model_env ~ret inferbo_mem ~of_function =
    let itv = Iterator.get_iter_itv begin_arg end_arg inferbo_mem in
    BoundsOfContainer.linear_length_itv itv cost_model_env ~ret ~of_function


  let sort begin_arg end_arg cost_model_env ~ret inferbo_mem ~of_function =
    let itv = Iterator.get_iter_itv begin_arg end_arg inferbo_mem in
    BoundsOfContainer.n_log_n_length_itv itv cost_model_env ~ret ~of_function
end

module NSString = struct
  let get_length str ~of_function {model_env= {location} as model_env} ~ret:_ mem =
    let itv =
      BufferOverrunModels.NSString.get_length model_env str mem |> BufferOverrunDomain.Val.get_itv
    in
    CostUtils.cost_of_itv ~itv ~degree_kind:Polynomials.DegreeKind.Linear ~of_function location


  let op_on_two_str cost_op ~of_function str1 str2 cost_model_env ~ret mem =
    let get_length str = get_length str ~of_function cost_model_env ~ret mem in
    cost_op (get_length str1) (get_length str2)


  let substring_from_index = JavaString.substring_no_end
end

module NSAttributedString = struct
  let enumerate_using_block args ({model_env} as cost_model_env) ~ret inferbo_mem =
    let pname = model_env.pname in
    match List.rev args with
    | _attr :: _inRange :: _options :: _usingBlock :: {exp= str} :: _captured_args -> (
        let length =
          BoundsOfCString.linear_length
            ~of_function:(Procname.to_simplified_string pname)
            str cost_model_env ~ret inferbo_mem
        in
        match pname with _ -> length )
    | _ ->
        BasicCost.one
end

module NSCollection = struct
  let get_length str ~of_function {model_env= {location}} ~ret:_ mem =
    let itv =
      BufferOverrunModels.NSCollection.eval_collection_length str mem
      |> BufferOverrunDomain.Val.get_itv
    in
    CostUtils.cost_of_itv ~itv ~degree_kind:Polynomials.DegreeKind.Linear ~of_function location


  let op_on_two_coll cost_op ~of_function coll1 coll2 cost_model_env ~ret mem =
    let get_length coll = get_length coll ~of_function cost_model_env ~ret mem in
    cost_op (get_length coll1) (get_length coll2)


  let enumerate_using_block args ({model_env} as cost_model_env) ~ret inferbo_mem =
    let pname = model_env.pname in
    match List.rev args with
    | _block :: {exp= array} :: _captured_args -> (
        let length =
          BoundsOfNSCollection.linear_length
            ~of_function:(Procname.to_simplified_string pname)
            array cost_model_env ~ret inferbo_mem
        in
        match pname with _ -> length )
    | _ ->
        BasicCost.one
end

module ImmutableSet = struct
  let construct = linear ~of_function:"ImmutableSet.construct"

  let choose_table_size = log ~of_function:"ImmutableSet.chooseTableSize"
end

let std_container_ord _ str =
  List.exists ~f:(String.equal str) ["map"; "multimap"; "multiset"; "set"]


module Call = struct
  let dispatch : (Tenv.t, CostUtils.model, unit) ProcnameDispatcher.Call.dispatcher =
    let open ProcnameDispatcher.Call in
    let int_typ = Typ.mk (Typ.Tint Typ.IInt) in
    let dispatcher =
      make_dispatcher
        [ -"google" &:: "StrLen" <>$ capt_exp
          $--> BoundsOfCString.linear_length ~of_function:"google::StrLen"
        ; +PatternMatch.ObjectiveC.implements "NSString"
          &:: "stringWithUTF8String:" <>$ capt_exp
          $--> BoundsOfCString.linear_length ~of_function:"NSString.stringWithUTF8String:"
        ; +PatternMatch.ObjectiveC.implements "NSString"
          &:: "stringByAppendingString:" <>$ capt_exp $+ capt_exp
          $--> NSString.op_on_two_str BasicCost.plus
                 ~of_function:"NSString.stringByAppendingString:"
        ; +PatternMatch.ObjectiveC.implements "NSString"
          &:: "stringByAppendingPathComponent:" <>$ capt_exp $+ capt_exp
          $--> NSString.op_on_two_str BasicCost.plus
                 ~of_function:"NSString.stringByAppendingPathComponent:"
        ; +PatternMatch.ObjectiveC.implements "NSString"
          &:: "isEqualToString:" <>$ capt_exp $+ capt_exp
          $--> NSString.op_on_two_str BasicCost.min_default_left
                 ~of_function:"NSString.isEqualToString:"
        ; +PatternMatch.ObjectiveC.implements "NSString"
          &:: "hasPrefix:" <>$ capt_exp $+ capt_exp
          $--> NSString.op_on_two_str BasicCost.min_default_left ~of_function:"NSString.hasPrefix:"
        ; +PatternMatch.ObjectiveC.implements "NSString"
          &:: "substringFromIndex:" <>$ capt_exp $+ capt_exp $!--> NSString.substring_from_index
        ; +PatternMatch.ObjectiveC.implements "NSString"
          &:: "rangeOfString:" <>$ capt_exp $+ capt_exp $+ any_arg
          $!--> NSString.op_on_two_str BasicCost.mult ~of_function:"NSString.rangeOfString:"
        ; +PatternMatch.ObjectiveC.implements "NSMutableString"
          &:: "appendString:" <>$ any_arg $+ capt_exp
          $--> NSString.get_length ~of_function:"NSMutableString.appendString:"
        ; +PatternMatch.ObjectiveC.implements "NSString"
          &:: "componentsSeparatedByString:" <>$ capt_exp $+ capt_exp
          $--> NSString.op_on_two_str BasicCost.mult
                 ~of_function:"NSString.componentsSeparatedByString:"
        ; +PatternMatch.ObjectiveC.implements "NSArray"
          &:: "initWithArray:" <>$ any_arg $+ capt_exp
          $--> BoundsOfNSCollection.linear_length ~of_function:"NSArray.initWithArray:"
        ; +PatternMatch.ObjectiveC.implements "NSArray"
          &:: "isEqualToArray:" <>$ capt_exp $+ capt_exp
          $--> NSCollection.op_on_two_coll BasicCost.min_default_left
                 ~of_function:"NSArray.isEqualToArray:"
        ; +PatternMatch.ObjectiveC.implements "NSArray"
          &:: "containsObject:" <>$ capt_exp $+ any_arg
          $--> BoundsOfNSCollection.linear_length ~of_function:"NSArray.containsObject:"
        ; +PatternMatch.ObjectiveC.implements "NSArray"
          &:: "sortedArrayUsingDescriptors:" <>$ capt_exp $+ any_arg
          $--> BoundsOfNSCollection.n_log_n_length
                 ~of_function:"NSArray.sortedArrayUsingDescriptors:"
        ; +PatternMatch.ObjectiveC.implements "NSArray"
          &:: "arrayByAddingObjectsFromArray:" <>$ capt_exp $+ capt_exp
          $--> NSCollection.op_on_two_coll BasicCost.plus
                 ~of_function:"NSArray.arrayByAddingObjectsFromArray:"
        ; +PatternMatch.ObjectiveC.implements "NSMutableArray"
          &:: "removeAllObjects" <>$ capt_exp
          $--> BoundsOfNSCollection.linear_length ~of_function:"NSArray.removeAllObjects"
        ; +PatternMatch.ObjectiveC.implements "NSDictionary"
          &:: "dictionaryWithDictionary:" <>$ capt_exp
          $--> BoundsOfNSCollection.linear_length
                 ~of_function:"NSDictionary.dictionaryWithDictionary:"
        ; +PatternMatch.ObjectiveC.implements "NSDictionary"
          &:: "initWithDictionary:" <>$ any_arg $+ capt_exp
          $--> BoundsOfNSCollection.linear_length ~of_function:"NSArray.initWithArray:"
        ; +PatternMatch.ObjectiveC.implements "NSMutableDictionary"
          &:: "removeAllObjects" <>$ capt_exp
          $--> BoundsOfNSCollection.linear_length
                 ~of_function:"NSMutableDictionary.removeAllObjects"
        ; +PatternMatch.ObjectiveC.implements "NSMutableDictionary"
          &:: "addEntriesFromDictionary:" <>$ any_arg $+ capt_exp
          $--> BoundsOfNSCollection.linear_length
                 ~of_function:"addEntriesFromDictionary.NSMutableDictionary:"
        ; +PatternMatch.ObjectiveC.implements "NSMutableArray"
          &:: "addObjectsFromArray:" <>$ any_arg $+ capt_exp
          $--> BoundsOfNSCollection.linear_length ~of_function:"NSArray.addObjectsFromArray:"
        ; +PatternMatch.ObjectiveC.implements_collection
          &:: "enumerateObjectsUsingBlock:" &::.*++> NSCollection.enumerate_using_block
        ; +PatternMatch.ObjectiveC.implements "NSAttributedString"
          &:: "enumerateAttribute:inRange:options:usingBlock:"
          &::.*++> NSAttributedString.enumerate_using_block
          (* C++ Cost Models *)
        ; -"std" &:: "binary_search" $ capt_exp $+ capt_exp
          $+...$--> Algorithm.binary_search ~of_function:"Container.binary_search"
        ; -"std" &:: "sort" $ capt_exp $+ capt_exp
          $+...$--> Algorithm.sort ~of_function:"Container.sort"
        ; -"std" &:: "find" $ capt_exp $+ capt_exp
          $+...$--> Algorithm.find ~of_function:"Container.find"
        ; -"std" &::+ std_container_ord &:: "find" $ capt_exp
          $+...$--> BoundsOfContainer.logarithmic_length ~of_function:"Container.find"
        ; -"std" &::+ std_container_ord &:: "count" $ capt_exp
          $+...$--> BoundsOfContainer.logarithmic_length ~of_function:"Container.count"
        ; -"std" &::+ std_container_ord &:: "emplace" $ capt_exp
          $+...$--> BoundsOfContainer.logarithmic_length ~of_function:"Container.emplace"
        ; -"std" &::+ std_container_ord &:: "emplace_hint" $ capt_exp
          $+...$--> BoundsOfContainer.logarithmic_length ~of_function:"Container.emplace_hint"
        ; -"std" &::+ std_container_ord &:: "erase" $ capt_exp $+ any_arg
          $--> BoundsOfContainer.logarithmic_length ~of_function:"Container.erase"
        ; -"std" &::+ std_container_ord &:: "erase" $ any_arg $+ capt_exp $+ capt_exp
          $+...$--> Algorithm.erase ~of_function:"Container.erase"
          (* Java Cost Models *)
        ; +PatternMatch.Java.implements_collections
          &:: "sort" $ capt_exp
          $+...$--> BoundsOfCollection.n_log_n_length ~of_function:"Collections.sort"
        ; +PatternMatch.Java.implements_list
          &:: "sort" $ capt_exp
          $+...$--> BoundsOfCollection.n_log_n_length ~of_function:"List.sort"
        ; +PatternMatch.Java.implements_arrays
          &:: "sort" $ capt_exp
          $+...$--> BoundsOfArray.n_log_n_length ~of_function:"Arrays.sort"
        ; +PatternMatch.Java.implements_set &:: "contains" <>$ any_arg $+ any_arg
          $--> unit_cost_model
        ; +PatternMatch.Java.implements_set &:: "containsAll" <>$ any_arg $+ capt_exp
          $--> BoundsOfCollection.linear_length ~of_function:"Set.containsAll"
        ; +PatternMatch.Java.implements_collection
          &:: "removeAll" <>$ any_arg $+ capt_exp
          $--> BoundsOfCollection.linear_length ~of_function:"Collection.removeAll"
        ; +PatternMatch.Java.implements_list
          &:: "contains" <>$ capt_exp
          $+...$--> BoundsOfCollection.linear_length ~of_function:"List.contains"
        ; +PatternMatch.Java.implements_collection
          &:: "contains" <>$ capt_exp
          $+...$--> BoundsOfCollection.linear_length ~of_function:"Collection.contains"
        ; +PatternMatch.Java.implements_collection
          &:: "containsAll" <>$ capt_exp $+ capt_exp
          $+...$--> BoundsOfCollection.op_on_two_lengths ~f:BasicCost.mult
                      ~of_function:"Collection.containsAll"
        ; +PatternMatch.Java.implements_collections
          &:: "binarySearch" <>$ capt_exp
          $+...$--> BoundsOfCollection.logarithmic_length ~of_function:"Collections.binarySearch"
        ; +PatternMatch.Java.implements_arrays
          &:: "binarySearch" <>$ capt_exp
          $+...$--> BoundsOfArray.logarithmic_length ~of_function:"Arrays.binarySearch"
        ; +PatternMatch.Java.implements_arrays
          &:: "copyOf" <>$ any_arg $+ capt_exp
          $+...$--> linear ~of_function:"Arrays.copyOf"
        ; +PatternMatch.Java.implements_collection
          &:: "addAll" <>$ any_arg $+ any_arg $+ capt_exp
          $--> BoundsOfCollection.linear_length ~of_function:"Collection.addAll"
        ; +PatternMatch.Java.implements_collection
          &:: "addAll" <>$ any_arg $+ capt_exp
          $--> BoundsOfCollection.linear_length ~of_function:"Collection.addAll"
        ; +PatternMatch.Java.implements_collections
          &:: "copy" <>$ capt_exp
          $+...$--> BoundsOfCollection.linear_length ~of_function:"Collections.copy"
          (* TODO: T72085946; take the cost of function into account *)
        ; +PatternMatch.Java.implements_iterable
          &:: "forEach" $ capt_exp
          $+...$--> BoundsOfCollection.linear_length ~of_function:"Iterable.forEach"
        ; +PatternMatch.Java.implements_map &:: "forEach" $ capt_exp
          $+...$--> BoundsOfCollection.linear_length ~of_function:"Map.forEach"
        ; +PatternMatch.Java.implements_map &:: "containsValue" $ capt_exp
          $+...$--> BoundsOfCollection.linear_length ~of_function:"Map.containsValue"
        ; +PatternMatch.Java.implements_collections
          &:: "fill" <>$ capt_exp
          $+...$--> BoundsOfCollection.linear_length ~of_function:"Collections.fill"
        ; +PatternMatch.Java.implements_arrays
          &:: "fill" <>$ capt_exp
          $+...$--> BoundsOfArray.linear_length ~of_function:"Arrays.fill"
        ; +PatternMatch.Java.implements_collections
          &:: "reverse" <>$ capt_exp
          $+...$--> BoundsOfCollection.linear_length ~of_function:"Collections.reverse"
        ; +PatternMatch.Java.implements_collections
          &:: "max" <>$ capt_exp
          $+...$--> BoundsOfCollection.linear_length ~of_function:"Collections.max"
        ; +PatternMatch.Java.implements_collections
          &:: "min" <>$ capt_exp
          $+...$--> BoundsOfCollection.linear_length ~of_function:"Collections.min"
        ; +PatternMatch.Java.implements_collections
          &:: "shuffle" <>$ capt_exp
          $+...$--> BoundsOfCollection.linear_length ~of_function:"Collections.shuffle"
        ; +PatternMatch.Java.implements_lang "String"
          &:: "substring" <>$ capt_exp $+ capt_exp $--> JavaString.substring_no_end
        ; +PatternMatch.Java.implements_lang "String"
          &:: "indexOf" <>$ capt_exp
          $+ capt_exp_of_typ (+PatternMatch.Java.implements_lang "String")
          $--> JavaString.indexOf_str
        ; +PatternMatch.Java.implements_lang "String"
          &:: "indexOf" <>$ capt_exp $+ any_arg_of_prim_typ int_typ $+ capt_exp
          $--> JavaString.indexOf_char_starting_from
        ; +PatternMatch.Java.implements_lang "String"
          &:: "indexOf" <>$ capt_exp $+ any_arg_of_prim_typ int_typ $--> JavaString.indexOf_char
        ; +PatternMatch.Java.implements_lang "String"
          &:: "substring"
          $ any_arg_of_typ (+PatternMatch.Java.implements_lang "String")
          $+ capt_exp $+ capt_exp $--> JavaString.substring
        ; +PatternMatch.Java.implements_xmob_utils "IntHashMap" &:: "<init>" <>--> unit_cost_model
        ; +PatternMatch.Java.implements_xmob_utils "IntHashMap"
          &:: "getElement" <>--> unit_cost_model
        ; +PatternMatch.Java.implements_xmob_utils "IntHashMap"
          &:: "keys" <>$ capt_arg $--> IntHashMap.keys
        ; +PatternMatch.Java.implements_xmob_utils "IntHashMap" &:: "put" <>--> unit_cost_model
        ; +PatternMatch.Java.implements_xmob_utils "IntHashMap" &:: "remove" <>--> unit_cost_model
        ; +PatternMatch.Java.implements_google "common.collect.ImmutableSet"
          &:: "chooseTableSize" <>$ capt_exp $+...$--> ImmutableSet.choose_table_size
        ; +PatternMatch.Java.implements_google "common.collect.ImmutableSet"
          &:: "construct" <>$ capt_exp_of_prim_typ int_typ $+...$--> ImmutableSet.construct
        ; +PatternMatch.Java.implements_google "common.collect.ImmutableSet"
          &:: "construct" <>$ any_arg $+ capt_exp_of_prim_typ int_typ
          $+...$--> ImmutableSet.construct
        ; +PatternMatch.Java.implements_sparse_float_array
          &:: "binarySearch" <>$ any_arg $+ capt_exp_of_prim_typ int_typ
          $+...$--> log ~of_function:"binarySearch" ]
    in
    merge_dispatchers dispatcher FbCostModels.Call.dispatch
end
