(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module IRAttributes = Attributes
open PulseBasicInterface
open PulseOperationResult.Import
open PulseModelsImport

module CoreFoundation = struct
  let cf_bridging_release access : model =
   fun {ret= ret_id, _} astate ->
    let astate = PulseOperations.write_id ret_id access astate in
    PulseOperations.remove_allocation_attr (fst access) astate |> Basic.ok_continue
end

let call args : model =
 (* [call \[args...; Closure _\]] models a call to the closure. It is similar to a
    dispatch function. *)
 fun {path; analysis_data= {analyze_dependency; tenv; proc_desc}; location; ret} astate ->
  match List.last args with
  | Some {ProcnameDispatcher.Call.FuncArg.exp= Closure c} when Procname.is_objc_block c.name ->
      (* TODO(T101946461): This code is very similar to [Pulse.dispatch_call] after the special
         case for objc dispatch models with a bit of [Pulse.interprocedural_call]. Maybe refactor
         it? *)
      PerfEvent.(log (fun logger -> log_begin_event logger ~name:"pulse interproc call" ())) ;
      let actuals = [] in
      let get_pvar_formals pname =
        IRAttributes.load pname |> Option.map ~f:ProcAttributes.get_pvar_formals
      in
      let formals_opt = get_pvar_formals c.name in
      let callee_data = analyze_dependency c.name in
      let call_kind = `Closure c.captured_vars in
      let r, _contradiction =
        PulseCallOperations.call tenv path ~caller_proc_desc:proc_desc ~callee_data location c.name
          ~ret ~actuals ~formals_opt ~call_kind astate
      in
      PerfEvent.(log (fun logger -> log_end_event logger ())) ;
      r
  | _ ->
      (* Nothing to call *)
      Basic.ok_continue astate


let insertion_into_collection_key_and_value (value, value_hist) (key, key_hist) ~desc : model =
 fun {path; location} astate ->
  let event = Hist.call_event path location desc in
  let<*> astate, _ =
    PulseOperations.eval_access path ~must_be_valid_reason:InsertionIntoCollectionValue Read
      location
      (value, Hist.add_event path event value_hist)
      Dereference astate
  in
  let<+> astate, _ =
    PulseOperations.eval_access path ~must_be_valid_reason:InsertionIntoCollectionKey Read location
      (key, Hist.add_event path event key_hist)
      Dereference astate
  in
  astate


let insertion_into_collection_key_or_value (value, value_hist) ~value_kind ~desc : model =
 fun {path; location} astate ->
  let must_be_valid_reason =
    match value_kind with
    | `Key ->
        Invalidation.InsertionIntoCollectionKey
    | `Value ->
        Invalidation.InsertionIntoCollectionValue
  in
  let<+> astate, _ =
    PulseOperations.eval_access path ~must_be_valid_reason Read location
      (value, Hist.add_call path location desc value_hist)
      Dereference astate
  in
  astate


let read_from_collection (key, _key_hist) ~desc : model =
 fun {path; location; ret= ret_id, _} astate ->
  let event = Hist.call_event path location desc in
  let ret_val = AbstractValue.mk_fresh () in
  let astate_nil =
    let<**> astate = PulseArithmetic.prune_eq_zero key astate in
    let<++> astate = PulseArithmetic.and_eq_int ret_val IntLit.zero astate in
    PulseOperations.write_id ret_id (ret_val, Hist.single_event path event) astate
  in
  let astate_not_nil =
    let<++> astate = PulseArithmetic.prune_positive key astate in
    PulseOperations.write_id ret_id (ret_val, Hist.single_event path event) astate
  in
  List.rev_append astate_nil astate_not_nil


(* NOTE: assume that this is always called with [freeWhenDone] being [YES] *)
let init_with_bytes_free_when_done bytes : model =
 fun {path; location; ret= ret_id, _; callee_procname} astate ->
  PulseOperations.havoc_id ret_id
    (Hist.single_call path location (Procname.to_string callee_procname))
    astate
  |> PulseOperations.remove_allocation_attr (fst bytes)
  |> Basic.ok_continue


let alloc_no_fail size : model =
 fun ({ret= ret_id, _} as model_data) astate ->
  (* NOTE: technically this doesn't initialize the result but we haven't modelled initialization so
     assume the object is initialized after [init] for now *)
  let<++> astate =
    Basic.alloc_not_null ~initialize:true ~desc:"alloc" ObjCAlloc (Some size) model_data astate
  in
  let ret_addr =
    match PulseOperations.read_id ret_id astate with
    | None ->
        AbstractValue.mk_fresh ()
    | Some (ret_addr, _) ->
        ret_addr
  in
  PulseOperations.add_ref_counted ret_addr astate


let set_ref_count (value, value_hist) (ref_count, ref_count_hist) : model =
 fun {path; location} astate ->
  let<+> astate =
    PulseOperations.write_field path location ~ref:(value, value_hist)
      PulseOperations.ModeledField.internal_ref_count ~obj:(ref_count, ref_count_hist) astate
  in
  astate


let get_ref_count (value, value_hist) : model =
 fun {path; location; ret= ret_id, _} astate ->
  let<+> astate, (ret_val, ret_hist) =
    PulseOperations.eval_access path Read location (value, value_hist)
      (FieldAccess PulseOperations.ModeledField.internal_ref_count) astate
  in
  PulseOperations.write_id ret_id (ret_val, ret_hist) astate


let construct_string ((value, value_hist) as char_array) : model =
 fun {path; location; ret= ret_id, _} astate ->
  let desc = "NSString.stringWithUTF8String:" in
  let event = Hist.call_event path location desc in
  let<*> astate, _ =
    PulseOperations.eval_access path Read location
      (value, Hist.add_event path event value_hist)
      Dereference astate
  in
  let string = AbstractValue.mk_fresh () in
  let string_hist = Hist.single_call path location desc in
  let<+> astate =
    PulseOperations.write_field path location ~ref:(string, string_hist)
      PulseOperations.ModeledField.internal_string ~obj:char_array astate
  in
  PulseOperations.write_id ret_id (string, string_hist) astate


let check_arg_not_nil (value, value_hist) ~desc : model =
 fun {path; location; ret= ret_id, _} astate ->
  let event = Hist.call_event path location desc in
  let<*> astate, _ =
    PulseOperations.eval_access path ~must_be_valid_reason:(NullArgumentWhereNonNullExpected desc)
      Read location
      (value, Hist.add_event path event value_hist)
      Dereference astate
  in
  let ret_val = AbstractValue.mk_fresh () in
  let<++> astate = PulseArithmetic.prune_positive ret_val astate in
  PulseOperations.write_id ret_id (ret_val, Hist.single_call path location desc) astate


let transfer_ownership_matchers : matcher list =
  let open ProcnameDispatcher.Call in
  let transfer_ownership_namespace_matchers =
    List.map
      ~f:(fun (namespace, m) ->
        -namespace &:: m $ capt_arg_payload $+...$--> CoreFoundation.cf_bridging_release )
      Config.pulse_model_transfer_ownership_namespace
  in
  let transfer_ownership_name_matchers =
    List.map
      ~f:(fun m -> -m $ capt_arg_payload $+...$--> CoreFoundation.cf_bridging_release)
      Config.pulse_model_transfer_ownership
  in
  ( -"NSString" &:: "initWithBytesNoCopy:length:encoding:freeWhenDone:" <>$ any_arg
  $+ capt_arg_payload $+ any_arg $+ any_arg $+ any_arg $--> init_with_bytes_free_when_done )
  :: ( -"NSData" &:: "initWithBytesNoCopy:length:freeWhenDone:" <>$ any_arg $+ capt_arg_payload
     $+ any_arg $+ any_arg $--> init_with_bytes_free_when_done )
  :: transfer_ownership_namespace_matchers
  @ transfer_ownership_name_matchers


let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  let match_regexp_opt r_opt (_tenv, proc_name) _ =
    Option.exists r_opt ~f:(fun r ->
        let s = Procname.to_string proc_name in
        Str.string_match r s 0 )
  in
  let class_match_prefix prefix (_tenv, proc_name) _ =
    Procname.get_objc_class_name proc_name |> Option.exists ~f:(String.is_prefix ~prefix)
  in
  let map_context_tenv f (x, _) = f x in
  [ -"dispatch_sync" <>$ any_arg $++$--> call
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "UITraitCollection")
    &:: "performAsCurrentTraitCollection:" <>$ any_arg $++$--> call
  ; +map_context_tenv PatternMatch.ObjectiveC.is_core_graphics_release
    <>$ capt_arg_payload $--> CoreFoundation.cf_bridging_release
  ; -"CFRelease" <>$ capt_arg_payload $--> CoreFoundation.cf_bridging_release
  ; +match_regexp_opt Config.pulse_model_release_pattern
    <>$ capt_arg_payload $--> CoreFoundation.cf_bridging_release
  ; -"CFAutorelease" <>$ capt_arg_payload $--> CoreFoundation.cf_bridging_release
  ; -"CFBridgingRelease" <>$ capt_arg_payload $--> CoreFoundation.cf_bridging_release
  ; +BuiltinDecl.(match_builtin __objc_bridge_transfer)
    <>$ capt_arg_payload $--> CoreFoundation.cf_bridging_release
  ; +BuiltinDecl.(match_builtin __objc_alloc_no_fail) <>$ capt_exp $--> alloc_no_fail
  ; +BuiltinDecl.(match_builtin __objc_get_ref_count) <>$ capt_arg_payload $--> get_ref_count
  ; +BuiltinDecl.(match_builtin __objc_set_ref_count)
    <>$ capt_arg_payload $+ capt_arg_payload $--> set_ref_count
  ; +class_match_prefix "NS"
    &:: "init" <>$ capt_arg_payload
    $--> Basic.id_first_arg ~desc:"NSObject.init"
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSString")
    &:: "stringWithUTF8String:" <>$ capt_arg_payload $--> construct_string
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSString")
    &:: "initWithFormat:" <>$ any_arg $+ capt_arg_payload
    $+...$--> check_arg_not_nil ~desc:"NSString.initWithFormat:"
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSString")
    &:: "stringWithFormat:" <>$ capt_arg_payload
    $+...$--> check_arg_not_nil ~desc:"NSString.stringWithFormat:"
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSString")
    &:: "stringWithString:" <>$ capt_arg_payload
    $--> check_arg_not_nil ~desc:"NSString.stringWithString:"
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSString")
    &:: "stringByAppendingString:" <>$ any_arg $+ capt_arg_payload
    $--> check_arg_not_nil ~desc:"NSString.stringByAppendingString:"
  ; +BuiltinDecl.(match_builtin objc_insert_key)
    <>$ capt_arg_payload
    $--> insertion_into_collection_key_or_value ~value_kind:`Key
           ~desc:"key insertion into collection literal"
  ; +BuiltinDecl.(match_builtin objc_insert_value)
    <>$ capt_arg_payload
    $--> insertion_into_collection_key_or_value ~value_kind:`Value
           ~desc:"value insertion into collection literal"
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSMutableDictionary")
    &:: "setObject:forKey:" <>$ any_arg $+ capt_arg_payload $+ capt_arg_payload
    $--> insertion_into_collection_key_and_value ~desc:"NSMutableDictionary.setObject:forKey:"
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSMutableDictionary")
    &:: "setObject:forKeyedSubscript:" <>$ any_arg $+ any_arg $+ capt_arg_payload
    $--> insertion_into_collection_key_or_value ~value_kind:`Key
           ~desc:"mutableDictionary[someKey] = value"
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSMutableDictionary")
    &:: "removeObjectForKey:" <>$ any_arg $+ capt_arg_payload
    $--> insertion_into_collection_key_or_value ~value_kind:`Key
           ~desc:"NSMutableDictionary.removeObjectForKey"
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSMutableDictionary")
    &:: "dictionaryWithSharedKeySet:" <>$ capt_arg_payload
    $--> insertion_into_collection_key_or_value ~value_kind:`Key
           ~desc:"NSMutableDictionary.dictionaryWithSharedKeySet"
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSMutableArray")
    &:: "addObject:" <>$ any_arg $+ capt_arg_payload
    $--> insertion_into_collection_key_or_value ~value_kind:`Value ~desc:"NSMutableArray.addObject:"
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSMutableArray")
    &:: "insertObject:atIndex:" <>$ any_arg $+ capt_arg_payload $+ any_arg
    $--> insertion_into_collection_key_or_value ~value_kind:`Value
           ~desc:"NSMutableArray.insertObject:atIndex:"
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSMutableArray")
    &:: "replaceObjectAtIndex:withObject:" <>$ any_arg $+ any_arg $+ capt_arg_payload
    $--> insertion_into_collection_key_or_value ~value_kind:`Value
           ~desc:"NSMutableArray.replaceObjectAtIndex:withObject:"
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSMutableSet")
    &:: "addObject:" <>$ any_arg $+ capt_arg_payload
    $--> insertion_into_collection_key_or_value ~value_kind:`Value ~desc:"NSMutableSet.addObject:"
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSMutableSet")
    &:: "removeObject:" <>$ any_arg $+ capt_arg_payload
    $--> insertion_into_collection_key_or_value ~value_kind:`Value
           ~desc:"NSMutableSet.removeObject:"
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSMutableArray")
    &:: "removeObjectsAtIndexes:" <>$ any_arg $+ capt_arg_payload
    $--> insertion_into_collection_key_or_value ~value_kind:`Key
           ~desc:"NSMutableArray.removeObjectsAtIndexes:"
  ; ( +map_context_tenv (PatternMatch.ObjectiveC.implements "NSMutableArray")
    &:: "replaceObjectsAtIndexes:withObjects:" <>$ any_arg $+ capt_arg_payload $+ capt_arg_payload
    $--> fun k v ->
    insertion_into_collection_key_and_value v k
      ~desc:"NSMutableArray.replaceObjectsAtIndexes:withObjects:" )
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSDictionary")
    &:: "dictionaryWithObject:forKey:" <>$ capt_arg_payload $+ capt_arg_payload
    $--> insertion_into_collection_key_and_value ~desc:"NSDictionary.dictionaryWithObject:forKey:"
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSDictionary")
    &:: "sharedKeySetForKeys:" <>$ capt_arg_payload
    $--> insertion_into_collection_key_or_value ~value_kind:`Key
           ~desc:"NSDictionary.sharedKeySetForKeys"
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSDictionary")
    &:: "objectForKey:" <>$ any_arg $+ capt_arg_payload
    $--> read_from_collection ~desc:"NSDictionary.objectForKey"
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSDictionary")
    &:: "objectForKeyedSubscript:" <>$ any_arg $+ capt_arg_payload
    $--> read_from_collection ~desc:"NSDictionary.objectForKeyedSubscript"
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSSet")
    &:: "setWithObject:" <>$ capt_arg_payload
    $--> insertion_into_collection_key_or_value ~value_kind:`Value ~desc:"NSSet.setWithObject"
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSSet")
    &:: "setByAddingObject:" <>$ any_arg $+ capt_arg_payload
    $--> insertion_into_collection_key_or_value ~value_kind:`Value ~desc:"NSSet.setByAddingObject"
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSArray")
    &:: "arrayWithObject:" <>$ capt_arg_payload
    $--> insertion_into_collection_key_or_value ~value_kind:`Value ~desc:"NSArray.arrayWithObject"
  ]
