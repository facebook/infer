(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface
open PulseOperationResult.Import
open PulseModelsImport
module DSL = PulseModelsDSL
module GenericArrayBackedCollection = PulseModelsGenericArrayBackedCollection

module CoreFoundation = struct
  let cf_bridging_release access : model_no_non_disj =
   fun {ret= ret_id, _} astate ->
    let astate = PulseOperations.write_id ret_id access astate in
    PulseOperations.remove_allocation_attr (fst access) astate |> Basic.ok_continue
end

let insertion_into_collection_key_and_value (value, value_hist) (key, key_hist) ~desc :
    model_no_non_disj =
 fun {path; location} astate ->
  let event = Hist.call_event path location desc in
  let<*> astate, _ =
    PulseOperations.eval_access path ~must_be_valid_reason:InsertionIntoCollectionValue Read
      location
      (value, Hist.add_event event value_hist)
      Dereference astate
  in
  let<+> astate, _ =
    PulseOperations.eval_access path ~must_be_valid_reason:InsertionIntoCollectionKey Read location
      (key, Hist.add_event event key_hist)
      Dereference astate
  in
  astate


let insertion_into_collection_key_or_value (value, value_hist) ~value_kind ~desc : model_no_non_disj
    =
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


let insert_object_at (collection, collection_hist) (obj_ptr, obj_hist) (index, _index_hist)
    ?(disallow_nil_obj = true) ~desc : model_no_non_disj =
 fun {path; location} astate ->
  let obj_hist = Hist.add_call path location desc obj_hist in
  let collection_hist = Hist.add_call path location desc collection_hist in
  let<*> astate, obj =
    if disallow_nil_obj then
      PulseOperations.eval_access path
        ~must_be_valid_reason:Invalidation.InsertionIntoCollectionValue Read location
        (obj_ptr, obj_hist) Dereference astate
    else Ok (astate, (obj_ptr, obj_hist))
  in
  let<*> astate, cell =
    GenericArrayBackedCollection.element path location (collection, collection_hist) index astate
  in
  let<+> astate = PulseOperations.write_deref path location ~ref:cell ~obj astate in
  astate


let object_at (collection, collection_hist) (index, _index_hist) ?(implement_nil_messaging = true)
    ~desc : model_no_non_disj =
 fun {path; location; ret= ret_id, _} astate ->
  let object_at_aux astate =
    let<*> astate, cell =
      GenericArrayBackedCollection.element path location (collection, collection_hist) index astate
    in
    let<+> astate, (obj, obj_hist) =
      PulseOperations.eval_access path Read location cell Dereference astate
    in
    let obj_hist = Hist.add_call path location desc obj_hist in
    PulseOperations.write_id ret_id (obj, obj_hist) astate
  in
  if not implement_nil_messaging then object_at_aux astate
  else
    let astate_nil =
      (* nil messaging *)
      let<++> astate = PulseArithmetic.prune_eq_zero collection astate in
      let ret_hist =
        let in_call =
          ValueHistory.sequence
            (ValueHistory.NilMessaging (location, path.PathContext.timestamp))
            collection_hist
        in
        Hist.single_call path ~in_call location desc
      in
      PulseOperations.write_id ret_id (collection, ret_hist) astate
    in
    let astate_not_nil =
      let<**> astate = PulseArithmetic.prune_positive collection astate in
      object_at_aux astate
    in
    List.rev_append astate_nil astate_not_nil


let init_array_backed_with_array (collection, collection_hist) array ~desc : model_no_non_disj =
 fun {path; location; ret= ret_id, _} astate ->
  let<*> astate, copy = PulseOperations.deep_copy ~depth_max:1 path location array astate in
  let collection_hist = Hist.add_call path location desc collection_hist in
  let<*> astate, backing_array =
    PulseOperations.eval_access path Read location (collection, collection_hist)
      GenericArrayBackedCollection.access astate
  in
  let<+> astate = PulseOperations.write_deref path location ~ref:backing_array ~obj:copy astate in
  PulseOperations.write_id ret_id (collection, collection_hist) astate


let init_array_backed_with_modelled_array collection other ~desc : model_no_non_disj =
 fun ({path; location} as model_data) astate ->
  let<*> astate, backing_array =
    GenericArrayBackedCollection.eval path Read location other astate
  in
  init_array_backed_with_array collection backing_array ~desc model_data astate


let create_array_backed_with_modelled_array other ~desc : model_no_non_disj =
 fun ({path; location} as model_data) astate ->
  let ret_val = AbstractValue.mk_fresh () in
  let ret_hist = Hist.single_call path location desc in
  init_array_backed_with_modelled_array (ret_val, ret_hist) other ~desc model_data astate


let read_from_collection (key, _key_hist) ~desc : model_no_non_disj =
 fun {path; location; ret= ret_id, _} astate ->
  let event = Hist.call_event path location desc in
  let ret_val = AbstractValue.mk_fresh () in
  let astate_nil =
    let<**> astate = PulseArithmetic.prune_eq_zero key astate in
    let<++> astate = PulseArithmetic.and_eq_int ret_val IntLit.zero astate in
    PulseOperations.write_id ret_id (ret_val, Hist.single_event event) astate
  in
  let astate_not_nil =
    let<++> astate = PulseArithmetic.prune_positive key astate in
    PulseOperations.write_id ret_id (ret_val, Hist.single_event event) astate
  in
  List.rev_append astate_nil astate_not_nil


(* NOTE: assume that this is always called with [freeWhenDone] being [YES] *)
let init_with_bytes_free_when_done bytes : model_no_non_disj =
 fun {path; location; ret= ret_id, _; callee_procname} astate ->
  PulseOperations.havoc_id ret_id
    (Hist.single_call path location (Procname.to_string callee_procname))
    astate
  |> PulseOperations.remove_allocation_attr (fst bytes)
  |> Basic.ok_continue


let alloc_no_fail size : model_no_non_disj =
 fun model_data astate ->
  (* NOTE: technically this doesn't initialize the result but we haven't modelled initialization so
     assume the object is initialized after [init] for now *)
  let<++> astate =
    Basic.alloc_not_null ~initialize:true ~desc:"alloc" ObjCAlloc (Some size) model_data astate
  in
  astate


let set_ref_count (value, value_hist) (ref_count, ref_count_hist) : model_no_non_disj =
 fun {path; location} astate ->
  let<+> astate =
    PulseOperations.write_field path location ~ref:(value, value_hist)
      PulseOperations.ModeledField.internal_ref_count ~obj:(ref_count, ref_count_hist) astate
  in
  astate


let get_ref_count (value, value_hist) : model_no_non_disj =
 fun {path; location; ret= ret_id, _} astate ->
  let<+> astate, (ret_val, ret_hist) =
    PulseOperations.eval_access path Read location (value, value_hist)
      (FieldAccess PulseOperations.ModeledField.internal_ref_count) astate
  in
  PulseOperations.write_id ret_id (ret_val, ret_hist) astate


let construct_string ((value, value_hist) as char_array) : model_no_non_disj =
 fun {path; location; ret= ret_id, _} astate ->
  let desc = "NSString.stringWithUTF8String:" in
  let event = Hist.call_event path location desc in
  let<*> astate, _ =
    PulseOperations.eval_access path Read location
      (value, Hist.add_event event value_hist)
      Dereference astate
  in
  let string = AbstractValue.mk_fresh () in
  let string_hist = Hist.single_call path location desc in
  let<+> astate =
    PulseOperations.write_field path location ~ref:(string, string_hist)
      PulseOperations.ModeledField.internal_string ~obj:char_array astate
  in
  PulseOperations.write_id ret_id (string, string_hist) astate


let check_arg_not_nil (value, value_hist) ~desc : model_no_non_disj =
 fun {path; location; ret= ret_id, _} astate ->
  let event = Hist.call_event path location desc in
  let<*> astate, _ =
    PulseOperations.eval_access path
      ~must_be_valid_reason:(NullArgumentWhereNonNullExpected (CallEvent.Model desc, None))
      Read location
      (value, Hist.add_event event value_hist)
      Dereference astate
  in
  let ret_val = AbstractValue.mk_fresh () in
  let<++> astate = PulseArithmetic.prune_positive ret_val astate in
  PulseOperations.write_id ret_id (ret_val, Hist.single_call path location desc) astate


let call_objc_block FuncArg.{arg_payload= block_ptr_hist; typ} actuals : model =
 fun {path; analysis_data; location; ret= (ret_id, _) as ret} astate non_disj ->
  let block = fst block_ptr_hist in
  let callee_proc_name_opt =
    match PulseArithmetic.get_dynamic_type block astate with
    | Some {typ= {desc= Typ.Tstruct (ObjcBlock bsig)}} ->
        Some (Procname.Block bsig)
    | _ ->
        None
  in
  match callee_proc_name_opt with
  | Some callee_proc_name ->
      let actuals =
        (block_ptr_hist, typ)
        :: List.map actuals ~f:(fun FuncArg.{arg_payload; typ} -> (arg_payload, typ))
      in
      let astate, non_disj, _, _ =
        PulseCallOperations.call analysis_data path location callee_proc_name ~ret ~actuals
          ~formals_opt:None ResolvedCall
          {CallFlags.default with cf_is_objc_block= true}
          astate non_disj
      in
      (astate, non_disj)
  | _ ->
      (* we don't know what procname this block resolves to *)
      let res =
        (* dereference call expression to catch nil issues *)
        let<+> astate, _ =
          PulseOperations.eval_access path Read location ~must_be_valid_reason:BlockCall
            block_ptr_hist Dereference astate
        in
        let desc = Procname.to_string BuiltinDecl.__call_objc_block in
        let hist = Hist.single_event (Hist.call_event path location desc) in
        let astate = PulseOperations.havoc_id ret_id hist astate in
        let astate = AbductiveDomain.add_need_dynamic_type_specialization block astate in
        let astate =
          let unknown_effect = Attribute.UnknownEffect (Model desc, hist) in
          List.fold actuals ~init:astate ~f:(fun acc FuncArg.{arg_payload= actual, _} ->
              AddressAttributes.add_one actual unknown_effect acc )
        in
        astate
      in
      (res, non_disj)


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
  |> List.map ~f:(fun matcher ->
      matcher
      |> ProcnameDispatcher.Call.contramap_arg_payload ~f:ValueOrigin.addr_hist
      |> ProcnameDispatcher.Call.map_matcher ~f:lift_model )


(* Model the "framework retains a block" shape for ObjC framework APIs:
   a fresh holder strongly retains [captured_env] (the heap-copied ObjC
   block produced by [_Block_copy]) and is returned to the caller.
   Without this model Pulse never establishes the holder->captured_env
   edge, so retain cycles closing through the block's captured `self`
   are missed (Timer / NotificationCenter / AVPlayer / ...). The body
   mirrors [PulseModelsSwift.register_closure_holder] (intentionally
   duplicated: matchers and bodies are colocated per Pulse's
   per-language module convention). *)
let block_holder captured_env : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let holder_class = Typ.SwiftClass SwiftClassName.swift_alloc_unknown_type in
  (* `~is_weak:false` makes the field a Strong access so PulseRefCounting
     classifies the holder->captured_env edge correctly when the
     retain-cycle checker traverses it; otherwise the field has no weak/
     strong info and the cycle's access type is reported as Unknown. *)
  let field = Fieldname.make ~is_weak:false holder_class "captured_env" in
  let* holder = fresh () in
  let* () = and_positive holder in
  let* () = allocation SwiftAlloc holder in
  let* () = and_dynamic_type_is holder (Typ.mk_struct holder_class) in
  let* () = store_field ~ref:holder field captured_env in
  assign_ret holder


(* Setter-style model for ObjC framework methods that take a block but don't
   return a token the user stashes — instead the receiver itself retains the
   block (e.g. `-[NSOperationQueue addOperationWithBlock:]`). The user already
   holds the receiver via some property on `self`; the model attaches the
   heap-copied block as a strong field on the receiver so the cycle
   `self -> _<receiver_property> -> receiver -> __infer_attached_block -> block
    -> __infer_tuple_field_1 -> field_1 -> self` becomes visible. The block
   value already carries the captured-self path (preserved by the `_Block_copy`
   model from D104407063). *)
let attach_block_to_receiver receiver block : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  store_field ~deref:false ~ref:receiver PulseOperations.ModeledField.objc_attached_block block


(* Return a fresh allocated value with a stamped ObjC dynamic type. Used to
   model unmodelled ObjC singleton accessors (e.g. `+[NSNotificationCenter
   defaultCenter]`) so that subsequent instance-method calls on the result
   resolve through [PulseModelsSwift.objc_msgSend]'s dynamic dispatch
   instead of falling back to [unknown]. *)
let fresh_with_objc_type class_name : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let typ = Typ.mk (Typ.Tstruct (Typ.Name.Objc.from_string class_name)) in
  let* v = fresh () in
  let* () = and_positive v in
  let* () = allocation ObjCAlloc v in
  let* () = and_dynamic_type_is v typ in
  assign_ret v


(* Model for `-[NSObject copy]` and its overrides on every subclass. The
   returned object is a fresh, owned (+1 retain count) instance whose
   per-field state mirrors the receiver's at the call site. We use
   [PulseOperations.shallow_copy] to allocate a new address that shares
   the receiver's post-cell (field bindings + per-address attributes,
   including taint and dynamic-type constraints) -- semantically the
   same shape as [-[NSObject copyWithZone:]]'s default member-wise
   copy, and what most [NSCopying] conformers implement. The copy is
   then stamped [ObjCAlloc] so ARC ownership and leak tracking work.

   Crucially, taint and value attributes on the receiver flow through
   to the copy -- which is what Pulse's unknown-callee fallback also
   does, so removing the unmodelled-callee event does not regress
   any taint analysis built on top of [-copy]. *)
let nsobject_copy receiver : model_no_non_disj =
 fun {path; location; ret= ret_id, _} astate ->
  let event = Hist.call_event path location "NSObject.copy" in
  let<*> astate, obj_copy =
    PulseOperations.shallow_copy ~ask_specialization:true path location receiver astate
  in
  let copy_addr = fst obj_copy in
  let copy_hist = Hist.add_event event (snd obj_copy) in
  let astate = PulseOperations.allocate ObjCAlloc location copy_addr astate in
  let astate = PulseOperations.write_id ret_id (copy_addr, copy_hist) astate in
  let<++> astate = PulseArithmetic.and_positive copy_addr astate in
  astate


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
  let class_match_name name (_tenv, proc_name) _ =
    Procname.get_objc_class_name proc_name |> Option.exists ~f:(String.equal name)
  in
  let map_context_tenv f (x, _) = f x in
  ( [ -"dispatch_sync" <>$ any_arg $+ capt_arg $++$--> call_objc_block
    ; -"dispatch_async" <>$ any_arg $+ capt_arg $++$--> call_objc_block
    ; -"dispatch_once" <>$ any_arg $+ capt_arg $++$--> call_objc_block
    ; +map_context_tenv (PatternMatch.ObjectiveC.implements "UITraitCollection")
      &:: "performAsCurrentTraitCollection:" $ capt_arg $++$--> call_objc_block
    ; +BuiltinDecl.(match_builtin __call_objc_block) $ capt_arg $++$--> call_objc_block
      (* NSTimer.scheduledTimerWithTimeInterval:repeats:block: retains the
         block on the returned Timer; the user typically stores the Timer
         on `self`, closing a `self -> timer -> block -> captured self`
         cycle. The model establishes the timer->block strong edge so
         PulseRefCounting can see the cycle. *)
    ; +class_match_name "NSTimer"
      &:: "scheduledTimerWithTimeInterval:repeats:block:" <>$ any_arg $+ any_arg $+ capt_arg_payload
      $--> block_holder
      (* +[NSNotificationCenter defaultCenter] returns the singleton.
         Without a typed return value, [PulseModelsSwift.objc_msgSend]'s
         dynamic dispatch can't resolve subsequent instance-method calls
         on [.default], so retain-cycle detection through
         `addObserverForName:object:queue:usingBlock:` never fires. The
         model returns a fresh value with dynamic type NSNotificationCenter
         so the dispatch resolves. *)
    ; +class_match_name "NSNotificationCenter"
      &:: "defaultCenter"
      <>$$--> fresh_with_objc_type "NSNotificationCenter"
      (* -[NSNotificationCenter addObserverForName:object:queue:usingBlock:]
         retains the block on the returned observer token; the token is
         typically stored on `self.observer`, closing the same shape of
         cycle as NSTimer above. The call lands here via [objc_msgSend]'s
         dynamic dispatch in [PulseModelsSwift], which constructs the
         instance-method procname `NSNotificationCenter.addObserverFor...`. *)
    ; +class_match_name "NSNotificationCenter"
      &:: "addObserverForName:object:queue:usingBlock:" <>$ any_arg $+ any_arg $+ any_arg $+ any_arg
      $+ capt_arg_payload $--> block_holder
      (* Foundation/UIKit singleton accessors: same shape as the
         NSNotificationCenter.defaultCenter model above. Without a typed
         return value, [PulseModelsSwift.objc_msgSend]'s dynamic dispatch
         can't resolve subsequent instance-method calls on the singleton
         (it falls back to [unknown]), so any future ObjC instance-method
         model on these receivers would silently never fire. Pure
         infrastructure: typing each accessor unblocks downstream
         matchers as they're added (e.g. observer/timer/operation
         registration through these singletons). *)
    ; +class_match_name "UIApplication"
      &:: "sharedApplication"
      <>$$--> fresh_with_objc_type "UIApplication"
    ; +class_match_name "NSFileManager"
      &:: "defaultManager"
      <>$$--> fresh_with_objc_type "NSFileManager"
    ; +class_match_name "NSUserDefaults"
      &:: "standardUserDefaults"
      <>$$--> fresh_with_objc_type "NSUserDefaults"
    ; +class_match_name "NSBundle" &:: "mainBundle" <>$$--> fresh_with_objc_type "NSBundle"
    ; +class_match_name "NSURLSession"
      &:: "sharedSession"
      <>$$--> fresh_with_objc_type "NSURLSession"
      (* -[NSURLSession dataTaskWithURL:completionHandler:] retains the
         completion block on the returned `URLSessionDataTask`; the task is
         typically stored on `self.task`, closing the same shape of cycle as
         NSTimer / NotificationCenter above. The call lands here via
         [objc_msgSend]'s dynamic dispatch in [PulseModelsSwift], which
         constructs the instance-method procname `NSURLSession.dataTaskWith...`. *)
    ; +class_match_name "NSURLSession"
      &:: "dataTaskWithURL:completionHandler:" <>$ any_arg $+ any_arg $+ capt_arg_payload
      $--> block_holder
      (* Sibling overloads of `dataTaskWithURL:completionHandler:` — same shape
         (block stored on the returned task, task stored on self), just
         different selectors and payload-arg type. *)
    ; +class_match_name "NSURLSession"
      &:: "dataTaskWithRequest:completionHandler:" <>$ any_arg $+ any_arg $+ capt_arg_payload
      $--> block_holder
    ; +class_match_name "NSURLSession"
      &:: "downloadTaskWithURL:completionHandler:" <>$ any_arg $+ any_arg $+ capt_arg_payload
      $--> block_holder
    ; +class_match_name "NSURLSession"
      &:: "downloadTaskWithRequest:completionHandler:" <>$ any_arg $+ any_arg $+ capt_arg_payload
      $--> block_holder
    ; +class_match_name "NSProcessInfo"
      &:: "processInfo"
      <>$$--> fresh_with_objc_type "NSProcessInfo"
    ; +class_match_name "NSRunLoop" &:: "currentRunLoop" <>$$--> fresh_with_objc_type "NSRunLoop"
    ; +class_match_name "NSRunLoop" &:: "mainRunLoop" <>$$--> fresh_with_objc_type "NSRunLoop"
    ; +class_match_name "NSOperationQueue"
      &:: "mainQueue"
      <>$$--> fresh_with_objc_type "NSOperationQueue"
      (* `-[NSOperationQueue addOperationWithBlock:]` retains the block on the
         queue while the operation is queued; the queue is typically a property
         of `self` (e.g. `let queue = OperationQueue()`), and the block captures
         self, closing
         `self -> _queue -> queue -> __infer_attached_block -> block
          -> captured self -> self`. *)
    ; +class_match_name "NSOperationQueue"
      &:: "addOperationWithBlock:" <>$ capt_arg_payload $+ capt_arg_payload
      $--> attach_block_to_receiver ]
  |> List.map ~f:(ProcnameDispatcher.Call.contramap_arg_payload ~f:ValueOrigin.addr_hist) )
  @
  let objc_only_name name =
    ~+(fun (_tenv, proc_name) n -> String.equal n name && Procname.is_objc_method proc_name)
  in
  [ objc_only_name "init" <>$ capt_arg_payload $+...$--> Basic.id_first_arg ~desc:"NSObject.init"
  ; objc_only_name "initWithFrame:" <>$ capt_arg_payload
    $+...$--> Basic.id_first_arg ~desc:"NSObject.initWithFrame:"
  ; objc_only_name "initWithCoder:" <>$ capt_arg_payload
    $+...$--> Basic.id_first_arg ~desc:"NSObject.initWithCoder:"
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
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSAttributedString")
    &:: "initWithString:" <>$ any_arg $+ capt_arg_payload
    $--> check_arg_not_nil ~desc:"NSAttributedString.initWithString:"
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
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSArray")
    &:: "arrayWithArray:" <>$ capt_arg_payload
    $--> create_array_backed_with_modelled_array ~desc:"NSArray.arrayWithArray:"
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSArray")
    &:: "initWithArray:" <>$ capt_arg_payload $+ capt_arg_payload
    $--> init_array_backed_with_modelled_array ~desc:"NSArray.initWithArray:"
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSMutableArray")
    &:: "addObject:" <>$ any_arg $+ capt_arg_payload
    $--> insertion_into_collection_key_or_value ~value_kind:`Value ~desc:"NSMutableArray.addObject:"
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSMutableArray")
    &:: "insertObject:atIndex:" <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg_payload
    $--> insert_object_at ~disallow_nil_obj:true ~desc:"NSMutableArray.insertObject:atIndex:"
  ; ( +map_context_tenv (PatternMatch.ObjectiveC.implements "NSMutableArray")
    &:: "replaceObjectAtIndex:withObject:" <>$ capt_arg_payload $+ capt_arg_payload
    $+ capt_arg_payload
    $--> fun collection index obj ->
    insert_object_at collection obj index ~disallow_nil_obj:true
      ~desc:"NSMutableArray.replaceObjectAtIndex:withObject:" )
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSMutableArray")
    &:: "setObject:atIndexedSubscript:" <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg_payload
    $--> insert_object_at ~disallow_nil_obj:true
           ~desc:"NSMutableArray.setObject:atIndexedSubscript:"
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSArray")
    &:: "objectAtIndex:" <>$ capt_arg_payload $+ capt_arg_payload
    $--> object_at ~implement_nil_messaging:true ~desc:"NSArray.objectAtIndex:"
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSArray")
    &:: "objectAtIndexedSubscript:" <>$ capt_arg_payload $+ capt_arg_payload
    $--> object_at ~implement_nil_messaging:true ~desc:"NSArray.objectAtIndexedSubscript:"
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
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "UIViewController")
    &:: "initWithNibName:bundle:" <>$ capt_arg_payload
    $+...$--> Basic.id_first_arg ~desc:"UIViewController.initWithNibName:bundle:"
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "UIView")
    &:: "initWithFrame:" <>$ capt_arg_payload
    $+...$--> Basic.id_first_arg ~desc:"UIView.initWithFrame:"
  ; +map_context_tenv (PatternMatch.ObjectiveC.implements "UIView")
    &:: "initWithCoder:" <>$ capt_arg_payload
    $+...$--> Basic.id_first_arg ~desc:"UIView.initWithCoder:"
    (* catch-all for any CLASS.init *)
  ; +(fun (_tenv, proc_name) _ -> Procname.is_objc_method proc_name)
    &:: "init" <>$ capt_arg_payload
    $+...$--> Basic.id_first_arg ~desc:"NSObject.init"
    (* catch-all for any CLASS.copy on an ObjC instance method. Falls
       through to a shallow-copy model that preserves field bindings,
       attributes, and taint from the receiver. *)
  ; +(fun (_tenv, proc_name) _ ->
       Procname.is_objc_method proc_name && Procname.is_objc_instance_method proc_name )
    &:: "copy" <>$ capt_arg_payload $--> nsobject_copy ]
  |> List.map ~f:(fun matcher ->
      matcher
      |> ProcnameDispatcher.Call.contramap_arg_payload ~f:ValueOrigin.addr_hist
      |> ProcnameDispatcher.Call.map_matcher ~f:lift_model )
