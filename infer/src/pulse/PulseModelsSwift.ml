(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module IRAttributes = Attributes
open PulseBasicInterface
module DSL = PulseModelsDSL
open PulseModelsImport

let make_swift_bool b : DSL.aval DSL.model_monad =
  let open DSL.Syntax in
  (* Translates an OCaml boolean into an Infer SMT integer (1 or 0) *)
  int (if b then 1 else 0)


let unknown _args () : unit DSL.model_monad =
  let open DSL.Syntax in
  let* res = fresh () in
  assign_ret res


let function_ptr_call args () : unit DSL.model_monad =
  (* TODO: implement this *)
  unknown args ()


let closure_call orig_args () : unit DSL.model_monad =
  let open DSL.Syntax in
  match orig_args with
  | proc_name_arg :: args -> (
      let* arg_dynamic_type_data = get_dynamic_type ~ask_specialization:true proc_name_arg in
      match arg_dynamic_type_data with
      | Some {Formula.typ= {desc= Typ.Tstruct (SwiftClosure csig)}} -> (
          let proc_name = Procname.Swift (SwiftProcname.mk_function csig) in
          let args = List.mapi ~f:(fun i arg -> (Format.sprintf "arg_%d" i, arg)) args in
          Logging.d_printfln "calling %a with args = %a" Procname.pp proc_name
            (Pp.comma_seq (Pp.pair ~fst:String.pp ~snd:DSL.pp_aval))
            args ;
          let* res_opt = swift_call proc_name args |> is_unsat in
          match res_opt with
          | Some res ->
              assign_ret res
          | None -> (
              (* if the call is unsat, it could be because of mismatched arguments, because the specialised
                 closure doesn't capture any variables, and so the captured argument is not needed. In this
                 case, we try the call again, by removing the last argument, which will be null. *)
              let args = List.take args (List.length args - 1) in
              Logging.d_printfln "calling %a again with args = %a" Procname.pp proc_name
                (Pp.comma_seq (Pp.pair ~fst:String.pp ~snd:DSL.pp_aval))
                args ;
              let* res_opt = swift_call proc_name args |> is_unsat in
              match res_opt with Some res -> assign_ret res | None -> unreachable ) )
      | _ ->
          Logging.d_printfln "no method name found for closure %a" DSL.pp_aval proc_name_arg ;
          function_ptr_call args () )
  | [] ->
      function_ptr_call orig_args ()


let dynamic_call arg orig_args () : unit DSL.model_monad =
  let open DSL.Syntax in
  let dynamic_call_with_type name offset self args =
    let args = List.mapi ~f:(fun i arg -> (Format.sprintf "arg_%d" i, arg)) (args @ [self]) in
    let* proc_name = tenv_resolve_method_with_offset name offset in
    match proc_name with
    | Some proc_name ->
        Logging.d_printfln "calling %a with args = %a" Procname.pp proc_name
          (Pp.comma_seq (Pp.pair ~fst:String.pp ~snd:DSL.pp_aval))
          args ;
        let* res = swift_call proc_name args in
        assign_ret res
    | None ->
        Logging.d_printfln "proc_name not found for name = %a and offset = %a" Typ.Name.pp name
          Int.pp offset ;
        unknown args ()
  in
  let* offset_opt = as_constant_int arg in
  match (offset_opt, List.rev orig_args) with
  | Some offset, self :: actuals -> (
      let args = List.rev actuals in
      let* arg_dynamic_type_data = get_dynamic_type ~ask_specialization:true self in
      match arg_dynamic_type_data with
      | Some {Formula.typ= {desc= Tstruct name}} ->
          dynamic_call_with_type name offset self args
      | _ -> (
          let* arg_static_type = get_static_type self in
          match arg_static_type with
          | Some (Typ.SwiftClass class_name as name)
            when not (SwiftClassName.equal (SwiftClassName.of_string "ptr_elt") class_name) ->
              dynamic_call_with_type name offset self args
          | _ ->
              Logging.d_printfln
                "method to call not found, no dynamic or static type found for %a, returning a \
                 fresh value"
                DSL.pp_aval self ;
              unknown args () ) )
  | _, _ ->
      closure_call (arg :: orig_args) ()


let check_missing_nullability proc_name : unit DSL.model_monad =
  let open DSL.Syntax in
  let* {location; call_flags} = get_data in
  match IRAttributes.load proc_name with
  | Some attrs when Typ.is_pointer attrs.ret_type ->
      (* Combine the callee's procdesc-level [ret_annots] with any per-call-site
         annotations the frontend recovered for this call (e.g. Swift recognising
         that the caller treats the result as [Optional<T>] even when the ObjC
         method itself is unannotated). *)
      let combined_annots = call_flags.CallFlags.cf_caller_ret_annots @ attrs.ret_annots in
      if
        (not (Annotations.ia_is_nullable combined_annots))
        && (not (Annotations.ia_is_nonnull combined_annots))
        && not call_flags.CallFlags.cf_return_null_checked
      then
        if
          SwiftObjCNullabilityIssue.should_report_at location
          && not (SwiftObjCNullabilityIssue.is_system_framework_callee attrs)
        then report (PulseDiagnostic.MissingNullabilityAnnotation {callee= proc_name; location})
        else ret ()
      else ret ()
  | _ ->
      ret ()


let objc_msgSend receiver selector_arg actual_args () : unit DSL.model_monad =
  let open DSL.Syntax in
  (* Helper to build and call the resolved ObjC method *)
  let dispatch_with_type class_name selector receiver args =
    let method_args =
      List.mapi ~f:(fun i arg -> (Format.sprintf "arg_%d" i, arg)) (receiver :: args)
    in
    (* Construct the ObjC Procname: -[class_name selector] *)
    let proc_name =
      Procname.ObjC_Cpp
        (Procname.ObjC_Cpp.make class_name selector ObjCInstanceMethod NoTemplate [])
    in
    Logging.d_printfln "Dynamic ObjC dispatch: calling %a" Procname.pp proc_name ;
    let* () = check_missing_nullability proc_name in
    let* res = swift_call proc_name method_args in
    assign_ret res
  in
  (* 1. Extract the selector string (e.g., "getBatteryStatus") *)
  let* selector_opt = as_constant_string selector_arg in
  match selector_opt with
  | Some selector -> (
      (* 2. Get the dynamic type of the receiver (stamped during init) *)
      let* dynamic_type_data = get_dynamic_type ~ask_specialization:true receiver in
      match dynamic_type_data with
      | Some {Formula.typ= {desc= Tstruct name}} ->
          dispatch_with_type name selector receiver actual_args
      | _ -> (
          (* 3. Fallback to static type if dynamic type is missing *)
          let* static_type = get_static_type receiver in
          match static_type with
          | Some (Typ.SwiftClass _ as name) ->
              dispatch_with_type name selector receiver actual_args
          | _ ->
              Logging.d_printfln "ObjC dispatch failed: no type for %a" DSL.pp_aval receiver ;
              unknown actual_args () ) )
  | None ->
      Logging.d_printfln "ObjC dispatch failed: selector is not a constant string" ;
      unknown actual_args ()


let swift_dynamic_type arg1 arg2 () : unit DSL.model_monad =
  let open DSL.Syntax in
  let* arg1_dynamic_type_data = get_dynamic_type ~ask_specialization:true arg1 in
  match arg1_dynamic_type_data with
  | None ->
      unknown [arg1; arg2] ()
  | Some {Formula.typ= arg1_dynamic_type} ->
      let* res = fresh () in
      let* () = and_dynamic_type_is res arg1_dynamic_type in
      assign_ret res


let derived_enum_equals arg1 arg2 () : unit DSL.model_monad =
  let open DSL.Syntax in
  let* res = binop Binop.Eq arg1 arg2 in
  assign_ret res


let metadata_equals arg1 arg2 () : unit DSL.model_monad =
  let open DSL.Syntax in
  let* arg1_dynamic_type_data = get_dynamic_type ~ask_specialization:true arg1 in
  let* arg2_dynamic_type_data = get_dynamic_type ~ask_specialization:true arg2 in
  match (arg1_dynamic_type_data, arg2_dynamic_type_data) with
  | Some {Formula.typ= typ1}, Some {Formula.typ= typ2} ->
      (* We know both types, so we directly compute the boolean result and assign it *)
      let* res = make_swift_bool (Typ.equal typ1 typ2) in
      assign_ret res
  | _ ->
      (* We lack dynamic type info for one or both, return an unconstrained value *)
      let* unknown_bool = fresh () in
      assign_ret unknown_bool


(* Model the "framework registers a handler" shape: a function takes a
   closure (split by Swift's calling convention into a function-pointer
   and its captured-environment) and returns an opaque holder that
   strongly retains the closure. Used for canonical Swift framework
   APIs (Timer / NotificationCenter / Combine sink /
   DispatchSourceTimer) that store a closure on a returned object the
   caller stashes on `self`. Without this model Pulse never establishes
   the holder->captured_env edge, so retain cycles closing through the
   captured `self` are missed. The body mirrors [PulseModelsObjC.block_holder]
   (intentionally duplicated: matchers and bodies are colocated per
   Pulse's per-language module convention). *)
let register_closure_holder _callback captured_env : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let holder_class = Typ.SwiftClass SwiftClassName.swift_alloc_unknown_type in
  let field = Fieldname.make ~is_weak:false holder_class "captured_env" in
  let* holder = fresh () in
  let* () = and_positive holder in
  let* () = allocation SwiftAlloc holder in
  let* () = and_dynamic_type_is holder (Typ.mk_struct holder_class) in
  let* () = store_field ~ref:holder field captured_env in
  assign_ret holder


let block_copy_identity block : model =
  let open DSL.Syntax in
  start_model @@ fun () -> assign_ret block


let block_release_skip _block : model =
  let open DSL.Syntax in
  start_model @@ fun () -> ret ()


let alloc size_exp () : unit DSL.model_monad =
  let open DSL.Syntax in
  (* 1. Extract the exact type directly from the sizeof AST *)
  let typ =
    match size_exp with
    | Exp.Sizeof {typ} ->
        if Typ.is_pointer typ then Typ.strip_ptr typ else typ
    | _ ->
        Logging.die InternalError "Expected sizeof expression in swift_allocObject"
  in
  (* 2. Create the main object memory space *)
  let* allocated_obj = fresh () in
  (* 3. Mark it as an allocated, non-null pointer *)
  let* () = and_positive allocated_obj in
  let* () = allocation SwiftAlloc allocated_obj in
  let* () = and_dynamic_type_is allocated_obj typ in
  match typ.desc with
  | Tstruct (SwiftClass name) when SwiftClassName.equal name SwiftClassName.swift_alloc_unknown_type
    ->
      (* Placeholder type emitted by the frontend when [swift_allocObject] was called
         outside a Swift-class context and the layout could not be recovered.  Skip the
         isa-pointer / offset-0 store: we don't know the object's layout, so we don't
         invent one.  The address is still tracked with [SwiftAlloc] provenance so
         PulseRefCounting can classify edges through it. *)
      Stats.incr_pulse_swift_alloc_unknown_type () ;
      assign_ret allocated_obj
  | _ ->
      (* 4. Create the symbolic metatype/vtable pointer (the isa pointer) *)
      let* meta_addr = fresh ~more:"swift_isa_vtable_pointer" () in
      let* () = and_dynamic_type_is meta_addr typ in
      (* 5. Emulate Swift ABI: write the metatype to the base address (offset 0) *)
      let* () = store ~ref:allocated_obj meta_addr in
      (* 6. Bind the fully constructed object to the return variable *)
      assign_ret allocated_obj


let objc_alloc_from_swift size_exp class_ptr () : unit DSL.model_monad =
  let open DSL.Syntax in
  (* 1. Extract the static fallback type from the LLVM sizeof AST *)
  let static_typ_opt =
    match size_exp with
    | Exp.Sizeof {typ} ->
        Some (if Typ.is_pointer typ then Typ.strip_ptr typ else typ)
    | _ ->
        None
  in
  (* 2. Check if Pulse tracked a dynamic type on the class pointer argument *)
  let* dynamic_type_data_opt = get_dynamic_type ~ask_specialization:true class_ptr in
  (* 3. Determine the final type to allocate: Dynamic > Static > Fallback (NSObject) *)
  let alloc_typ =
    match (dynamic_type_data_opt, static_typ_opt) with
    | Some {Formula.typ= dyn_typ}, _ ->
        dyn_typ
    | None, Some stat_typ ->
        stat_typ
    | None, None ->
        Typ.mk (Typ.Tstruct (Typ.Name.Objc.from_string "NSObject"))
  in
  (* 4. Create the main object memory space *)
  let* allocated_obj = fresh () in
  let* () = and_positive allocated_obj in
  (* 5. Allocate as an ObjC object *)
  let* () = allocation ObjCAlloc allocated_obj in
  (* 6. Bind the strictly resolved type *)
  let* () = and_dynamic_type_is allocated_obj alloc_typ in
  assign_ret allocated_obj


let builtins_matcher builtin (func_args : ValueOrigin.t FuncArg.t list) :
    unit -> unit DSL.model_monad =
  let builtin_s = SwiftProcname.show_builtin builtin in
  let args = List.map func_args ~f:(fun fa -> FuncArg.arg_payload fa |> ValueOrigin.addr_hist) in
  match (builtin : SwiftProcname.builtin) with
  | NonDet ->
      unknown args
  | InitTuple ->
      unknown args
  | DynamicCall -> (
    match args with arg :: args -> dynamic_call arg args | [] -> unknown args )
  | DerivedEnumEquals -> (
    (* We are modelling the case for simple enums where there are exactly two args.
         In the case of complex enums there can be more args, but we are not modelling
         that yet. Be defensive on the lower bound too: malformed Textual can produce
         a [__derived_enum_equals] call with fewer than 2 args (observed in production
         on some Swift [isEqual] translations); fall back to [unknown] in that case
         instead of [die]ing and abandoning the whole procedure analysis. *)
    match args with
    | [arg1; arg2] ->
        derived_enum_equals arg1 arg2
    | _ ->
        unknown args )
  | ObjcMsgSend ->
      let receiver, selector, actual_args =
        ProcnameDispatcherBuiltins.expect_at_least_2_args args builtin_s
      in
      objc_msgSend receiver selector actual_args
  | ObjcMsgSendSuper2 ->
      unknown args
  | ObjcAllocFromSwift -> (
    match (func_args, args) with
    | size :: _, _ :: class_ptr :: _ ->
        objc_alloc_from_swift (FuncArg.exp size) class_ptr
    | _ ->
        unknown args )
  | SwiftAlloc -> (
    match func_args with size :: _ -> alloc (FuncArg.exp size) | _ -> unknown args )
  | Memcpy ->
      unknown args
  | MetadataEquals ->
      let arg1, arg2, _ = ProcnameDispatcherBuiltins.expect_at_least_2_args args builtin_s in
      metadata_equals arg1 arg2
  | SwiftGetDynamicType ->
      let arg1, arg2, _ = ProcnameDispatcherBuiltins.expect_at_least_2_args args builtin_s in
      swift_dynamic_type arg1 arg2


(* Setter-style model for `DispatchSourceProtocol.{setEventHandler, setCancelHandler}
   (qos:flags:handler:)`. At the SIL level the 5-arg call is
   `(_, _, block, _, receiver)`:
     - args 0,1: type-introspection helpers (witness/metatype)
     - arg 2:    the heap-copied closure block (post `_Block_copy`)
     - arg 3:    `swift_getObjectType(receiver)`
     - arg 4:    the receiver (the dispatch source itself, returned by makeTimerSource)
   We capture arg2 and arg4, stamp receiver as OS_dispatch_source ObjcClass so
   PulseRetainCycleChecker treats it as ref-counted, and stash the block as a
   strong field on receiver under [field]. The closure value already carries
   the captured-self path via `__infer_tuple_field_1.field_1` (preserved by the
   `_Block_copy` model). *)
let dispatch_source_set_handler ~field receiver block : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let recv_class = Typ.Name.Objc.from_string "OS_dispatch_source" in
  let* () = and_dynamic_type_is receiver (Typ.mk_struct recv_class) in
  store_field ~deref:false ~ref:receiver field block


let dispatch_source_set_event_handler block receiver =
  dispatch_source_set_handler ~field:PulseOperations.ModeledField.swift_event_handler receiver block


let dispatch_source_set_cancel_handler block receiver =
  dispatch_source_set_handler ~field:PulseOperations.ModeledField.swift_cancel_handler receiver
    block


let is_dispatch_source_set_event_handler _ n =
  String.is_substring n ~substring:"OS_dispatch_source"
  && String.is_substring n ~substring:"setEventHandler"


let is_dispatch_source_set_cancel_handler _ n =
  String.is_substring n ~substring:"OS_dispatch_source"
  && String.is_substring n ~substring:"setCancelHandler"


let is_dispatch_source_state_setter _ n =
  String.is_substring n ~substring:"OS_dispatch_source"
  && (String.is_substring n ~substring:"resume" || String.is_substring n ~substring:"schedule")


(* No-op for Dispatch source state-setters and the type-introspection runtime
   helper. Without these, Pulse treats them as unknown calls and havocs their
   arguments — clobbering the `__infer_event_handler` strong edge that
   [dispatch_source_set_handler] just established on the source, which breaks
   the retain-cycle traversal at the subsequent `self.timer = source` store. *)
let skip_with_fresh_ret : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* v = fresh () in
  assign_ret v


(* Swift KVO closure form: `NSObject.observe(_:options:changeHandler:)`
   (Foundation extension on `_KeyValueCodingAndObserving`). The 6-arg call
   site lays out as
   `observe(keypath, options, callback, captured_env, witness, subject)`,
   where `captured_env` arrives directly typed as the calling-proc's `self`
   (no `_Block_copy` wrapping; this is a Swift closure, not an ObjC block).
   The call returns an `NSKeyValueObservation` token that the user almost
   always stashes on `self` (otherwise observation stops on token-deinit),
   closing `self -> _observation -> token -> _captured_env -> self`. The
   existing [register_closure_holder] body fits this shape directly. *)
let is_kvo_observe _ n =
  String.is_substring n ~substring:"_KeyValueCodingAndObserving"
  && String.is_substring n ~substring:"observe"


(* Combine `Publisher.sink(receiveValue:)` (Failure == Never overload). The 5-arg
   call site lays out as `sink(callback, captured_env, witness, witness, publisher)`,
   where `captured_env` arrives directly typed as the calling-proc's `self`. The
   call returns an `AnyCancellable` that the user almost always stashes on `self`
   (or in a `Set<AnyCancellable>` on `self`), closing
   `self -> _cancellable -> token -> _captured_env -> self`. The existing
   [register_closure_holder] body fits this shape directly. *)
let is_combine_sink _ n =
  String.is_substring n ~substring:"Combine" && String.is_substring n ~substring:"sink"


(* GCD `DispatchWorkItem.init(qos:flags:block:)`. The 3-arg call site lays out as
   `init(uninit_self, block, metatype)`, where [block] is the heap-copied closure
   value (post `_Block_copy`) carrying the captured-self path via
   `__infer_tuple_field_1.field_1`. The user almost always stashes the resulting
   work item on `self` (otherwise it can't be cancelled / re-scheduled), closing
   `self -> _workItem -> token -> _captured_env -> block -> __infer_tuple_field_1
    -> field_1 -> self`. We substitute the work-item return value with a
   [register_closure_holder]-shaped holder whose `_captured_env` strong field
   points at the block. *)
let is_dispatch_workitem_init _ n =
  String.is_substring n ~substring:"WorkItem" && String.is_substring n ~substring:"block"


let dispatch_workitem_init block = register_closure_holder block block

let is_dispatch_queue_async _ n =
  String.is_substring n ~substring:"OS_dispatch_queue" && String.is_substring n ~substring:"async"


let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  [ -"external_register_handler" <>$ capt_arg_payload $+ capt_arg_payload
    $--> register_closure_holder
  ; -"_Block_copy" <>$ capt_arg_payload $--> block_copy_identity
  ; -"_Block_release" <>$ capt_arg_payload $--> block_release_skip
  ; ~+is_dispatch_source_set_event_handler
    $ any_arg $+ any_arg $+ capt_arg_payload $+ any_arg $+ capt_arg_payload
    $+...$--> dispatch_source_set_event_handler
  ; ~+is_dispatch_source_set_cancel_handler
    $ any_arg $+ any_arg $+ capt_arg_payload $+ any_arg $+ capt_arg_payload
    $+...$--> dispatch_source_set_cancel_handler
  ; ~+is_kvo_observe $ any_arg $+ any_arg $+ capt_arg_payload $+ capt_arg_payload
    $+...$--> register_closure_holder
  ; ~+is_combine_sink $ capt_arg_payload $+ capt_arg_payload $+...$--> register_closure_holder
  ; ~+is_dispatch_workitem_init $ any_arg $+ capt_arg_payload $+...$--> dispatch_workitem_init
  ; ~+is_dispatch_source_state_setter <>--> skip_with_fresh_ret
  ; ~+is_dispatch_queue_async <>--> skip_with_fresh_ret
  ; -"swift_getObjectType" <>--> skip_with_fresh_ret ]
  |> List.map ~f:(fun matcher ->
         matcher |> ProcnameDispatcher.Call.contramap_arg_payload ~f:ValueOrigin.addr_hist )
