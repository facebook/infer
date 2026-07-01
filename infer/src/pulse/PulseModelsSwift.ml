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
      (* [_Null_unspecified] is an explicit annotation, not a missing one: maintainers write it to
         expose the return as a Swift implicitly-unwrapped Optional. Treat it as annotated to keep
         the Pulse-side detector in sync with [SwiftObjCNullabilityChecker]. *)
      if
        (not (Annotations.ia_is_nullable combined_annots))
        && (not (Annotations.ia_is_nonnull combined_annots))
        && (not (Annotations.ia_is_null_unspecified combined_annots))
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


let swift_get_object_type arg : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* arg_dynamic_type_data = get_dynamic_type ~ask_specialization:true arg in
  match arg_dynamic_type_data with
  | None ->
      unknown [arg] ()
  | Some {Formula.typ= arg_dynamic_type} ->
      let* res = fresh () in
      let* () = and_dynamic_type_is res arg_dynamic_type in
      assign_ret res


(* Models for the Swift / Foundation [String] <-> [NSString] bridges.
   At the IR level the bridge calls fall through to Pulse's unknown-callee
   path, leaving the result with no dynamic type — which then defeats
   downstream [objc_msgSend] dispatch on the bridged value.  Returning a
   fresh, non-null value with the appropriate dynamic type is the minimum
   needed to keep the post-bridge value tracking and method dispatch
   working.  Content preservation via [internal_string] (mirroring
   [PulseModelsObjC.construct_string]) is a richer follow-up.

   The matchers use substring predicates (mirroring [is_combine_sink])
   rather than exact mangled-name strings: the underscored protocol-method
   names plus [NSString] are stable across Swift versions and across
   generic specialisations, while the surrounding mangling suffix is not. *)

let is_string_bridge_to_nsstring _ n =
  String.is_substring n ~substring:"_bridgeToObjectiveC"
  && String.is_substring n ~substring:"NSString"


let is_string_bridge_from_nsstring _ n =
  String.is_substring n ~substring:"_unconditionallyBridgeFromObjectiveC"
  && String.is_substring n ~substring:"NSString"


(* Matches the Swift String initialiser used to construct a String from
   a static string literal: [String.init(_builtinStringLiteral:
   utf8CodeUnitCount: isASCII:)]. Llair2Textual's selector-tracking pass
   already detects this name with the same substring; reuse the same
   probe here for Pulse. *)
let is_builtin_string_literal_init _ n = String.is_substring n ~substring:"builtinStringLiteral"

(* Returns a fresh, non-null value stamped with the given dynamic type.
   The bridge models share this shape and nothing pre-existing in
   [PulseModelsDSL] / [PulseModelsSwift] / [PulseModelsObjC] bundles the
   three steps without also stamping an allocator attribute (which would
   be wrong for a value that already exists on the heap before the
   bridge call). *)
let fresh_typed_nonnull typ : DSL.aval DSL.model_monad =
  let open DSL.Syntax in
  let* v = fresh () in
  let* () = and_positive v in
  let* () = and_dynamic_type_is v typ in
  ret v


let nsstring_typ = Typ.mk_struct (Typ.SwiftClass (SwiftClassName.of_string "NSString"))

let swift_string_typ = Typ.mk_struct (Typ.SwiftClass (SwiftClassName.of_string "TSS"))

let string_bridge_to_nsstring _arg1 _arg2 : model =
  let open DSL.Syntax in
  start_model @@ fun () -> fresh_typed_nonnull nsstring_typ >>= assign_ret


let string_bridge_from_nsstring _arg : model =
  let open DSL.Syntax in
  start_model @@ fun () -> fresh_typed_nonnull swift_string_typ >>= assign_ret


(* [String.init(_builtinStringLiteral:utf8CodeUnitCount:isASCII:)] is
   the initialiser emitted for every Swift static string literal. We
   don't propagate the literal's bytes here — the selector-tracking
   side-channel in [Llair2TextualModels] handles that for the cases
   where it matters (e.g. NSSelectorFromString). *)
let builtin_string_literal_init : model =
  let open DSL.Syntax in
  start_model @@ fun () -> fresh_typed_nonnull swift_string_typ >>= assign_ret


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


(* Whether the closure weak-captures self ([weak self]). The callback handed to
   [register_closure_holder] is usually a partial-apply forwarder whose mangled name is the real
   body's name + [TA]; strip that suffix so we look up the body's cached attribute. *)
let closure_captures_self_weakly mangled =
  let s = Mangled.to_string mangled in
  let body = Option.value (String.chop_suffix s ~suffix:"TA") ~default:s in
  match
    IRAttributes.load (Procname.Swift (SwiftProcname.mk_function (Mangled.from_string body)))
  with
  | Some attrs ->
      attrs.ProcAttributes.swift_captures_self_weakly
  | None ->
      false


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
   Pulse's per-language module convention). The captured-env edge is made weak when the
   callback weak-captures self ([weak self]), otherwise a spurious retain cycle is reported. *)
let register_closure_holder callback captured_env : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* callback_dyn = get_dynamic_type ~ask_specialization:true callback in
  let captures_self_weakly =
    match callback_dyn with
    | Some {Formula.typ= {desc= Typ.Tstruct (SwiftClosure csig)}} ->
        closure_captures_self_weakly csig
    | _ ->
        false
  in
  let holder_class = Typ.SwiftClass SwiftClassName.swift_alloc_unknown_type in
  let field = Fieldname.make ~is_weak:captures_self_weakly holder_class "captured_env" in
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


(* Swift [Optional<T>] construction: [.none] pins the marker to [0], [.some]
   stores the payload and a positive marker.  [.none] records none-ness as a
   marker *value* (not an object invalidation): safe-unwrap lowering -- [guard
   let] / [if let] / optional chaining -- reads the optional's fields without
   going through an unwrap model, so an invalidation here would fire on those
   safe reads (SWIFT_NPE FP).  The unwrap models below raise on a provably-[0]
   marker instead. *)
let swift_optional_init_none opt : unit DSL.model_monad =
  let open DSL.Syntax in
  let* marker = fresh () in
  let* () = store_field ~deref:false ~ref:opt ModeledField.swift_optional_marker marker in
  and_eq_int marker IntLit.zero


let swift_optional_init_some opt payload : unit DSL.model_monad =
  let open DSL.Syntax in
  let* marker = fresh () in
  let* () = and_positive marker in
  let* () = store_field ~deref:false ~ref:opt ModeledField.swift_optional_marker marker in
  store_field ~deref:false ~ref:opt ModeledField.swift_optional_value payload


(* [.some] with no known payload value: used by the symbolic-discriminator
   path-split below, where the construction site doesn't carry an adjacent
   [field_0] store to harvest the payload from.  The marker is still
   positive so downstream unwraps see [.some] semantics; the payload field
   is left unmodelled (any subsequent [load] returns a fresh symbolic
   value, matching Pulse's default for unwritten fields). *)
let swift_optional_init_some_unknown opt : unit DSL.model_monad =
  let open DSL.Syntax in
  let* marker = fresh () in
  let* () = and_positive marker in
  store_field ~deref:false ~ref:opt ModeledField.swift_optional_marker marker


(* Symbolic-discriminator path-split: prune on the tag value and dispatch
   to the literal-[.none] / unknown-[.some] semantics on each branch.
   The two shape-specific entry points below pin the [.none] tag value:
   [Sg]-class shape uses [tag = 1] for [.none], 2-component tuple shape
   uses [tag = 0]. *)
let swift_optional_init_path_split opt tag none_value : unit DSL.model_monad =
  let open DSL.Syntax in
  disj
    [ (let* () = prune_eq_int tag none_value in
       swift_optional_init_none opt )
    ; (let* () = prune_ne_int tag none_value in
       swift_optional_init_some_unknown opt ) ]


let swift_optional_init_sg opt tag = swift_optional_init_path_split opt tag IntLit.one

let swift_optional_init_tuple opt tag = swift_optional_init_path_split opt tag IntLit.zero

(* Raise [OPTIONAL_EMPTY_ACCESS] (remapped to SWIFT_NPE for Swift procnames) by
   invalidating [marker] and immediately accessing it.  Shared by the unwrap
   models so the empty access happens at the unwrap site, not at construction. *)
let swift_optional_report_empty marker : unit DSL.model_monad =
  let open DSL.Syntax in
  let* {path; location} = get_data in
  let* () =
    exec_command (fun astate ->
        PulseOperations.invalidate path UntraceableAccess location OptionalEmpty marker astate )
  in
  check_valid (ValueOrigin.unknown marker)


(* Swift [Optional<T>.unsafelyUnwrapped]: indirect-return ABI
   [getter(ret_buf, type_meta, receiver)].  Fires SWIFT_NPE on a provably-[.none]
   receiver (marker pinned to [0]; symbolic markers from unknown optionals stay
   silent), writes the payload to [ret_buf.field_0] (the field's enclosing class
   is the Optional's payload type, recovered from [type_meta]'s dynamic type by
   stripping the [Sg] Optional suffix) and also assigns the payload to the SIL
   return slot.  Both writes matter: Swift's indirect-return ABI uses ret_buf for
   value-typed payloads, while reference-typed unwraps may consume the SIL return
   value. *)
let swift_unsafely_unwrapped ret_buf type_meta receiver : unit DSL.model_monad =
  let open DSL.Syntax in
  let* marker =
    load_access ~deref:false receiver (FieldAccess ModeledField.swift_optional_marker)
  in
  let* marker_const = as_constant_int marker in
  match marker_const with
  | Some 0 ->
      swift_optional_report_empty marker
  | _ ->
      let* payload =
        load_access ~deref:false receiver (FieldAccess ModeledField.swift_optional_value)
      in
      let* type_meta_data = get_dynamic_type ~ask_specialization:false type_meta in
      let payload_class =
        match type_meta_data with
        | Some {Formula.typ= {desc= Typ.Tstruct (Typ.SwiftClass swift_name)}} ->
            (* The type_meta arg is allocated with the Optional class [TSiSg / TSSSg / ...];
               strip the trailing [Sg] to get the payload's class [TSi / TSS / ...]. *)
            let mangled = SwiftClassName.mangled swift_name in
            if String.is_suffix mangled ~suffix:"Sg" then
              let payload_mangled = String.drop_suffix mangled 2 in
              Some (Typ.SwiftClass (SwiftClassName.of_string payload_mangled))
            else None
        | _ ->
            None
      in
      let* () =
        match payload_class with
        | Some payload_class ->
            let field_0 = Fieldname.make payload_class "field_0" in
            store_field ~ref:ret_buf field_0 payload
        | None ->
            ret ()
      in
      assign_ret payload


(* Swift Optional force-unwrap (postfix [!]) trap: emitted by the frontend on
   the proven-[.none] branch of the [if eq(disc, 0) then __sil_assert_fail]
   idiom. Marks the Optional's marker as [OptionalEmpty]-invalidated and
   immediately accesses it -- triggers [OPTIONAL_EMPTY_ACCESS] which the
   [PulseReport.get_issue_type] hook remaps to [SWIFT_NPE] for Swift
   procnames. *)
let swift_optional_force_unwrap_trap opt : unit DSL.model_monad =
  let open DSL.Syntax in
  let* marker = fresh () in
  let* () = and_eq_int marker IntLit.zero in
  let* () = swift_optional_report_empty marker in
  (* Best-effort: attach the marker to [opt] for nicer trace context.  Last so a path where
     [opt] is constrained to be null (e.g. the raw-pointer null-check shape from an [as!] cast)
     doesn't get killed before the report fires. *)
  store_field ~deref:false ~ref:opt ModeledField.swift_optional_marker marker


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
  | OptionalInitNone -> (
    match args with [opt] -> fun () -> swift_optional_init_none opt | _ -> unknown args )
  | OptionalInitSome -> (
    match args with
    | [opt; payload] ->
        fun () -> swift_optional_init_some opt payload
    | _ ->
        unknown args )
  | OptionalInitSg -> (
    match args with [opt; tag] -> fun () -> swift_optional_init_sg opt tag | _ -> unknown args )
  | OptionalInitTuple -> (
    match args with [opt; tag] -> fun () -> swift_optional_init_tuple opt tag | _ -> unknown args )
  | OptionalUnsafelyUnwrapped -> (
    match args with
    | [ret_buf; type_meta; receiver] ->
        fun () -> swift_unsafely_unwrapped ret_buf type_meta receiver
    | _ ->
        unknown args )
  | OptionalForceUnwrapTrap -> (
    match args with [opt] -> fun () -> swift_optional_force_unwrap_trap opt | _ -> unknown args )
  | SwiftAlloc -> (
    match func_args with size :: _ -> alloc (FuncArg.exp size) | _ -> unknown args )
  | Memcpy ->
      unknown args
  | MetadataEquals ->
      let arg1, arg2, _ = ProcnameDispatcherBuiltins.expect_at_least_2_args args builtin_s in
      metadata_equals arg1 arg2
  | SwiftGetDynamicType -> (
    (* degrade to [unknown] on a <2-arg call instead of aborting analysis *)
    match args with
    | arg1 :: arg2 :: _ ->
        swift_dynamic_type arg1 arg2
    | _ ->
        unknown args )


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
  ; -"swift_getObjectType" <>$ capt_arg_payload $--> swift_get_object_type
  ; ~+is_string_bridge_to_nsstring $ capt_arg_payload $+ capt_arg_payload
    $+...$--> string_bridge_to_nsstring
  ; ~+is_string_bridge_from_nsstring $ capt_arg_payload $+...$--> string_bridge_from_nsstring
  ; ~+is_builtin_string_literal_init <>--> builtin_string_literal_init ]
  |> List.map ~f:(fun matcher ->
      matcher |> ProcnameDispatcher.Call.contramap_arg_payload ~f:ValueOrigin.addr_hist )
