(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging
module AbstractValue = PulseAbstractValue
module CallEvent = PulseCallEvent
module Invalidation = PulseInvalidation
module Taint = PulseTaint
module Timestamp = PulseTimestamp
module Trace = PulseTrace
module ValueHistory = PulseValueHistory

module Attribute = struct
  type allocator =
    | CMalloc
    | CustomMalloc of Procname.t
    | CRealloc
    | CustomRealloc of Procname.t
    | CppNew
    | CppNewArray
    | JavaResource of JavaClassName.t
    | ObjCAlloc
  [@@deriving compare, equal]

  let pp_allocator fmt = function
    | CMalloc ->
        F.fprintf fmt "malloc"
    | CustomMalloc proc_name ->
        F.fprintf fmt "%a (custom malloc)" Procname.pp proc_name
    | CRealloc ->
        F.fprintf fmt "realloc"
    | CustomRealloc proc_name ->
        F.fprintf fmt "%a (custom realloc)" Procname.pp proc_name
    | CppNew ->
        F.fprintf fmt "new"
    | CppNewArray ->
        F.fprintf fmt "new[]"
    | JavaResource class_name ->
        F.fprintf fmt "resource %a" JavaClassName.pp class_name
    | ObjCAlloc ->
        F.fprintf fmt "alloc"


  type taint_in = {v: AbstractValue.t} [@@deriving compare, equal]

  let pp_taint_in fmt {v} = F.fprintf fmt "{@[v= %a@]}" AbstractValue.pp v

  module CopiedInto = struct
    type t = IntoVar of Var.t | IntoField of Fieldname.t [@@deriving compare, equal]

    let pp fmt = function
      | IntoVar var ->
          Var.pp fmt var
      | IntoField field ->
          Fieldname.pp fmt field
  end

  type t =
    | AddressOfCppTemporary of Var.t * ValueHistory.t
    | AddressOfStackVariable of Var.t * Location.t * ValueHistory.t
    | Allocated of allocator * Trace.t
    | AlwaysReachable
    | Closure of Procname.t
    | CopiedInto of CopiedInto.t
    | DynamicType of Typ.t
    | EndOfCollection
    | Invalid of Invalidation.t * Trace.t
    | ISLAbduced of Trace.t
    | MustBeInitialized of Timestamp.t * Trace.t
    | MustBeValid of Timestamp.t * Trace.t * Invalidation.must_be_valid_reason option
    | MustNotBeTainted of Timestamp.t * Taint.t * Trace.t
    | JavaResourceReleased
    | PropagateTaintFrom of taint_in list (* [v -> PropagateTaintFrom \[v1; ..; vn\]] does not
                                             retain [v1] to [vn], in fact they should be collected
                                             when they become unreachable *)
    | RefCounted
    | SourceOriginOfCopy of {source: AbstractValue.t; is_const_ref: bool}
    | StdMoved
    | StdVectorReserve
    | Tainted of {source: Taint.t; hist: ValueHistory.t; intra_procedural_only: bool}
    | TaintSanitized of Taint.t * Trace.t
    | Uninitialized
    | UnknownEffect of CallEvent.t * ValueHistory.t
    | UnreachableAt of Location.t
    | WrittenTo of Trace.t
  [@@deriving compare, variants]

  let equal = [%compare.equal: t]

  type rank = int

  let to_rank = Variants.to_rank

  let address_of_stack_variable_rank = Variants.addressofstackvariable.rank

  let allocated_rank = Variants.allocated.rank

  let always_reachable_rank = Variants.alwaysreachable.rank

  let closure_rank = Variants.closure.rank

  let copied_into_rank = Variants.copiedinto.rank

  let copy_origin_rank = Variants.sourceoriginofcopy.rank

  let dynamic_type_rank = Variants.dynamictype.rank

  let end_of_collection_rank = Variants.endofcollection.rank

  let invalid_rank = Variants.invalid.rank

  let isl_abduced_rank = Variants.islabduced.rank

  let java_resource_released_rank = Variants.javaresourcereleased.rank

  let must_be_initialized_rank = Variants.mustbeinitialized.rank

  let must_be_valid_rank = Variants.mustbevalid.rank

  let must_not_be_tainted_rank = Variants.mustnotbetainted.rank

  let propagate_taint_from_rank = Variants.propagatetaintfrom.rank

  let ref_counted_rank = Variants.refcounted.rank

  let std_moved_rank = Variants.stdmoved.rank

  let std_vector_reserve_rank = Variants.stdvectorreserve.rank

  let taint_sanitized_rank = Variants.taintsanitized.rank

  let tainted_rank = Variants.tainted.rank

  let uninitialized_rank = Variants.uninitialized.rank

  let unknown_effect_rank = Variants.unknowneffect.rank

  let unreachable_at_rank = Variants.unreachableat.rank

  let written_to_rank = Variants.writtento.rank

  let isl_subset attr1 attr2 =
    match (attr1, attr2) with
    | Invalid (v1, _), Invalid (v2, _) ->
        Invalidation.isl_equiv v1 v2
    | Invalid _, (WrittenTo _ | DynamicType _) ->
        true
    | Uninitialized, Uninitialized ->
        true
    | (MustBeValid _ | Allocated _ | ISLAbduced _), Invalid _ ->
        false
    | _, UnknownEffect _ ->
        (* ignore *)
        true
    | Invalid _, _ | _, Uninitialized ->
        false
    | _ ->
        true


  let pp f attribute =
    let pp_string_if_debug string fmt =
      if Config.debug_level_analysis >= 3 then F.pp_print_string fmt string
    in
    match attribute with
    | AddressOfCppTemporary (var, history) ->
        F.fprintf f "t&%a (%a)" Var.pp var ValueHistory.pp history
    | AddressOfStackVariable (var, location, history) ->
        F.fprintf f "s&%a (%a) at %a" Var.pp var ValueHistory.pp history Location.pp location
    | Allocated (allocator, trace) ->
        F.fprintf f "Allocated%a"
          (Trace.pp ~pp_immediate:(pp_string_if_debug (F.asprintf "(%a)" pp_allocator allocator)))
          trace
    | AlwaysReachable ->
        F.pp_print_string f "AlwaysReachable"
    | Closure pname ->
        Procname.pp f pname
    | CopiedInto copied_into ->
        CopiedInto.pp f copied_into
    | DynamicType typ ->
        F.fprintf f "DynamicType %a" (Typ.pp Pp.text) typ
    | EndOfCollection ->
        F.pp_print_string f "EndOfCollection"
    | Invalid (invalidation, trace) ->
        F.fprintf f "Invalid %a"
          (Trace.pp ~pp_immediate:(fun fmt -> Invalidation.pp fmt invalidation))
          trace
    | ISLAbduced trace ->
        F.fprintf f "ISLAbduced %a" (Trace.pp ~pp_immediate:(pp_string_if_debug "ISLAbduced")) trace
    | MustBeInitialized (timestamp, trace) ->
        F.fprintf f "MustBeInitialized(%a, t=%d)"
          (Trace.pp ~pp_immediate:(pp_string_if_debug "read"))
          trace
          (timestamp :> int)
    | MustBeValid (timestamp, trace, reason) ->
        F.fprintf f "MustBeValid(%a, %a, t=%d)"
          (Trace.pp ~pp_immediate:(pp_string_if_debug "access"))
          trace Invalidation.pp_must_be_valid_reason reason
          (timestamp :> int)
    | MustNotBeTainted (timestamp, sink, trace) ->
        F.fprintf f "MustNotBeTainted(%a, t=%d)"
          (Trace.pp ~pp_immediate:(fun fmt -> Taint.pp fmt sink))
          trace
          (timestamp :> int)
    | JavaResourceReleased ->
        F.pp_print_string f "Released"
    | PropagateTaintFrom taints_in ->
        F.fprintf f "PropagateTaintFrom([%a])" (Pp.seq ~sep:";" pp_taint_in) taints_in
    | RefCounted ->
        F.fprintf f "RefCounted"
    | SourceOriginOfCopy {source; is_const_ref} ->
        F.fprintf f "copied of source %a" AbstractValue.pp source ;
        if is_const_ref then F.pp_print_string f " (const&)"
    | StdMoved ->
        F.pp_print_string f "std::move()"
    | StdVectorReserve ->
        F.pp_print_string f "std::vector::reserve()"
    | Tainted {source; hist; intra_procedural_only} ->
        F.fprintf f "Tainted(%a,%a,%b)" Taint.pp source ValueHistory.pp hist intra_procedural_only
    | TaintSanitized (sanitizer, trace) ->
        F.fprintf f "TaintSanitized(%a)"
          (Trace.pp ~pp_immediate:(fun fmt -> Taint.pp fmt sanitizer))
          trace
    | Uninitialized ->
        F.pp_print_string f "Uninitialized"
    | UnknownEffect (call, hist) ->
        F.fprintf f "UnknownEffect(%a, %a)" CallEvent.pp call ValueHistory.pp hist
    | UnreachableAt location ->
        F.fprintf f "UnreachableAt(%a)" Location.pp location
    | WrittenTo trace ->
        F.fprintf f "WrittenTo %a" (Trace.pp ~pp_immediate:(pp_string_if_debug "mutation")) trace


  let is_suitable_for_pre = function
    | MustBeValid _ | MustBeInitialized _ | MustNotBeTainted _ | RefCounted ->
        true
    | Invalid _ | Allocated _ | ISLAbduced _ ->
        Config.pulse_isl
    | AddressOfCppTemporary _
    | AddressOfStackVariable _
    | AlwaysReachable
    | Closure _
    | CopiedInto _
    | DynamicType _
    | EndOfCollection
    | JavaResourceReleased
    | PropagateTaintFrom _
    | SourceOriginOfCopy _
    | StdMoved
    | StdVectorReserve
    | Tainted _
    | TaintSanitized _
    | Uninitialized
    | UnknownEffect _
    | UnreachableAt _
    | WrittenTo _ ->
        false


  let is_suitable_for_post = function
    | MustBeInitialized _ | MustBeValid _ | MustNotBeTainted _ | UnreachableAt _ ->
        false
    | AddressOfCppTemporary _
    | AddressOfStackVariable _
    | Allocated _
    | AlwaysReachable
    | Closure _
    | CopiedInto _
    | DynamicType _
    | EndOfCollection
    | ISLAbduced _
    | Invalid _
    | JavaResourceReleased
    | PropagateTaintFrom _
    | RefCounted
    | SourceOriginOfCopy _
    | StdMoved
    | StdVectorReserve
    | Tainted _
    | TaintSanitized _
    | Uninitialized
    | UnknownEffect _
    | WrittenTo _ ->
        true


  let is_suitable_for_summary attr =
    match attr with
    | CopiedInto _ | SourceOriginOfCopy _ ->
        false
    | Tainted {intra_procedural_only} ->
        not intra_procedural_only
    | AddressOfCppTemporary _
    | AddressOfStackVariable _
    | Allocated _
    | AlwaysReachable
    | Closure _
    | DynamicType _
    | EndOfCollection
    | Invalid _
    | ISLAbduced _
    | MustBeInitialized _
    | MustBeValid _
    | MustNotBeTainted _
    | JavaResourceReleased
    | PropagateTaintFrom _
    | RefCounted
    | StdMoved
    | StdVectorReserve
    | TaintSanitized _
    | Uninitialized
    | UnknownEffect _
    | UnreachableAt _
    | WrittenTo _ ->
        true


  let add_call_and_subst subst timestamp proc_name call_location caller_history attr =
    let add_call_to_trace in_call =
      Trace.ViaCall {f= Call proc_name; location= call_location; history= caller_history; in_call}
    in
    let add_call_to_history in_call =
      ValueHistory.singleton (Call {f= Call proc_name; location= call_location; in_call; timestamp})
    in
    match attr with
    | Allocated (proc_name, trace) ->
        Allocated (proc_name, add_call_to_trace trace)
    | Invalid (invalidation, trace) ->
        Invalid (invalidation, add_call_to_trace trace)
    | ISLAbduced trace ->
        ISLAbduced (add_call_to_trace trace)
    | MustBeValid (_timestamp, trace, reason) ->
        MustBeValid (timestamp, add_call_to_trace trace, reason)
    | MustBeInitialized (_timestamp, trace) ->
        MustBeInitialized (timestamp, add_call_to_trace trace)
    | MustNotBeTainted (_timestamp, sink, trace) ->
        MustNotBeTainted (timestamp, sink, add_call_to_trace trace)
    | PropagateTaintFrom taints_in ->
        PropagateTaintFrom (List.map taints_in ~f:(fun {v} -> {v= subst v}))
    | Tainted {source; hist; intra_procedural_only= false} ->
        Tainted {source; hist= add_call_to_history hist; intra_procedural_only= false}
    | TaintSanitized (sanitizer, trace) ->
        TaintSanitized (sanitizer, add_call_to_trace trace)
    | UnknownEffect (call, hist) ->
        UnknownEffect (call, add_call_to_history hist)
    | WrittenTo trace ->
        WrittenTo (add_call_to_trace trace)
    | CopiedInto _ | SourceOriginOfCopy _ | Tainted {intra_procedural_only= true} ->
        L.die InternalError "Unexpected attribute %a in the summary of %a" pp attr Procname.pp
          proc_name
    | ( AddressOfCppTemporary _
      | AddressOfStackVariable _
      | AlwaysReachable
      | Closure _
      | DynamicType _
      | EndOfCollection
      | JavaResourceReleased
      | RefCounted
      | StdMoved
      | StdVectorReserve
      | UnreachableAt _
      | Uninitialized ) as attr ->
        attr


  let alloc_free_match allocator (invalidation : (Invalidation.t * Trace.t) option) is_released =
    match (allocator, invalidation) with
    | (CMalloc | CustomMalloc _ | CRealloc | CustomRealloc _), Some ((CFree | CustomFree _), _)
    | CppNew, Some (CppDelete, _)
    | CppNewArray, Some (CppDeleteArray, _)
    | ObjCAlloc, _ ->
        true
    | JavaResource _, _ ->
        is_released
    | _ ->
        false


  let filter_unreachable f_keep = function
    | PropagateTaintFrom taints_in ->
        let taints_in' = List.filter taints_in ~f:(fun {v} -> f_keep v) in
        if List.is_empty taints_in' then None else Some (PropagateTaintFrom taints_in')
    | ( AddressOfCppTemporary _
      | AddressOfStackVariable _
      | Allocated _
      | AlwaysReachable
      | Closure _
      | CopiedInto _
      | DynamicType _
      | EndOfCollection
      | Invalid _
      | ISLAbduced _
      | MustBeInitialized _
      | MustBeValid _
      | MustNotBeTainted _
      | JavaResourceReleased
      | RefCounted
      | SourceOriginOfCopy _
      | StdMoved
      | StdVectorReserve
      | Tainted _
      | TaintSanitized _
      | Uninitialized
      | UnknownEffect _
      | UnreachableAt _
      | WrittenTo _ ) as attr ->
        Some attr
end

module Attributes = struct
  module Set = PrettyPrintable.MakePPUniqRankSet (Int) (Attribute)

  let get_by_rank rank ~dest attrs = Set.find_rank attrs rank |> Option.map ~f:dest

  let mem_by_rank rank attrs = Set.find_rank attrs rank |> Option.is_some

  let get_invalid =
    get_by_rank Attribute.invalid_rank ~dest:(function [@warning "-8"]
        | Invalid (invalidation, trace) -> (invalidation, trace) )


  let get_propagate_taint_from =
    get_by_rank Attribute.propagate_taint_from_rank ~dest:(function [@warning "-8"]
        | PropagateTaintFrom taints_in -> taints_in )


  let get_tainted =
    get_by_rank Attribute.tainted_rank ~dest:(function [@warning "-8"]
        | Tainted {source; hist; intra_procedural_only} -> (source, hist, intra_procedural_only) )


  let get_taint_sanitized =
    get_by_rank Attribute.taint_sanitized_rank ~dest:(function [@warning "-8"]
        | TaintSanitized (sanitizer, trace) -> (sanitizer, trace) )


  let is_java_resource_released = mem_by_rank Attribute.java_resource_released_rank

  let get_must_be_valid =
    get_by_rank Attribute.must_be_valid_rank ~dest:(function [@warning "-8"]
        | Attribute.MustBeValid (timestamp, trace, reason) -> (timestamp, trace, reason) )


  let get_must_not_be_tainted =
    get_by_rank Attribute.must_not_be_tainted_rank ~dest:(function [@warning "-8"]
        | MustNotBeTainted (timestamp, sink, trace) -> (timestamp, sink, trace) )


  let get_written_to =
    get_by_rank Attribute.written_to_rank ~dest:(function [@warning "-8"] WrittenTo action ->
        action )


  let get_closure_proc_name =
    get_by_rank Attribute.closure_rank ~dest:(function [@warning "-8"] Closure proc_name ->
        proc_name )


  let get_copied_into =
    get_by_rank Attribute.copied_into_rank ~dest:(function [@warning "-8"]
        | CopiedInto copied_into -> copied_into )


  let get_source_origin_of_copy =
    get_by_rank Attribute.copy_origin_rank ~dest:(function [@warning "-8"]
        | SourceOriginOfCopy {source; is_const_ref} -> (source, is_const_ref) )


  let get_address_of_stack_variable =
    get_by_rank Attribute.address_of_stack_variable_rank ~dest:(function [@warning "-8"]
        | AddressOfStackVariable (var, loc, history) -> (var, loc, history) )


  let is_end_of_collection = mem_by_rank Attribute.end_of_collection_rank

  let is_std_moved = mem_by_rank Attribute.std_moved_rank

  let is_std_vector_reserved = mem_by_rank Attribute.std_vector_reserve_rank

  let is_modified attrs =
    mem_by_rank Attribute.written_to_rank attrs
    || mem_by_rank Attribute.invalid_rank attrs
    || mem_by_rank Attribute.unknown_effect_rank attrs
    || mem_by_rank Attribute.java_resource_released_rank attrs


  let is_always_reachable = mem_by_rank Attribute.always_reachable_rank

  let is_uninitialized = mem_by_rank Attribute.uninitialized_rank

  let is_ref_counted = mem_by_rank Attribute.ref_counted_rank

  let get_allocation =
    get_by_rank Attribute.allocated_rank ~dest:(function [@warning "-8"]
        | Allocated (allocator, trace) -> (allocator, trace) )


  let get_isl_abduced =
    get_by_rank Attribute.isl_abduced_rank ~dest:(function [@warning "-8"] ISLAbduced trace ->
        trace )


  let get_unknown_effect =
    get_by_rank Attribute.unknown_effect_rank ~dest:(function [@warning "-8"]
        | UnknownEffect (call, hist) -> (call, hist) )


  let get_dynamic_type =
    get_by_rank Attribute.dynamic_type_rank ~dest:(function [@warning "-8"] DynamicType typ -> typ)


  let get_must_be_initialized =
    get_by_rank Attribute.must_be_initialized_rank ~dest:(function [@warning "-8"]
        | MustBeInitialized (timestamp, trace) -> (timestamp, trace) )


  let get_unreachable_at =
    get_by_rank Attribute.unreachable_at_rank ~dest:(function [@warning "-8"]
        | UnreachableAt location -> location )


  let isl_subset callee_attrs caller_attrs =
    Set.for_all callee_attrs ~f:(fun attr1 ->
        Set.for_all caller_attrs ~f:(fun attr2 -> Attribute.isl_subset attr1 attr2) )


  let replace_isl_abduced attrs_callee attrs_caller =
    Set.fold attrs_callee ~init:Set.empty ~f:(fun acc attr1 ->
        let attr1 =
          match attr1 with
          | ISLAbduced _ -> (
            match get_allocation attrs_caller with
            | None ->
                attr1
            | Some (p, a) ->
                Attribute.Allocated (p, a) )
          | Invalid (v_callee, _) -> (
            match get_invalid attrs_caller with
            | None ->
                attr1
            | Some (v_caller, trace) -> (
              match (v_callee, v_caller) with
              | CFree, (CFree | CppDelete) ->
                  Attribute.Invalid (v_caller, trace)
              | ConstantDereference i, OptionalEmpty when IntLit.iszero i ->
                  Attribute.Invalid (OptionalEmpty, trace)
              | _ ->
                  attr1 ) )
          | _ ->
              attr1
        in
        Set.add acc attr1 )


  let add_call_and_subst subst timestamp proc_name call_location caller_history attrs =
    Set.map attrs ~f:(fun attr ->
        Attribute.add_call_and_subst subst timestamp proc_name call_location caller_history attr )


  let get_allocated_not_freed attributes =
    let allocated_opt = get_allocation attributes in
    Option.value_map ~default:None allocated_opt ~f:(fun (allocator, _) ->
        let invalidation = get_invalid attributes in
        let is_released = is_java_resource_released attributes in
        if Attribute.alloc_free_match allocator invalidation is_released then None
        else allocated_opt )


  let remove_unsuitable_for_summary = Set.filter ~f:Attribute.is_suitable_for_summary

  include Set
end

include Attribute
