(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module CallEvent = PulseCallEvent
module Invalidation = PulseInvalidation
module Taint = PulseTaint
module Timestamp = PulseTimestamp
module Trace = PulseTrace
module ValueHistory = PulseValueHistory

(** Ideally, we don't want attributes to depend on {!AbstractValue} because they become a pain to
    handle when comparing memory states.

    If you find you need to make attributes depend on {!AbstractValue} then remember to modify 1)
    graph operations of {!PulseDomain.GraphVisit} for reachability and interprocedural operations in
    {!PulseAbductiveDomain} 2) application of summaries if these attributes can appear in summaries
    and 3) canonicalisation operations in {!PulseBaseMemory.ml} as these interpret abstract values
    in the state up to some substitution. *)

module Attribute = struct
  type allocator =
    | CMalloc
    | CustomMalloc of Procname.t
    | CRealloc
    | CustomRealloc of Procname.t
    | CppNew
    | CppNewArray
    | JavaResource of JavaClassName.t
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


  type t =
    | AddressOfCppTemporary of Var.t * ValueHistory.t
    | AddressOfStackVariable of Var.t * Location.t * ValueHistory.t
    | Allocated of allocator * Trace.t
    | Closure of Procname.t
    | CopiedVar of Var.t
    | DynamicType of Typ.t
    | EndOfCollection
    | Invalid of Invalidation.t * Trace.t
    | ISLAbduced of Trace.t
    | MustBeInitialized of Timestamp.t * Trace.t
    | MustBeValid of Timestamp.t * Trace.t * Invalidation.must_be_valid_reason option
    | MustNotBeTainted of Timestamp.t * Taint.sink * Trace.t
    | JavaResourceReleased
    | RefCounted
    | SourceOriginOfCopy of PulseAbstractValue.t
    | StdVectorReserve
    | Tainted of Taint.source * ValueHistory.t
    | Uninitialized
    | UnknownEffect of CallEvent.t * ValueHistory.t
    | UnreachableAt of Location.t
    | WrittenTo of Trace.t
  [@@deriving compare, variants]

  let equal = [%compare.equal: t]

  type rank = int

  let to_rank = Variants.to_rank

  let dummy_trace = Trace.Immediate {location= Location.dummy; history= ValueHistory.epoch}

  let dummy_var =
    let pname = Procname.from_string_c_fun "" in
    Var.of_pvar (Pvar.mk (Mangled.from_string "") pname)


  let closure_rank = Variants.to_rank (Closure (Procname.from_string_c_fun ""))

  let copied_var_rank = Variants.to_rank (CopiedVar dummy_var)

  let dummy_val = PulseAbstractValue.of_id 0

  let copy_origin_rank = Variants.to_rank (SourceOriginOfCopy dummy_val)

  let written_to_rank = Variants.to_rank (WrittenTo dummy_trace)

  let address_of_stack_variable_rank =
    let location = Location.dummy in
    Variants.to_rank (AddressOfStackVariable (dummy_var, location, ValueHistory.epoch))


  let invalid_rank =
    Variants.to_rank (Invalid (Invalidation.ConstantDereference IntLit.zero, dummy_trace))


  let java_resource_released_rank = Variants.to_rank JavaResourceReleased

  let must_not_be_tainted_rank =
    let dummy_proc_name = Procname.from_string_c_fun "" in
    Variants.to_rank
      (MustNotBeTainted (Timestamp.t0, Taint.PassedAsArgumentTo dummy_proc_name, dummy_trace))


  let must_be_valid_rank = Variants.to_rank (MustBeValid (Timestamp.t0, dummy_trace, None))

  let std_vector_reserve_rank = Variants.to_rank StdVectorReserve

  let allocated_rank = Variants.to_rank (Allocated (CMalloc, dummy_trace))

  let ref_counted_rank = Variants.to_rank RefCounted

  let dynamic_type_rank = Variants.to_rank (DynamicType StdTyp.void)

  let end_of_collection_rank = Variants.to_rank EndOfCollection

  let isl_abduced_rank = Variants.to_rank (ISLAbduced dummy_trace)

  let tainted_rank =
    let dummy_proc_name = Procname.from_string_c_fun "" in
    Variants.to_rank (Tainted (ReturnValue dummy_proc_name, ValueHistory.epoch))


  let uninitialized_rank = Variants.to_rank Uninitialized

  let unknown_effect_rank = Variants.to_rank (UnknownEffect (Model "", ValueHistory.epoch))

  let unreachable_at_rank = Variants.to_rank (UnreachableAt Location.dummy)

  let must_be_initialized_rank = Variants.to_rank (MustBeInitialized (Timestamp.t0, dummy_trace))

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
    | Closure pname ->
        Procname.pp f pname
    | CopiedVar var ->
        Var.pp f var
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
          (Trace.pp ~pp_immediate:(fun fmt -> Taint.pp_sink fmt sink))
          trace
          (timestamp :> int)
    | JavaResourceReleased ->
        F.pp_print_string f "Released"
    | RefCounted ->
        F.fprintf f "RefCounted"
    | SourceOriginOfCopy source ->
        F.fprintf f "copied of source %a" PulseAbstractValue.pp source
    | StdVectorReserve ->
        F.pp_print_string f "std::vector::reserve()"
    | Tainted (source, hist) ->
        F.fprintf f "Tainted(%a,%a)" Taint.pp_source source ValueHistory.pp hist
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
    | Closure _
    | CopiedVar _
    | DynamicType _
    | EndOfCollection
    | JavaResourceReleased
    | SourceOriginOfCopy _
    | StdVectorReserve
    | Tainted _
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
    | Closure _
    | CopiedVar _
    | DynamicType _
    | EndOfCollection
    | ISLAbduced _
    | Invalid _
    | JavaResourceReleased
    | RefCounted
    | SourceOriginOfCopy _
    | StdVectorReserve
    | Tainted _
    | Uninitialized
    | UnknownEffect _
    | WrittenTo _ ->
        true


  let is_suitable_for_summary attr =
    match attr with CopiedVar _ | SourceOriginOfCopy _ -> false | _ -> true


  let add_call timestamp proc_name call_location caller_history attr =
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
    | Tainted (source, hist) ->
        Tainted (source, add_call_to_history hist)
    | UnknownEffect (call, hist) ->
        UnknownEffect (call, add_call_to_history hist)
    | WrittenTo trace ->
        WrittenTo (add_call_to_trace trace)
    | ( AddressOfCppTemporary _
      | AddressOfStackVariable _
      | Closure _
      | CopiedVar _
      | DynamicType _
      | EndOfCollection
      | JavaResourceReleased
      | RefCounted
      | SourceOriginOfCopy _
      | StdVectorReserve
      | UnreachableAt _
      | Uninitialized ) as attr ->
        attr


  let alloc_free_match allocator (invalidation : (Invalidation.t * Trace.t) option) is_released =
    match (allocator, invalidation) with
    | (CMalloc | CustomMalloc _ | CRealloc | CustomRealloc _), Some ((CFree | CustomFree _), _)
    | CppNew, Some (CppDelete, _)
    | CppNewArray, Some (CppDeleteArray, _) ->
        true
    | JavaResource _, _ ->
        is_released
    | _ ->
        false
end

module Attributes = struct
  module Set = PrettyPrintable.MakePPUniqRankSet (Int) (Attribute)

  let get_invalid attrs =
    Set.find_rank attrs Attribute.invalid_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.Invalid (invalidation, trace)) = attr in
           (invalidation, trace) )


  let get_tainted attrs =
    Set.find_rank attrs Attribute.tainted_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.Tainted (source, hist)) = attr in
           (source, hist) )


  let is_java_resource_released attrs =
    Set.find_rank attrs Attribute.java_resource_released_rank |> Option.is_some


  let get_must_be_valid attrs =
    Set.find_rank attrs Attribute.must_be_valid_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.MustBeValid (timestamp, trace, reason)) = attr in
           (timestamp, trace, reason) )


  let get_must_not_be_tainted attrs =
    Set.find_rank attrs Attribute.must_not_be_tainted_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.MustNotBeTainted (timestamp, sink, trace)) = attr in
           (timestamp, sink, trace) )


  let get_written_to attrs =
    Set.find_rank attrs Attribute.written_to_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.WrittenTo action) = attr in
           action )


  let get_closure_proc_name attrs =
    Set.find_rank attrs Attribute.closure_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.Closure proc_name) = attr in
           proc_name )


  let get_copied_var attrs =
    Set.find_rank attrs Attribute.copied_var_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.CopiedVar var) = attr in
           var )


  let get_source_origin_of_copy attrs =
    Set.find_rank attrs Attribute.copy_origin_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.SourceOriginOfCopy source) = attr in
           source )


  let get_address_of_stack_variable attrs =
    Set.find_rank attrs Attribute.address_of_stack_variable_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.AddressOfStackVariable (var, loc, history)) = attr in
           (var, loc, history) )


  let is_end_of_collection attrs =
    Set.find_rank attrs Attribute.end_of_collection_rank |> Option.is_some


  let is_std_vector_reserved attrs =
    Set.find_rank attrs Attribute.std_vector_reserve_rank |> Option.is_some


  let is_modified attrs =
    Option.is_some (Set.find_rank attrs Attribute.written_to_rank)
    || Option.is_some (Set.find_rank attrs Attribute.invalid_rank)
    || Option.is_some (Set.find_rank attrs Attribute.unknown_effect_rank)
    || Option.is_some (Set.find_rank attrs Attribute.java_resource_released_rank)


  let is_uninitialized attrs = Set.find_rank attrs Attribute.uninitialized_rank |> Option.is_some

  let is_ref_counted attrs = Set.find_rank attrs Attribute.ref_counted_rank |> Option.is_some

  let get_allocation attrs =
    Set.find_rank attrs Attribute.allocated_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.Allocated (allocator, trace)) = attr in
           (allocator, trace) )


  let get_isl_abduced attrs =
    Set.find_rank attrs Attribute.isl_abduced_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.ISLAbduced trace) = attr in
           trace )


  let get_unknown_effect attrs =
    Set.find_rank attrs Attribute.unknown_effect_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.UnknownEffect (call, hist)) = attr in
           (call, hist) )


  let get_dynamic_type attrs =
    Set.find_rank attrs Attribute.dynamic_type_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.DynamicType typ) = attr in
           typ )


  let get_must_be_initialized attrs =
    Set.find_rank attrs Attribute.must_be_initialized_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.MustBeInitialized (timestamp, trace)) = attr in
           (timestamp, trace) )


  let get_unreachable_at attrs =
    Set.find_rank attrs Attribute.unreachable_at_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.UnreachableAt location) = attr in
           location )


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


  let add_call timestamp proc_name call_location caller_history attrs =
    Set.map attrs ~f:(fun attr ->
        Attribute.add_call timestamp proc_name call_location caller_history attr )


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
