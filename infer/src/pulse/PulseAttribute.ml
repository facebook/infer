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
module ConfigName = FbPulseConfigName
module DecompilerExpr = PulseDecompilerExpr
module Invalidation = PulseInvalidation
module TaintConfig = PulseTaintConfig
module TaintItem = PulseTaintItem
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
    | CSharpResource of CSharpClassName.t
    | ObjCAlloc
    | HackAsync
    | HackBuilderResource of HackClassName.t
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
        F.fprintf fmt "java resource %a" JavaClassName.pp class_name
    | CSharpResource class_name ->
        F.fprintf fmt "csharp resource %a" CSharpClassName.pp class_name
    | ObjCAlloc ->
        F.fprintf fmt "alloc"
    | HackAsync ->
        F.fprintf fmt "hack async"
    | HackBuilderResource class_name ->
        F.fprintf fmt "hack builder %a" HackClassName.pp class_name


  type taint_in = {v: AbstractValue.t; history: (ValueHistory.t[@compare.ignore] [@equal.ignore])}
  [@@deriving compare, equal]

  let pp_taint_in fmt {v; history} =
    let pp_history fmt =
      if Config.debug_level_analysis >= 3 then F.fprintf fmt "; history= %a" ValueHistory.pp history
    in
    F.fprintf fmt "{@[v= %a%t@]}" AbstractValue.pp v pp_history


  module Tainted = struct
    type t =
      { source: TaintItem.t
      ; time_trace: Timestamp.trace
      ; hist: ValueHistory.t
      ; intra_procedural_only: bool }
    [@@deriving compare, equal]

    let pp fmt {source; hist; time_trace; intra_procedural_only} =
      F.fprintf fmt "(%a, %a, %b, t=%a)" TaintItem.pp source ValueHistory.pp hist
        intra_procedural_only Timestamp.pp_trace time_trace
  end

  module TaintedSet = PrettyPrintable.MakePPSet (Tainted)

  module TaintSink = struct
    type t = {sink: TaintItem.value_tuple; time: Timestamp.t; trace: Trace.t}
    [@@deriving compare, equal]

    let pp fmt {time; sink; trace} =
      F.fprintf fmt "(%a, t=%d)"
        (Trace.pp ~pp_immediate:(fun fmt -> TaintItem.pp_value_tuple_debug fmt sink))
        trace
        (time :> int)
  end

  module TaintSinkMap = PrettyPrintable.MakePPMap (TaintConfig.Kind)

  module TaintSanitized = struct
    type t = {sanitizer: TaintItem.t; time_trace: Timestamp.trace; trace: Trace.t}
    [@@deriving compare, equal]

    let pp fmt {sanitizer; time_trace; trace} =
      F.fprintf fmt "(%a, t=%a)"
        (Trace.pp ~pp_immediate:(fun fmt -> TaintItem.pp fmt sanitizer))
        trace Timestamp.pp_trace time_trace
  end

  module TaintSanitizedSet = PrettyPrintable.MakePPSet (TaintSanitized)

  type taint_propagation_reason = InternalModel | UnknownCall | UserConfig
  [@@deriving compare, equal, show {with_path= false}]

  module CopyOrigin = struct
    type t = CopyCtor | CopyAssignment | CopyToOptional | CopyInGetDefault
    [@@deriving compare, equal]

    let pp fmt = function
      | CopyCtor ->
          F.fprintf fmt "copied"
      | CopyAssignment ->
          F.fprintf fmt "copy assigned"
      | CopyToOptional ->
          F.fprintf fmt "copied by Optional value construction"
      | CopyInGetDefault ->
          F.fprintf fmt "copied in `folly::get_default`"
  end

  module CopiedInto = struct
    type t =
      | IntoVar of {copied_var: Var.t}
      | IntoIntermediate of {copied_var: Var.t}
      | IntoField of {field: Fieldname.t}
    [@@deriving compare, equal]

    let pp fmt = function
      | IntoVar {copied_var} ->
          Var.pp fmt copied_var
      | IntoIntermediate {copied_var} ->
          F.fprintf fmt "intermediate(%a)" Var.pp copied_var
      | IntoField {field} ->
          Fieldname.pp fmt field


    let is_copied_into_var = function
      | IntoVar _ ->
          true
      | IntoIntermediate _ | IntoField _ ->
          false
  end

  module ConfigUsage = struct
    type t = ConfigName of ConfigName.t | StringParam of {v: AbstractValue.t; config_type: string}
    [@@deriving compare, equal]

    let pp f = function
      | ConfigName config ->
          ConfigName.pp f config
      | StringParam {v; config_type} ->
          F.fprintf f "%s.%a" config_type AbstractValue.pp v
  end

  module Builder = struct
    type t = Discardable | NonDiscardable [@@deriving compare, equal, show]
  end

  module UninitializedTyp = struct
    type t =
      | Value
      | Const of Fieldname.t
      | DictMissingKey of {dict: DecompilerExpr.t; key: Fieldname.t}
    [@@deriving compare, equal]

    let pp f = function
      | Value ->
          F.pp_print_string f "value"
      | Const fld ->
          F.fprintf f "const(%a)" Fieldname.pp fld
      | DictMissingKey {dict; key} ->
          F.fprintf f "DictMissingKey(%a, %a)" DecompilerExpr.pp dict Fieldname.pp key
  end

  module ConstKeys = struct
    module Metadata = struct
      (** The metadata contains a timestamp and a trace. The trace is to construct a proper trace of
          an issue when reporting and the timestamp is to choose the firstly found issue when
          deduplicating the same missing key accesses. *)
      type t = Timestamp.t * Trace.t [@@deriving compare, equal]

      let pp fmt (timestamp, trace) =
        let pp_immediate fmt = F.pp_print_string fmt "immediate" in
        F.fprintf fmt "(%a, %a)" Timestamp.pp timestamp (Trace.pp ~pp_immediate) trace
    end

    include PrettyPrintable.MakePPMonoMap (Fieldname) (Metadata)

    let compare = compare Metadata.compare

    let equal = equal Metadata.equal

    let union =
      union (fun _ left right ->
          (if Timestamp.compare (fst left) (fst right) <= 0 then left else right) |> Option.some )
  end

  type t =
    | AddressOfCppTemporary of Var.t * ValueHistory.t
    | AddressOfStackVariable of Var.t * Location.t * ValueHistory.t
    | Allocated of allocator * Trace.t
    | AlwaysReachable
    | Closure of Procname.t
    | ConfigUsage of ConfigUsage.t
    | CopiedInto of CopiedInto.t
    | CopiedReturn of
        { source: AbstractValue.t
        ; is_const_ref: bool
        ; from: CopyOrigin.t
        ; copied_location: Location.t }
    | DictContainConstKeys
    | DictReadConstKeys of ConstKeys.t
    | EndOfCollection
    | HackBuilder of Builder.t
    | HackSinitCalled
    | InReportedRetainCycle
    | Initialized
    | Invalid of Invalidation.t * Trace.t
    | LastLookup of AbstractValue.t
    | MustBeInitialized of Timestamp.t * Trace.t
    | MustBeValid of Timestamp.t * Trace.t * Invalidation.must_be_valid_reason option
    | MustNotBeTainted of TaintSink.t TaintSinkMap.t
    | JavaResourceReleased
    | CSharpResourceReleased
    | HackAsyncAwaited
    | PropagateTaintFrom of taint_propagation_reason * taint_in list
      (* [v -> PropagateTaintFrom \[v1; ..; vn\]] does not
         retain [v1] to [vn], in fact they should be collected
         when they become unreachable *)
    | ReturnedFromUnknown of AbstractValue.t list
    (* [ret_v -> ReturnedFromUnknown \[v1; ..; vn\]] does not
         retain actuals [v1] to [vn] just like PropagateTaintFrom *)
    | SourceOriginOfCopy of {source: AbstractValue.t; is_const_ref: bool}
    | StaticType of Typ.Name.t
    | StdMoved
    | StdVectorReserve
    | Tainted of TaintedSet.t
    | TaintSanitized of TaintSanitizedSet.t
    | Uninitialized of UninitializedTyp.t
    | UnknownEffect of CallEvent.t * ValueHistory.t
    | UnreachableAt of Location.t
    | UsedAsBranchCond of Procname.t * Location.t * Trace.t
    | WrittenTo of Timestamp.t * Trace.t
  [@@deriving compare, equal, variants]

  type rank = int

  let to_rank = Variants.to_rank

  let address_of_stack_variable_rank = Variants.addressofstackvariable.rank

  let allocated_rank = Variants.allocated.rank

  let always_reachable_rank = Variants.alwaysreachable.rank

  let closure_rank = Variants.closure.rank

  let config_usage_rank = Variants.configusage.rank

  let copied_into_rank = Variants.copiedinto.rank

  let copied_return_rank = Variants.copiedreturn.rank

  let copy_origin_rank = Variants.sourceoriginofcopy.rank

  let dict_contain_const_keys_rank = Variants.dictcontainconstkeys.rank

  let dict_read_const_keys_rank = Variants.dictreadconstkeys.rank

  let end_of_collection_rank = Variants.endofcollection.rank

  let hack_builder_rank = Variants.hackbuilder.rank

  let hack_sinit_called_rank = Variants.hacksinitcalled.rank

  let in_reported_retain_cycle_rank = Variants.inreportedretaincycle.rank

  let initialized_rank = Variants.initialized.rank

  let invalid_rank = Variants.invalid.rank

  let java_resource_released_rank = Variants.javaresourcereleased.rank

  let last_lookup_rank = Variants.lastlookup.rank

  let hack_async_awaited_rank = Variants.hackasyncawaited.rank

  let csharp_resource_released_rank = Variants.csharpresourcereleased.rank

  let must_be_initialized_rank = Variants.mustbeinitialized.rank

  let must_be_valid_rank = Variants.mustbevalid.rank

  let must_not_be_tainted_rank = Variants.mustnotbetainted.rank

  let propagate_taint_from_rank = Variants.propagatetaintfrom.rank

  let returned_from_unknown = Variants.returnedfromunknown.rank

  let static_type_rank = Variants.statictype.rank

  let std_moved_rank = Variants.stdmoved.rank

  let std_vector_reserve_rank = Variants.stdvectorreserve.rank

  let taint_sanitized_rank = Variants.taintsanitized.rank

  let tainted_rank = Variants.tainted.rank

  let uninitialized_rank = Variants.uninitialized.rank

  let unknown_effect_rank = Variants.unknowneffect.rank

  let unreachable_at_rank = Variants.unreachableat.rank

  let used_as_branch_cond_rank = Variants.usedasbranchcond.rank

  let written_to_rank = Variants.writtento.rank

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
    | ConfigUsage config ->
        F.fprintf f "ConfigUsage (%a)" ConfigUsage.pp config
    | CopiedInto copied_into ->
        CopiedInto.pp f copied_into
    | CopiedReturn {source; is_const_ref; from; copied_location} ->
        F.fprintf f "CopiedReturn (%a%t by %a at %a)" AbstractValue.pp source
          (fun f -> if is_const_ref then F.pp_print_string f ":const&")
          CopyOrigin.pp from Location.pp copied_location
    | DictContainConstKeys ->
        F.pp_print_string f "DictContainConstKeys"
    | DictReadConstKeys keys ->
        F.fprintf f "DictReadConstKeys(@[%a@])" ConstKeys.pp keys
    | EndOfCollection ->
        F.pp_print_string f "EndOfCollection"
    | HackBuilder builderstate ->
        F.fprintf f "HackBuilder(%a)" Builder.pp builderstate
    | HackSinitCalled ->
        F.pp_print_string f "HackSinitCalled"
    | InReportedRetainCycle ->
        F.pp_print_string f "InReportedRetainCycle"
    | Initialized ->
        F.pp_print_string f "Initialized"
    | Invalid (invalidation, trace) ->
        F.fprintf f "Invalid %a"
          (Trace.pp ~pp_immediate:(fun fmt -> Invalidation.pp fmt invalidation))
          trace
    | LastLookup value ->
        F.fprintf f "LastLookup(%a)" AbstractValue.pp value
    | MustBeInitialized (timestamp, trace) ->
        F.fprintf f "MustBeInitialized(@[@[%a@],@;t=%d@])"
          (Trace.pp ~pp_immediate:(pp_string_if_debug "read"))
          trace
          (timestamp :> int)
    | MustBeValid (timestamp, trace, reason) ->
        F.fprintf f "MustBeValid(@[@[%a@],@;@[%a@],@;t=%d@])"
          (Trace.pp ~pp_immediate:(pp_string_if_debug "access"))
          trace Invalidation.pp_must_be_valid_reason reason
          (timestamp :> int)
    | MustNotBeTainted sinks ->
        F.fprintf f "MustNotBeTainted%a" (TaintSinkMap.pp ~pp_value:TaintSink.pp) sinks
    | JavaResourceReleased ->
        F.pp_print_string f "Released"
    | CSharpResourceReleased ->
        F.pp_print_string f "Released"
    | HackAsyncAwaited ->
        F.pp_print_string f "Awaited"
    | PropagateTaintFrom (reason, taints_in) ->
        F.fprintf f "PropagateTaintFrom(%a, [%a])" pp_taint_propagation_reason reason
          (Pp.seq ~sep:";" pp_taint_in) taints_in
    | ReturnedFromUnknown values ->
        F.fprintf f "ReturnedFromUnknown([%a])" (Pp.seq ~sep:";" AbstractValue.pp) values
    | SourceOriginOfCopy {source; is_const_ref} ->
        F.fprintf f "copied of source %a" AbstractValue.pp source ;
        if is_const_ref then F.pp_print_string f " (const&)"
    | StaticType type_name ->
        F.fprintf f "StaticType %a" Typ.Name.pp type_name
    | StdMoved ->
        F.pp_print_string f "std::move()"
    | StdVectorReserve ->
        F.pp_print_string f "std::vector::reserve()"
    | Tainted tainted ->
        F.fprintf f "Tainted%a" TaintedSet.pp tainted
    | TaintSanitized taint_sanitized ->
        F.fprintf f "TaintedSanitized%a" TaintSanitizedSet.pp taint_sanitized
    | Uninitialized typ ->
        F.fprintf f "Uninitialized(%a)" UninitializedTyp.pp typ
    | UnknownEffect (call, hist) ->
        F.fprintf f "UnknownEffect(@[%a,@ %a)@]" CallEvent.pp call ValueHistory.pp hist
    | UnreachableAt location ->
        F.fprintf f "UnreachableAt(%a)" Location.pp location
    | UsedAsBranchCond (pname, location, trace) ->
        F.fprintf f "UsedAsBranchCond(%a, %a, %a)" Procname.pp pname Location.pp location
          (Trace.pp ~pp_immediate:(pp_string_if_debug "used"))
          trace
    | WrittenTo (timestamp, trace) ->
        F.fprintf f "WrittenTo (%d, %a)"
          (timestamp :> int)
          (Trace.pp ~pp_immediate:(pp_string_if_debug "mutation"))
          trace


  let is_suitable_for_pre = function
    | DictReadConstKeys _
    | MustBeValid _
    | MustBeInitialized _
    | MustNotBeTainted _
    | UsedAsBranchCond _ ->
        true
    | Invalid _
    | Allocated _
    | AddressOfCppTemporary _
    | AddressOfStackVariable _
    | AlwaysReachable
    | Closure _
    | ConfigUsage _
    | CopiedInto _
    | CopiedReturn _
    | DictContainConstKeys
    | EndOfCollection
    | HackSinitCalled
    | InReportedRetainCycle
    | Initialized
    | JavaResourceReleased
    | LastLookup _
    | CSharpResourceReleased
    | HackAsyncAwaited
    | HackBuilder _ (* TODO: right choice? Planning on doing on the outside in pulse call/return *)
    | PropagateTaintFrom _
    | ReturnedFromUnknown _
    | SourceOriginOfCopy _
    | StaticType _
    | StdMoved
    | StdVectorReserve
    | Tainted _
    | TaintSanitized _
    | Uninitialized _
    | UnknownEffect _
    | UnreachableAt _
    | WrittenTo _ ->
        false


  let is_suitable_for_pre_summary = is_suitable_for_pre

  let is_suitable_for_post = function
    | DictReadConstKeys _
    | MustBeInitialized _
    | MustNotBeTainted _
    | MustBeValid _
    | UnreachableAt _
    | UsedAsBranchCond _ ->
        false
    | AddressOfCppTemporary _
    | AddressOfStackVariable _
    | Allocated _
    | AlwaysReachable
    | Closure _
    | ConfigUsage _
    | CopiedInto _
    | CopiedReturn _
    | DictContainConstKeys
    | EndOfCollection
    | HackSinitCalled
    | InReportedRetainCycle
    | Initialized
    | Invalid _
    | JavaResourceReleased
    | LastLookup _
    | CSharpResourceReleased
    | HackAsyncAwaited
    | HackBuilder _ (* TODO: right choice again? *)
    | PropagateTaintFrom _
    | ReturnedFromUnknown _
    | SourceOriginOfCopy _
    | StaticType _
    | StdMoved
    | StdVectorReserve
    | Tainted _
    | TaintSanitized _
    | Uninitialized _
    | UnknownEffect _
    | WrittenTo _ ->
        true


  let is_suitable_for_post_summary attr = is_suitable_for_post attr

  let make_suitable_for_summary attr =
    match attr with
    | CopiedInto _ | SourceOriginOfCopy _ ->
        None
    | Tainted tainted ->
        let tainted' =
          TaintedSet.filter (fun {intra_procedural_only} -> not intra_procedural_only) tainted
        in
        if TaintedSet.is_empty tainted' then None else Some (Tainted tainted')
    | AddressOfCppTemporary _
    | AddressOfStackVariable _
    | Allocated _
    | AlwaysReachable
    | Closure _
    | ConfigUsage _
    | CopiedReturn _
    | DictContainConstKeys
    | DictReadConstKeys _
    | EndOfCollection
    | InReportedRetainCycle
    | Initialized
    | Invalid _
    | JavaResourceReleased
    | LastLookup _
    | CSharpResourceReleased
    | HackAsyncAwaited
    | HackBuilder _
    | HackSinitCalled
    | MustBeInitialized _
    | MustBeValid _
    | MustNotBeTainted _
    | PropagateTaintFrom _
    | ReturnedFromUnknown _
    | StaticType _
    | StdMoved
    | StdVectorReserve
    | TaintSanitized _
    | Uninitialized _
    | UnknownEffect _
    | UnreachableAt _
    | UsedAsBranchCond _
    | WrittenTo _ ->
        Some attr


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
    | ConfigUsage (StringParam {v; config_type}) ->
        ConfigUsage (StringParam {v= subst v; config_type})
    | CopiedReturn {source; is_const_ref; from; copied_location} ->
        CopiedReturn {source= subst source; is_const_ref; from; copied_location}
    | DictReadConstKeys const_keys ->
        DictReadConstKeys
          (ConstKeys.map
             (fun (_timestamp, trace) -> (timestamp, add_call_to_trace trace))
             const_keys )
    | InReportedRetainCycle ->
        InReportedRetainCycle
    | Invalid (invalidation, trace) ->
        Invalid (invalidation, add_call_to_trace trace)
    | MustBeValid (_timestamp, trace, reason) ->
        MustBeValid (timestamp, add_call_to_trace trace, reason)
    | MustBeInitialized (_timestamp, trace) ->
        MustBeInitialized (timestamp, add_call_to_trace trace)
    | MustNotBeTainted sinks ->
        let add_call_to_sink taint_sink =
          TaintSink.{taint_sink with trace= add_call_to_trace taint_sink.trace}
        in
        MustNotBeTainted (TaintSinkMap.map add_call_to_sink sinks)
    | PropagateTaintFrom (reason, taints_in) ->
        let add_propagation_event_to_history hist =
          let hist = add_call_to_history hist in
          let propagation_event = ValueHistory.TaintPropagated (call_location, timestamp) in
          ValueHistory.sequence propagation_event hist
        in
        PropagateTaintFrom
          ( reason
          , List.map taints_in ~f:(fun {v; history} ->
                {v= subst v; history= add_propagation_event_to_history history} ) )
    | ReturnedFromUnknown values ->
        ReturnedFromUnknown (List.map values ~f:subst)
    | Tainted tainted ->
        let add_call_to_tainted Tainted.{source; time_trace; hist; intra_procedural_only} =
          if intra_procedural_only then
            L.die InternalError "Unexpected attribute %a in the summary of %a" pp attr Procname.pp
              proc_name
          else
            Tainted.
              { source
              ; time_trace= Timestamp.add_to_trace time_trace timestamp
              ; hist= add_call_to_history hist
              ; intra_procedural_only }
        in
        Tainted (TaintedSet.map add_call_to_tainted tainted)
    | TaintSanitized taint_sanitized ->
        let add_call_to_taint_sanitized TaintSanitized.{sanitizer; time_trace; trace} =
          TaintSanitized.
            { sanitizer
            ; time_trace= Timestamp.add_to_trace time_trace timestamp
            ; trace= add_call_to_trace trace }
        in
        TaintSanitized (TaintSanitizedSet.map add_call_to_taint_sanitized taint_sanitized)
    | UnknownEffect (call, hist) ->
        UnknownEffect (call, add_call_to_history hist)
    | WrittenTo (_timestamp, trace) ->
        WrittenTo (timestamp, add_call_to_trace trace)
    | CopiedInto _ | SourceOriginOfCopy _ ->
        L.die InternalError "Unexpected attribute %a in the summary of %a" pp attr Procname.pp
          proc_name
    | UsedAsBranchCond (pname, location, trace) ->
        UsedAsBranchCond (pname, location, add_call_to_trace trace)
    | ( AddressOfCppTemporary _
      | AddressOfStackVariable _
      | AlwaysReachable
      | Closure _
      | ConfigUsage (ConfigName _)
      | CSharpResourceReleased
      | DictContainConstKeys
      | EndOfCollection
      | HackAsyncAwaited
      | HackBuilder _
      | HackSinitCalled
      | Initialized
      | JavaResourceReleased
      | LastLookup _
      | StaticType _
      | StdMoved
      | StdVectorReserve
      | Uninitialized _
      | UnreachableAt _ ) as attr ->
        attr


  let alloc_free_match allocator (invalidation : (Invalidation.t * Trace.t) option) is_released =
    match (allocator, invalidation) with
    | (CMalloc | CustomMalloc _ | CRealloc | CustomRealloc _), Some (CFree, _)
    | CppNew, Some (CppDelete, _)
    | CppNewArray, Some (CppDeleteArray, _)
    | ObjCAlloc, _ ->
        true
    | JavaResource _, _ | CSharpResource _, _ | HackAsync, _ | HackBuilderResource _, _ ->
        is_released
    | _ ->
        false


  let is_hack_resource allocator =
    match allocator with
    | CMalloc
    | CustomMalloc _
    | CRealloc
    | CustomRealloc _
    | CppNew
    | CppNewArray
    | ObjCAlloc
    | JavaResource _
    | CSharpResource _ ->
        false
    | HackAsync | HackBuilderResource _ ->
        true


  let filter_unreachable subst f_keep attr =
    let filter_aux things ~get_addr ~set_addr =
      let module Hashtbl = Stdlib.Hashtbl in
      let to_keep = Hashtbl.create 17 in
      let filter_thing thing =
        let addr = get_addr thing in
        if f_keep addr then Hashtbl.replace to_keep thing ()
        else
          match AbstractValue.Map.find_opt addr subst with
          | Some subst_addrs ->
              AbstractValue.Set.iter
                (fun subst_addr ->
                  let thing' = set_addr subst_addr thing in
                  Hashtbl.replace to_keep thing' () )
                subst_addrs
          | None ->
              ()
      in
      List.iter things ~f:filter_thing ;
      if Hashtbl.length to_keep |> Int.equal 0 then None
      else Some (Hashtbl.to_seq_keys to_keep |> Stdlib.List.of_seq)
    in
    match attr with
    | ConfigUsage (StringParam {v= source}) | CopiedReturn {source} ->
        Option.some_if (f_keep source) attr
    | PropagateTaintFrom (reason, taints_in) ->
        filter_aux taints_in ~get_addr:(fun {v} -> v) ~set_addr:(fun v thing -> {thing with v})
        |> Option.map ~f:(fun taints_in -> PropagateTaintFrom (reason, taints_in))
    | ReturnedFromUnknown values ->
        filter_aux values ~get_addr:Fn.id ~set_addr:(fun v _ -> v)
        |> Option.map ~f:(fun values -> ReturnedFromUnknown values)
    | MustNotBeTainted sinks when TaintSinkMap.is_empty sinks ->
        L.die InternalError "Unexpected attribute %a." pp attr
    | Tainted set when TaintedSet.is_empty set ->
        L.die InternalError "Unexpected attribute %a." pp attr
    | TaintSanitized set when TaintSanitizedSet.is_empty set ->
        L.die InternalError "Unexpected attribute %a." pp attr
    | ( AddressOfCppTemporary _
      | AddressOfStackVariable _
      | Allocated _
      | AlwaysReachable
      | Closure _
      | ConfigUsage (ConfigName _)
      | CopiedInto _
      | CSharpResourceReleased
      | DictContainConstKeys
      | DictReadConstKeys _
      | EndOfCollection
      | HackAsyncAwaited
      | HackBuilder _
      | HackSinitCalled
      | InReportedRetainCycle
      | Initialized
      | Invalid _
      | JavaResourceReleased
      | LastLookup _
      | MustBeInitialized _
      | MustBeValid _
      | MustNotBeTainted _
      | SourceOriginOfCopy _
      | StaticType _
      | StdMoved
      | StdVectorReserve
      | Tainted _
      | TaintSanitized _
      | Uninitialized _
      | UnknownEffect _
      | UnreachableAt _
      | UsedAsBranchCond _
      | WrittenTo _ ) as attr ->
        Some attr


  let is_not_taint_related_attribute = function
    | MustNotBeTainted _ | PropagateTaintFrom _ | Tainted _ | TaintSanitized _ ->
        false
    | _ ->
        true
end

module Attributes = struct
  module Set = struct
    include PrettyPrintable.MakePPUniqRankSet (Int) (Attribute)

    let get_by_rank rank ~dest attrs = find_rank attrs rank |> Option.map ~f:dest

    let get_dict_read_const_keys attrs =
      get_by_rank Attribute.dict_read_const_keys_rank
        ~dest:(function[@warning "-partial-match"] DictReadConstKeys keys -> keys)
        attrs


    let get_tainted attrs =
      get_by_rank Attribute.tainted_rank
        ~dest:(function[@warning "-partial-match"] Tainted tainted -> tainted)
        attrs
      |> Option.value ~default:Attribute.TaintedSet.empty


    let remove_tainted = remove_by_rank Attribute.tainted_rank

    let get_taint_sanitized attrs =
      get_by_rank Attribute.taint_sanitized_rank
        ~dest:(function[@warning "-partial-match"]
          | TaintSanitized taint_sanitized -> taint_sanitized )
        attrs
      |> Option.value ~default:Attribute.TaintSanitizedSet.empty


    let remove_taint_sanitized = remove_by_rank Attribute.taint_sanitized_rank

    let get_must_not_be_tainted attrs =
      get_by_rank Attribute.must_not_be_tainted_rank
        ~dest:(function[@warning "-partial-match"] MustNotBeTainted sinks -> sinks)
        attrs
      |> Option.value ~default:Attribute.TaintSinkMap.empty


    let add attrs value =
      let open Attribute in
      match value with
      | DictReadConstKeys keys ->
          if ConstKeys.is_empty keys then attrs
          else
            let existing_set =
              Option.value ~default:Attribute.ConstKeys.empty (get_dict_read_const_keys attrs)
            in
            update (DictReadConstKeys (ConstKeys.union existing_set keys)) attrs
      | Tainted new_set ->
          if TaintedSet.is_empty new_set then attrs
          else
            let existing_set = get_tainted attrs in
            update (Tainted (TaintedSet.union new_set existing_set)) attrs
      | TaintSanitized new_set ->
          if TaintSanitizedSet.is_empty new_set then attrs
          else
            let existing_set = get_taint_sanitized attrs in
            update (TaintSanitized (TaintSanitizedSet.union new_set existing_set)) attrs
      | MustNotBeTainted new_sinks ->
          if TaintSinkMap.is_empty new_sinks then attrs
          else
            (* If we find the same kind twice we keep the sink for which compare_value_tuple
               is smaller. In particular we want to keep whichever one is Basic. *)
            let sinks = get_must_not_be_tainted attrs in
            let aux _kind (sink1 : TaintSink.t) (sink2 : TaintSink.t) =
              if TaintItem.compare_value_tuple sink1.sink sink2.sink < 0 then Some sink1
              else Some sink2
            in
            update (MustNotBeTainted (TaintSinkMap.union aux new_sinks sinks)) attrs
      | Invalid (OptionalEmpty, _) | WrittenTo _ ->
          update value attrs
      | _ ->
          add attrs value


    let remove_must_not_be_tainted ?kinds attrs =
      let open Attribute in
      match kinds with
      | None ->
          remove_by_rank Attribute.must_not_be_tainted_rank attrs
      | Some kinds_to_remove -> (
          let taint_map =
            get_by_rank Attribute.must_not_be_tainted_rank attrs
                ~dest:(function [@warning "-partial-match"] MustNotBeTainted map -> map)
          in
          match taint_map with
          | None ->
              attrs
          | Some taint_map ->
              let filtered_map =
                TaintSinkMap.filter
                  (fun kind _ -> TaintConfig.Kind.Set.mem kind kinds_to_remove |> not)
                  taint_map
              in
              update (MustNotBeTainted filtered_map) attrs )
  end

  let get_by_rank = Set.get_by_rank

  let remove_by_rank = Set.remove_by_rank

  let mem_by_rank rank attrs = Set.find_rank attrs rank |> Option.is_some

  let is_in_reported_retain_cycle = mem_by_rank Attribute.in_reported_retain_cycle_rank

  let get_invalid =
    get_by_rank Attribute.invalid_rank ~dest:(function [@warning "-partial-match"]
        | Invalid (invalidation, trace) -> (invalidation, trace) )


  let get_propagate_taint_from =
    get_by_rank Attribute.propagate_taint_from_rank ~dest:(function [@warning "-partial-match"]
        | PropagateTaintFrom (reason, taints_in) -> (reason, taints_in) )


  let remove_propagate_taint_from = remove_by_rank Attribute.propagate_taint_from_rank

  let get_returned_from_unknown =
    get_by_rank Attribute.returned_from_unknown ~dest:(function [@warning "-partial-match"]
        | ReturnedFromUnknown values -> values )


  let is_java_resource_released = mem_by_rank Attribute.java_resource_released_rank

  let is_hack_async_awaited = mem_by_rank Attribute.hack_async_awaited_rank

  let get_hack_builder =
    get_by_rank Attribute.hack_builder_rank ~dest:(function [@warning "-partial-match"]
        | Attribute.HackBuilder builderstate -> builderstate )


  let is_hack_builder_discardable s =
    match get_hack_builder s with
    | None ->
        false
    | Some Discardable ->
        true
    | Some NonDiscardable ->
        false


  let remove_hack_builder = remove_by_rank Attribute.hack_builder_rank

  let is_hack_sinit_called = mem_by_rank Attribute.hack_sinit_called_rank

  let is_csharp_resource_released = mem_by_rank Attribute.csharp_resource_released_rank

  let get_must_be_valid =
    get_by_rank Attribute.must_be_valid_rank ~dest:(function [@warning "-partial-match"]
        | Attribute.MustBeValid (timestamp, trace, reason) -> (timestamp, trace, reason) )


  let remove_must_be_valid = remove_by_rank Attribute.must_be_valid_rank

  let get_written_to =
    get_by_rank Attribute.written_to_rank ~dest:(function [@warning "-partial-match"]
        | WrittenTo (timestamp, trace) -> (timestamp, trace) )


  let get_closure_proc_name =
    get_by_rank Attribute.closure_rank ~dest:(function [@warning "-partial-match"]
        | Closure proc_name -> proc_name )


  let get_config_usage =
    get_by_rank Attribute.config_usage_rank ~dest:(function [@warning "-partial-match"]
        | ConfigUsage config -> config )


  let get_used_as_branch_cond =
    get_by_rank Attribute.used_as_branch_cond_rank ~dest:(function [@warning "-partial-match"]
        | UsedAsBranchCond (pname, location, trace) -> (pname, location, trace) )


  let get_copied_into =
    get_by_rank Attribute.copied_into_rank ~dest:(function [@warning "-partial-match"]
        | CopiedInto copied_into -> copied_into )


  let get_copied_return =
    get_by_rank Attribute.copied_return_rank ~dest:(function [@warning "-partial-match"]
        | CopiedReturn {source; is_const_ref; from; copied_location} ->
        (source, is_const_ref, from, copied_location) )


  let remove_copied_return = remove_by_rank Attribute.copied_return_rank

  let get_source_origin_of_copy =
    get_by_rank Attribute.copy_origin_rank ~dest:(function [@warning "-partial-match"]
        | SourceOriginOfCopy {source; is_const_ref} -> (source, is_const_ref) )


  let get_address_of_stack_variable =
    get_by_rank Attribute.address_of_stack_variable_rank ~dest:(function [@warning "-partial-match"]
        | AddressOfStackVariable (var, loc, history) -> (var, loc, history) )


  let is_end_of_collection = mem_by_rank Attribute.end_of_collection_rank

  let is_std_moved = mem_by_rank Attribute.std_moved_rank

  let is_std_vector_reserved = mem_by_rank Attribute.std_vector_reserve_rank

  let get_last_lookup =
    get_by_rank Attribute.last_lookup_rank ~dest:(function [@warning "-partial-match"]
        | LastLookup value -> value )


  let is_modified attrs =
    mem_by_rank Attribute.written_to_rank attrs
    || mem_by_rank Attribute.initialized_rank attrs
    || mem_by_rank Attribute.invalid_rank attrs
    || mem_by_rank Attribute.unknown_effect_rank attrs
    || mem_by_rank Attribute.java_resource_released_rank attrs
    || mem_by_rank Attribute.hack_async_awaited_rank attrs
    || mem_by_rank Attribute.hack_builder_rank attrs
    || mem_by_rank Attribute.csharp_resource_released_rank attrs
    || mem_by_rank Attribute.propagate_taint_from_rank attrs


  let is_always_reachable = mem_by_rank Attribute.always_reachable_rank

  let get_uninitialized attrs =
    if not (mem_by_rank Attribute.initialized_rank attrs) then
      get_by_rank Attribute.uninitialized_rank
        ~dest:(function[@warning "-partial-match"] Uninitialized typ -> typ)
        attrs
    else None


  let remove_uninitialized = remove_by_rank Attribute.uninitialized_rank

  let get_allocation =
    get_by_rank Attribute.allocated_rank ~dest:(function [@warning "-partial-match"]
        | Allocated (allocator, trace) -> (allocator, trace) )


  let remove_allocation = remove_by_rank Attribute.allocated_rank

  let get_unknown_effect =
    get_by_rank Attribute.unknown_effect_rank ~dest:(function [@warning "-partial-match"]
        | UnknownEffect (call, hist) -> (call, hist) )


  let remove_dict_contain_const_keys = remove_by_rank Attribute.dict_contain_const_keys_rank

  let is_dict_contain_const_keys = mem_by_rank Attribute.dict_contain_const_keys_rank

  let get_must_be_initialized =
    get_by_rank Attribute.must_be_initialized_rank ~dest:(function [@warning "-partial-match"]
        | MustBeInitialized (timestamp, trace) -> (timestamp, trace) )


  let get_static_type =
    get_by_rank Attribute.static_type_rank ~dest:(function [@warning "-partial-match"]
        | StaticType typ -> typ )


  let get_unreachable_at =
    get_by_rank Attribute.unreachable_at_rank ~dest:(function [@warning "-partial-match"]
        | UnreachableAt location -> location )


  let add_call_and_subst subst timestamp proc_name call_location caller_history attrs =
    Set.map attrs ~f:(fun attr ->
        Attribute.add_call_and_subst subst timestamp proc_name call_location caller_history attr )


  let get_allocated_not_freed attributes =
    let allocated_opt = get_allocation attributes in
    Option.value_map ~default:None allocated_opt ~f:(fun (allocator, _) ->
        let invalidation = get_invalid attributes in
        let is_released =
          is_java_resource_released attributes
          || is_csharp_resource_released attributes
          || is_hack_async_awaited attributes
          || is_hack_builder_discardable
               attributes (* Not entirely sure about the definition of this *)
        in
        if Attribute.alloc_free_match allocator invalidation is_released then None
        else allocated_opt )


  let make_suitable_for_summary is_ok attributes =
    let f attr = if is_ok attr then Attribute.make_suitable_for_summary attr else None in
    Set.filter_map ~f attributes


  let make_suitable_for_pre_summary attributes =
    make_suitable_for_summary Attribute.is_suitable_for_pre_summary attributes


  let make_suitable_for_post_summary attributes =
    make_suitable_for_summary Attribute.is_suitable_for_post_summary attributes


  let remove_all_taint_related attributes =
    Set.filter attributes ~f:Attribute.is_not_taint_related_attribute


  include Set
end

include Attribute
