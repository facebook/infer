(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module Invalidation = PulseInvalidation
module Trace = PulseTrace
module ValueHistory = PulseValueHistory

(** Make sure we don't depend on {!AbstractValue} to avoid attributes depending on values. Otherwise
    they become a pain to handle when comparing memory states.

    If you find you need to make attributes depend on {!AbstractValue} then remember to modify graph
    operations of {!PulseDomain} and the interprocedural operations in {!PulseAbductiveDomain} *)
include struct
  [@@@warning "-60"]

  module AbstractValue = struct end

  module PulseAbstractValue = struct end
end

module Attribute = struct
  type t =
    | AddressOfCppTemporary of Var.t * ValueHistory.t
    | AddressOfStackVariable of Var.t * Location.t * ValueHistory.t
    | Allocated of Procname.t * Trace.t
    | Closure of Procname.t
    | DynamicType of Typ.t
    | EndOfCollection
    | Invalid of Invalidation.t * Trace.t
    | ISLAbduced of Trace.t
    | MustBeInitialized of Trace.t
    | MustBeValid of Trace.t
    | StdVectorReserve
    | Uninitialized
    | WrittenTo of Trace.t
  [@@deriving compare, variants]

  let equal = [%compare.equal: t]

  type rank = int

  let to_rank = Variants.to_rank

  let dummy_trace = Trace.Immediate {location= Location.dummy; history= []}

  let closure_rank = Variants.to_rank (Closure (Procname.from_string_c_fun ""))

  let written_to_rank = Variants.to_rank (WrittenTo dummy_trace)

  let address_of_stack_variable_rank =
    let pname = Procname.from_string_c_fun "" in
    let var = Var.of_pvar (Pvar.mk (Mangled.from_string "") pname) in
    let location = Location.dummy in
    Variants.to_rank (AddressOfStackVariable (var, location, []))


  let invalid_rank =
    Variants.to_rank (Invalid (Invalidation.ConstantDereference IntLit.zero, dummy_trace))


  let must_be_valid_rank = Variants.to_rank (MustBeValid dummy_trace)

  let std_vector_reserve_rank = Variants.to_rank StdVectorReserve

  let allocated_rank = Variants.to_rank (Allocated (Procname.Linters_dummy_method, dummy_trace))

  let dynamic_type_rank = Variants.to_rank (DynamicType StdTyp.void)

  let end_of_collection_rank = Variants.to_rank EndOfCollection

  let isl_abduced_rank = Variants.to_rank (ISLAbduced dummy_trace)

  let isl_subset attr1 attr2 =
    match (attr1, attr2) with
    | Invalid (v1, _), Invalid (v2, _) ->
        Invalidation.isl_equiv v1 v2
    | Invalid _, WrittenTo _ ->
        true
    | Uninitialized, Uninitialized ->
        true
    | (MustBeValid _ | Allocated _ | ISLAbduced _), Invalid _ ->
        false
    | Invalid _, _ | _, Uninitialized ->
        false
    | _ ->
        true


  let uninitialized_rank = Variants.to_rank Uninitialized

  let must_be_initialized_rank = Variants.to_rank (MustBeInitialized dummy_trace)

  let pp f attribute =
    let pp_string_if_debug string fmt =
      if Config.debug_level_analysis >= 3 then F.pp_print_string fmt string
    in
    match attribute with
    | AddressOfCppTemporary (var, history) ->
        F.fprintf f "t&%a (%a)" Var.pp var ValueHistory.pp history
    | AddressOfStackVariable (var, location, history) ->
        F.fprintf f "s&%a (%a) at %a" Var.pp var ValueHistory.pp history Location.pp location
    | Allocated (procname, trace) ->
        F.fprintf f "Allocated %a"
          (Trace.pp
             ~pp_immediate:(pp_string_if_debug ("allocation with " ^ Procname.to_string procname)))
          trace
    | Closure pname ->
        Procname.pp f pname
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
    | MustBeInitialized trace ->
        F.fprintf f "MustBeInitialized %a"
          (Trace.pp ~pp_immediate:(pp_string_if_debug "read"))
          trace
    | MustBeValid trace ->
        F.fprintf f "MustBeValid %a" (Trace.pp ~pp_immediate:(pp_string_if_debug "access")) trace
    | StdVectorReserve ->
        F.pp_print_string f "std::vector::reserve()"
    | Uninitialized ->
        F.pp_print_string f "Uninitialized"
    | WrittenTo trace ->
        F.fprintf f "WrittenTo %a" (Trace.pp ~pp_immediate:(pp_string_if_debug "mutation")) trace
end

module Attributes = struct
  module Set = PrettyPrintable.MakePPUniqRankSet (Int) (Attribute)

  let get_invalid attrs =
    Set.find_rank attrs Attribute.invalid_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.Invalid (invalidation, trace)) = attr in
           (invalidation, trace) )


  let get_must_be_valid attrs =
    Set.find_rank attrs Attribute.must_be_valid_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.MustBeValid action) = attr in
           action )


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


  let is_uninitialized attrs = Set.find_rank attrs Attribute.uninitialized_rank |> Option.is_some

  let get_allocation attrs =
    Set.find_rank attrs Attribute.allocated_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.Allocated (procname, trace)) = attr in
           (procname, trace) )


  let get_isl_abduced attrs =
    Set.find_rank attrs Attribute.isl_abduced_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.ISLAbduced trace) = attr in
           trace )


  let get_dynamic_type attrs =
    Set.find_rank attrs Attribute.dynamic_type_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.DynamicType typ) = attr in
           typ )


  let get_must_be_initialized attrs =
    Set.find_rank attrs Attribute.must_be_initialized_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.MustBeInitialized trace) = attr in
           trace )


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


  include Set
end

include Attribute

let is_suitable_for_pre = function
  | MustBeValid _ | MustBeInitialized _ ->
      true
  | Invalid _ | Allocated _ | ISLAbduced _ ->
      Config.pulse_isl
  | AddressOfCppTemporary _
  | AddressOfStackVariable _
  | Closure _
  | DynamicType _
  | EndOfCollection
  | StdVectorReserve
  | Uninitialized
  | WrittenTo _ ->
      false


let is_suitable_for_post = function
  | MustBeInitialized _ | MustBeValid _ ->
      false
  | Invalid _
  | Allocated _
  | ISLAbduced _
  | AddressOfCppTemporary _
  | AddressOfStackVariable _
  | Closure _
  | DynamicType _
  | EndOfCollection
  | StdVectorReserve
  | Uninitialized
  | WrittenTo _ ->
      true


let map_trace ~f = function
  | Allocated (procname, trace) ->
      Allocated (procname, f trace)
  | ISLAbduced trace ->
      ISLAbduced (f trace)
  | Invalid (invalidation, trace) ->
      Invalid (invalidation, f trace)
  | MustBeValid trace ->
      MustBeValid (f trace)
  | WrittenTo trace ->
      WrittenTo (f trace)
  | MustBeInitialized trace ->
      MustBeInitialized (f trace)
  | ( AddressOfCppTemporary _
    | AddressOfStackVariable _
    | Closure _
    | DynamicType _
    | EndOfCollection
    | StdVectorReserve
    | Uninitialized ) as attr ->
      attr
