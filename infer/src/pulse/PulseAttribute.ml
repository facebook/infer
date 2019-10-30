(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module Arithmetic = PulseArithmetic
module Invalidation = PulseInvalidation
module Trace = PulseTrace
module ValueHistory = PulseValueHistory

(** Make sure we don't depend on {!AbstractValue} to avoid attributes depending on
    values. Otherwise they become a pain to handle when comparing memory states.

    If you find you need to make attributes depend on {!AbstractValue} then remember to modify
    graph operations of {!PulseDomain} and the interprocedural operations in {!PulseAbductiveDomain}
*)
include struct
  [@@@warning "-60"]

  module AbstractValue = struct end

  module PulseAbstractValue = struct end
end

module Attribute = struct
  type t =
    | AddressOfCppTemporary of Var.t * ValueHistory.t
    | AddressOfStackVariable of Var.t * Location.t * ValueHistory.t
    | Arithmetic of Arithmetic.t * Trace.t
    | Closure of Typ.Procname.t
    | Invalid of Invalidation.t * Trace.t
    | MustBeValid of Trace.t
    | StdVectorReserve
    | WrittenTo of Trace.t
  [@@deriving compare, variants]

  let equal = [%compare.equal: t]

  let to_rank = Variants.to_rank

  let dummy_trace = Trace.Immediate {location= Location.dummy; history= []}

  let closure_rank = Variants.to_rank (Closure (Typ.Procname.from_string_c_fun ""))

  let written_to_rank = Variants.to_rank (WrittenTo dummy_trace)

  let address_of_stack_variable_rank =
    let pname = Typ.Procname.from_string_c_fun "" in
    let var = Var.of_pvar (Pvar.mk (Mangled.from_string "") pname) in
    let location = Location.dummy in
    Variants.to_rank (AddressOfStackVariable (var, location, []))


  let invalid_rank = Variants.to_rank (Invalid (Invalidation.Nullptr, dummy_trace))

  let must_be_valid_rank = Variants.to_rank (MustBeValid dummy_trace)

  let std_vector_reserve_rank = Variants.to_rank StdVectorReserve

  let const_rank = Variants.to_rank (Arithmetic (Arithmetic.equal_to IntLit.zero, dummy_trace))

  let pp f attribute =
    let pp_string_if_debug string fmt =
      if Config.debug_level_analysis >= 3 then F.pp_print_string fmt string
    in
    match attribute with
    | AddressOfCppTemporary (var, history) ->
        F.fprintf f "t&%a (%a)" Var.pp var ValueHistory.pp history
    | AddressOfStackVariable (var, location, history) ->
        F.fprintf f "s&%a (%a) at %a" Var.pp var ValueHistory.pp history Location.pp location
    | Closure pname ->
        Typ.Procname.pp f pname
    | Arithmetic (phi, trace) ->
        F.fprintf f "Arith %a" (Trace.pp ~pp_immediate:(fun fmt -> Arithmetic.pp fmt phi)) trace
    | Invalid (invalidation, trace) ->
        F.fprintf f "Invalid %a"
          (Trace.pp ~pp_immediate:(fun fmt -> Invalidation.pp fmt invalidation))
          trace
    | MustBeValid trace ->
        F.fprintf f "MustBeValid %a" (Trace.pp ~pp_immediate:(pp_string_if_debug "access")) trace
    | StdVectorReserve ->
        F.pp_print_string f "std::vector::reserve()"
    | WrittenTo trace ->
        F.fprintf f "WrittenTo %a" (Trace.pp ~pp_immediate:(pp_string_if_debug "mutation")) trace
end

module Attributes = struct
  module Set = PrettyPrintable.MakePPUniqRankSet (Attribute)

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


  let is_std_vector_reserved attrs =
    Set.find_rank attrs Attribute.std_vector_reserve_rank |> Option.is_some


  let is_modified attrs =
    Option.is_some (Set.find_rank attrs Attribute.written_to_rank)
    || Option.is_some (Set.find_rank attrs Attribute.invalid_rank)


  let get_arithmetic attrs =
    Set.find_rank attrs Attribute.const_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.Arithmetic (a, trace)) = attr in
           (a, trace) )


  include Set
end

include Attribute
