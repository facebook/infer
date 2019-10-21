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

(** Make sure we don't depend on {!AbstractAddress} to avoid attributes depending on
   addresses. Otherwise they become a pain to handle when comparing memory states.

    If you find you need to make attributes depend on {!AbstractAddress} then remember to modify
    graph operations of {!PulseDomain} and the interprocedural operations in {!PulseAbductiveDomain}
*)
include struct
  [@@@warning "-60"]

  module AbstractAddress = struct end

  module PulseAbstractAddress = struct end
end

module Attribute = struct
  type t =
    | AddressOfCppTemporary of Var.t * ValueHistory.t
    | AddressOfStackVariable of Var.t * Location.t * ValueHistory.t
    | Closure of Typ.Procname.t
    | Constant of Const.t
    | Invalid of Invalidation.t Trace.t
    | MustBeValid of unit Trace.t
    | StdVectorReserve
    | WrittenTo of unit Trace.t
  [@@deriving compare, variants]

  let equal = [%compare.equal: t]

  let to_rank = Variants.to_rank

  let mk_dummy_trace imm = Trace.Immediate {imm; location= Location.dummy; history= []}

  let closure_rank = Variants.to_rank (Closure (Typ.Procname.from_string_c_fun ""))

  let written_to_rank = Variants.to_rank (WrittenTo (mk_dummy_trace ()))

  let address_of_stack_variable_rank =
    let pname = Typ.Procname.from_string_c_fun "" in
    let var = Var.of_pvar (Pvar.mk (Mangled.from_string "") pname) in
    let location = Location.dummy in
    Variants.to_rank (AddressOfStackVariable (var, location, []))


  let invalid_rank = Variants.to_rank (Invalid (mk_dummy_trace Invalidation.Nullptr))

  let must_be_valid_rank = Variants.to_rank (MustBeValid (mk_dummy_trace ()))

  let std_vector_reserve_rank = Variants.to_rank StdVectorReserve

  let const_rank = Variants.to_rank (Constant (Const.Cint IntLit.zero))

  let pp f attribute =
    let pp_string_if_debug string fmt () =
      if Config.debug_level_analysis >= 3 then F.pp_print_string fmt string
    in
    match attribute with
    | AddressOfCppTemporary (var, history) ->
        F.fprintf f "t&%a (%a)" Var.pp var ValueHistory.pp history
    | AddressOfStackVariable (var, location, history) ->
        F.fprintf f "s&%a (%a) at %a" Var.pp var ValueHistory.pp history Location.pp location
    | Closure pname ->
        Typ.Procname.pp f pname
    | Constant c ->
        F.fprintf f "=%a" (Const.pp Pp.text) c
    | Invalid invalidation ->
        F.fprintf f "Invalid %a" (Trace.pp Invalidation.pp) invalidation
    | MustBeValid action ->
        F.fprintf f "MustBeValid %a" (Trace.pp (pp_string_if_debug "access")) action
    | StdVectorReserve ->
        F.pp_print_string f "std::vector::reserve()"
    | WrittenTo action ->
        F.fprintf f "WrittenTo %a" (Trace.pp (pp_string_if_debug "mutation")) action
end

module Attributes = struct
  module Set = PrettyPrintable.MakePPUniqRankSet (Attribute)

  let get_invalid attrs =
    Set.find_rank attrs Attribute.invalid_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.Invalid invalidation) = attr in
           invalidation )


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


  let get_constant attrs =
    Set.find_rank attrs Attribute.const_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.Constant c) = attr in
           c )


  include Set
end

include Attribute
