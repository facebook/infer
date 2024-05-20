(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module AbstractValue = PulseAbstractValue

type ('fieldname, 'array_index) access_ =
  | FieldAccess of 'fieldname
  | ArrayAccess of (Typ.t[@ignore]) * 'array_index
  | Dereference
[@@deriving compare, equal, yojson_of]

type 'array_index access = (Fieldname.t, 'array_index) access_
[@@deriving compare, equal, yojson_of]

let loose_compare compare_array_index = compare_access_ Fieldname.compare_name compare_array_index

let pp_access pp_array_index fmt = function
  | FieldAccess field_name ->
      Fieldname.pp fmt field_name
  | ArrayAccess (_, index) ->
      F.fprintf fmt "[%a]" pp_array_index index
  | Dereference ->
      F.pp_print_string fmt "*"


module type S = sig
  type key

  include PrettyPrintable.PrintableEquatableOrderedType with type t = key access

  val canonicalize : get_var_repr:(PulseAbstractValue.t -> PulseAbstractValue.t) -> t -> t

  val yojson_of_t : t -> Yojson.Safe.t

  module Set : Caml.Set.S with type elt = t
end

module T = struct
  type nonrec t = AbstractValue.t access [@@deriving yojson_of]

  let compare = loose_compare AbstractValue.compare

  let equal = [%compare.equal: t]

  let pp = pp_access AbstractValue.pp

  let canonicalize ~get_var_repr (access : t) =
    match access with
    | ArrayAccess (typ, addr) ->
        let addr' = get_var_repr addr in
        if AbstractValue.equal addr addr' then access else ArrayAccess (typ, addr')
    | FieldAccess _ | Dereference ->
        access
end

include T
module Set = Caml.Set.Make (T)
