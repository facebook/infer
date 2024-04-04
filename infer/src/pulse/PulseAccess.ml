(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module AbstractValue = PulseAbstractValue

module type S = sig
  type key

  include PrettyPrintable.PrintableEquatableOrderedType with type t = key MemoryAccess.t

  val canonicalize : get_var_repr:(PulseAbstractValue.t -> PulseAbstractValue.t) -> t -> t

  val yojson_of_t : t -> Yojson.Safe.t

  module Set : Caml.Set.S with type elt = t
end

module T = struct
  type t = AbstractValue.t MemoryAccess.t [@@deriving yojson_of]

  let compare = MemoryAccess.loose_compare AbstractValue.compare

  let equal = [%compare.equal: t]

  let pp = MemoryAccess.pp AbstractValue.pp

  let canonicalize ~get_var_repr (access : t) =
    match access with
    | ArrayAccess (typ, addr) ->
        let addr' = get_var_repr addr in
        if AbstractValue.equal addr addr' then access else MemoryAccess.ArrayAccess (typ, addr')
    | FieldAccess _ | TakeAddress | Dereference ->
        access
end

include T
module Set = Caml.Set.Make (T)
