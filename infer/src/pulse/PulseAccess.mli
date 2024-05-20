(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module AbstractValue = PulseAbstractValue

(** {1 Memory Accesses}

    The various kind of possible ways to access memory addresses and values. Can be used for edges
    in a memory graph representation or to represent values as access paths. *)

(** for internal use only, prefer using [access] *)
type ('fieldname, 'array_index) access_ =
  | FieldAccess of 'fieldname
  | ArrayAccess of Typ.t * 'array_index
  | Dereference

type 'array_index access = (Fieldname.t, 'array_index) access_
[@@deriving compare, equal, yojson_of]

val pp_access : (F.formatter -> 'array_index -> unit) -> F.formatter -> 'array_index access -> unit

module type S = sig
  type key

  include PrettyPrintable.PrintableEquatableOrderedType with type t = key access

  val canonicalize : get_var_repr:(AbstractValue.t -> AbstractValue.t) -> t -> t

  val yojson_of_t : t -> Yojson.Safe.t

  module Set : Caml.Set.S with type elt = t
end

include S with type key := AbstractValue.t
