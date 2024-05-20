(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** {1 Memory Accesses}

    The various kind of possible ways to access memory addresses and values. Can be used for edges
    in a memory graph representation or to represent values as access paths. *)

(** for internal use only, prefer using [t] *)
type ('fieldname, 'array_index) t_ =
  | FieldAccess of 'fieldname
  | ArrayAccess of Typ.t * 'array_index
  | TakeAddress
  | Dereference
[@@deriving compare, equal, yojson_of]

type 'array_index t = (Fieldname.t, 'array_index) t_ [@@deriving compare, equal, yojson_of]

val pp : (F.formatter -> 'array_index -> unit) -> F.formatter -> 'array_index t -> unit

val is_field_or_array_access : 'a t -> bool
