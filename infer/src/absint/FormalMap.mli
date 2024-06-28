(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** a map from a formal to its positional index *)
type t

val make : ProcAttributes.t -> t
(** create a formal map for the given procdesc *)

val is_formal : AccessPath.base -> t -> bool
(** return true if the given base var is a formal according to the given formal map *)

val get_formal_index : AccessPath.base -> t -> int option
(** return the index for the given base var if it is a formal, or None if it is not *)

val get_formal_base : int -> t -> AccessPath.base option
(** return the base var for the given index if it exists, or None if it does not. Note: this is
    linear in the size of the formal map *)

val pp : F.formatter -> t -> unit [@@warning "-unused-value-declaration"]

val cardinal : t -> int

val iter : (AccessPath.base -> int -> unit) -> t -> unit
