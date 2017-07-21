(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format
module L = Logging

(** a map from a formal to its positional index *)
type t

val make : Procdesc.t -> t
(** create a formal map for the given procdesc *)

val empty : t
(** the empty formal map *)

val is_formal : AccessPath.base -> t -> bool
(** return true if the given base var is a formal according to the given formal map *)

val get_formal_index : AccessPath.base -> t -> int option
(** return the index for the given base var if it is a formal, or None if it is not *)

val get_formal_base : int -> t -> AccessPath.base option
(** return the base var for the given index if it exists, or None if it does not. Note: this is
    linear in the size of the formal map *)
