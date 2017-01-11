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

(** create a formal map for the given procdesc *)
val make : Procdesc.t -> t

(** the empty formal map *)
val empty : t

(** return true if the given base var is a formal according to the given formal map *)
val is_formal : AccessPath.base -> t -> bool

(** return the index for the given base var if it is a formal, or None if it is not *)
val get_formal_index : AccessPath.base -> t -> int option
