(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

(** An abstract value, eg an address in memory. *)

type t = private int [@@deriving compare]

val equal : t -> t -> bool

val mk_fresh : unit -> t

val pp : F.formatter -> t -> unit

val init : unit -> unit

type state

val get_state : unit -> state

val set_state : state -> unit

module Set : PrettyPrintable.PPSet with type elt = t

module Map : PrettyPrintable.PPMap with type key = t
