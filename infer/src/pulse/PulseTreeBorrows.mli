(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Abstract state of the Tree Borrows checker. *)
type state [@@deriving compare, equal]

val start : unit -> state

val pp : F.formatter -> state -> unit
