(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type timestamp = private int [@@deriving compare]

val t0 : timestamp

type t = {timestamp: timestamp  (** step number *)}

val leq : lhs:t -> rhs:t -> bool

val initial : t

val post_exec_instr : t -> t
(** call this after each step of the symbolic execution to update the path information *)

val pp : F.formatter -> t -> unit
