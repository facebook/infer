(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Function-wide classifier for the swift_refcounted metadata-extract idiom. See the [.ml] for
    context. *)

open! IStd
module ProcState = Llair2TextualState.ProcState

type t

val classify : proc_state:ProcState.t -> Llair.func -> t
(** Scan [func] and return the set of [Reg.id]s for formal-parameter [Reg] [Load]s whose result is
    used *only* as the value operand of one or more [Store]s — never as a Field base, GEP base, call
    arg, or terminator expression. Empty for any function with no formal-Reg [Load]s. *)

val mem : Llair.Reg.t -> t -> bool
(** [true] if the given [Reg] (the destination of a [Load]) is classified as a metadata-extract
    load. *)

val is_empty : t -> bool

val empty : t
