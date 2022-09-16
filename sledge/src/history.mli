(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** An execution history is a current instruction pointer and some
    predecessors. [preds] are empty iff this is an entrypoint. *)

type t = {curr: Llair.ip; preds: t iarray} [@@deriving sexp_of]

val init : Llair.ip -> t
val extend : Llair.ip -> t list -> t
val dump : t -> Format.formatter -> unit
