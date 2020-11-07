(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Uninterpreted Predicate Symbols *)

type t [@@deriving compare, equal, hash, sexp]

val pp : t pp
val uninterp : string -> t
