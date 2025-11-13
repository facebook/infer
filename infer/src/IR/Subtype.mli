(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The Smallfoot Intermediate Language: Subtypes *)

open! IStd
module F = Format

type t [@@deriving compare, equal, hash, normalize, yojson_of]

val pp : F.formatter -> t -> unit

val exact : t

val subtypes_cast : t
(** denotes the current type and any subtypes *)

val subtypes_instof : t

val is_known_subtype : Tenv.t -> Typ.Name.t -> Typ.Name.t -> bool
(** [is_known_subtype tenv c1 c2] returns true if there is enough information in [tenv] to prove
    that [c1] is a subtype of [c2]. Note that [not (is_known_subtype tenv c1 c2) == true] does not
    imply that [is_known_not_subtype tenv c1 c2 == true] *)

val is_instof : t -> bool [@@warning "-unused-value-declaration"]
