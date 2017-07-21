(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** The Smallfoot Intermediate Language: Subtypes *)

open! IStd
module L = Logging
module F = Format

type t [@@deriving compare]

val pp : F.formatter -> t -> unit

val sub_type : (Typ.Name.t -> Typ.Name.t) -> t -> t

val exact : t

val subtypes : t
(** denotes the current type only *)

val subtypes_cast : t
(** denotes the current type and any subtypes *)

val subtypes_instof : t

val join : t -> t -> t

val case_analysis : Tenv.t -> Typ.Name.t * t -> Typ.Name.t * t -> t option * t option
(** [case_analysis tenv (c1, st1) (c2, st2)] performs case analysis on [c1 <: c2] according
    to [st1] and [st2].
    [case_analysis] returns a pair:
    - whether [st1] and [st2] admit [c1 <: c2], and in case returns the updated subtype [st1]
    - whether [st1] and [st2] admit [not(c1 <: c2)], and in case returns the updated subtype [st1] *)

val is_known_subtype : Tenv.t -> Typ.Name.t -> Typ.Name.t -> bool
(** [is_known_subtype tenv c1 c2] returns true if there is enough information in [tenv] to prove
    that [c1] is a subtype of [c2].
    Note that [not (is_known_subtype tenv c1 c2) == true] does not imply
    that [is_known_not_subtype tenv c1 c2 == true] *)

val is_known_not_subtype : Tenv.t -> Typ.Name.t -> Typ.Name.t -> bool
(** [is_known_not_subtype tenv c1 c2] returns true if there is enough information in [tenv] to prove
    that [c1] is not a subtype of [c2].
    Note that [not (is_known_not_subtype tenv c1 c2) == true] does not imply
    that [is_known_subtype tenv c1 c2 == true] *)

val subtypes_to_string : t -> string

val is_cast : t -> bool

val is_instof : t -> bool

val equal_modulo_flag : t -> t -> bool
(** equality ignoring flags in the subtype *)
