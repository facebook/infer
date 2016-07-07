(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Module for naming heap locations via the path used to access them (e.g., x.f.g, y[a].b) *)

type base = Pvar.t * Typ.t

type access =
  | FieldAccess of Ident.fieldname * Typ.t (* field name * field type *)
  | ArrayAccess of Typ.t (* array element type. index is unknown *)

(** root var, and a list of accesses. closest to the root var is first that is, x.f.g is represented
    as (x, [f; g]) *)
type raw = base * access list

type t =
  | Exact of raw (** precise representation of an access path, e.g. x.f.g *)
  | Abstracted of raw (** abstraction of heap reachable from an access path, e.g. x.f* *)

val raw_compare : raw -> raw -> int

val raw_equal : raw -> raw -> bool

(** create an access path from a pvar *)
val of_pvar : Pvar.t -> Typ.t -> raw

(** append a new access to an existing access path; e.g., `append_access g x.f` produces `x.f.g` *)
val append : raw -> access -> raw

(** return true if [ap1] is a prefix of [ap2]. returns true for equal access paths *)
val is_prefix : raw -> raw -> bool

val pp_raw : Format.formatter -> raw -> unit

val compare : t -> t -> int

val equal : t -> t -> bool

(** extract a raw access path from its wrapper *)
val extract : t -> raw

(** return true if [t] is an exact representation of an access path, false if it's an abstraction *)
val is_exact : t -> bool

(** return true if \gamma(lhs) \subseteq \gamma(rhs) *)
val (<=) : lhs:t -> rhs:t -> bool

val pp : Format.formatter -> t -> unit
