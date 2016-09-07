(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Module for naming heap locations via the path used to access them (e.g., x.f.g, y[a].b) *)

type base = Var.t * Typ.t

type access =
  | FieldAccess of Ident.fieldname * Typ.t (* field name * field type *)
  | ArrayAccess of Typ.t (* array element type. index is unknown *)

(** root var, and a list of accesses. closest to the root var is first that is, x.f.g is represented
    as (x, [f; g]) *)
type raw = base * access list

type t =
  | Exact of raw (** precise representation of an access path, e.g. x.f.g *)
  | Abstracted of raw (** abstraction of heap reachable from an access path, e.g. x.f* *)

val base_compare : base -> base -> int

val base_equal : base -> base -> bool

val raw_compare : raw -> raw -> int

val raw_equal : raw -> raw -> bool

val access_compare : access -> access -> int

val access_equal : access -> access -> bool

(** create a base from a pvar *)
val base_of_pvar : Pvar.t -> Typ.t -> base

(** create a base from an ident *)
val base_of_id : Ident.t -> Typ.t -> base

(** create an access path from a pvar *)
val of_pvar : Pvar.t -> Typ.t -> raw

(** create an access path from an ident *)
val of_id : Ident.t -> Typ.t -> raw

(** convert [exp] to a raw access path, resolving identifiers using [f_resolve_id] *)
val of_exp : Exp.t -> Typ.t -> f_resolve_id:(Var.t -> raw option) -> raw option

(** append new accesses to an existing access path; e.g., `append_access x.f [g, h]` produces
    `x.f.g.h` *)
val append : raw -> access list -> raw

(** return true if [ap1] is a prefix of [ap2]. returns true for equal access paths *)
val is_prefix : raw -> raw -> bool

val pp_access : Format.formatter -> access -> unit

val pp_raw : Format.formatter -> raw -> unit

val compare : t -> t -> int

val equal : t -> t -> bool

(** extract a raw access path from its wrapper *)
val extract : t -> raw

(** return true if [t] is an exact representation of an access path, false if it's an abstraction *)
val is_exact : t -> bool

(** return true if \gamma(lhs) \subseteq \gamma(rhs) *)
val (<=) : lhs:t -> rhs:t -> bool

val pp_base : Format.formatter -> base -> unit

val pp : Format.formatter -> t -> unit
