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
type t = base * access list

val compare : t -> t -> int

val equal : t -> t -> bool

(** create an access path from a pvar *)
val of_pvar : Pvar.t -> Typ.t -> t

(** append a new access to an existing access path; e.g., `append_access g x.f` produces `x.f.g` *)
val append : t -> access -> t

(** return true if [ap1] is a prefix of [ap2]. returns true for equal access paths *)
val is_prefix : t -> t -> bool

val pp : Format.formatter -> t -> unit
