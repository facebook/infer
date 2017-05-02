(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Module for naming heap locations via the path used to access them (e.g., x.f.g, y[a].b) *)

type base = Var.t * Typ.t [@@deriving compare]

type access =
  | ArrayAccess of Typ.t (* array element type. index is unknown *)
  | FieldAccess of Fieldname.t (* field name *)
[@@deriving compare]

module Raw : sig
  (** root var, and a list of accesses. closest to the root var is first that is, x.f.g is
      representedas (x, [f; g]) *)
  type t = base * access list [@@deriving compare]

  (** remove the last access of the access path if the access list is non-empty. returns the
      original access path if the access list is empty *)
  val truncate : t -> t

  (** get the typ of the last access in the list of accesses if the list is non-empty, or the base
      if the list is empty. that is, for x.f.g, return typ(g), and for x, return typ(x) *)
  val get_typ : t -> Tenv.t -> Typ.t option

  val equal : t -> t -> bool

  val pp : Format.formatter -> t -> unit
end

type t =
  | Abstracted of Raw.t (** abstraction of heap reachable from an access path, e.g. x.f* *)
  | Exact of Raw.t (** precise representation of an access path, e.g. x.f.g *)
[@@deriving compare]

val equal_base : base -> base -> bool

val equal_access : access -> access -> bool

val equal_access_list : access list -> access list -> bool

val equal : t -> t -> bool

(** create a base from a pvar *)
val base_of_pvar : Pvar.t -> Typ.t -> base

(** create a base from an ident *)
val base_of_id : Ident.t -> Typ.t -> base

(** create an access path from a pvar *)
val of_pvar : Pvar.t -> Typ.t -> Raw.t

(** create an access path from an ident *)
val of_id : Ident.t -> Typ.t -> Raw.t

(** extract the raw access paths that occur in [exp], resolving identifiers using [f_resolve_id] *)
val of_exp : Exp.t -> Typ.t -> f_resolve_id:(Var.t -> Raw.t option) -> Raw.t list

(** convert [lhs_exp] to a raw access path, resolving identifiers using [f_resolve_id] *)
val of_lhs_exp : Exp.t -> Typ.t -> f_resolve_id:(Var.t -> Raw.t option) -> Raw.t option

(** append new accesses to an existing access path; e.g., `append_access x.f [g, h]` produces
    `x.f.g.h` *)
val append : Raw.t -> access list -> Raw.t

(** swap base of existing access path for [base_var] (e.g., `with_base_bvar x y.f.g` produces
    `x.f.g` *)
val with_base : base -> t -> t

(** return true if [ap1] is a prefix of [ap2]. returns true for equal access paths *)
val is_prefix : Raw.t -> Raw.t -> bool

val pp_access : Format.formatter -> access -> unit

val pp_access_list : Format.formatter -> access list -> unit

(** extract a raw access path from its wrapper *)
val extract : t -> Raw.t

(** return true if [t] is an exact representation of an access path, false if it's an abstraction *)
val is_exact : t -> bool

(** return true if \gamma(lhs) \subseteq \gamma(rhs) *)
val (<=) : lhs:t -> rhs:t -> bool

val pp_base : Format.formatter -> base -> unit

val pp : Format.formatter -> t -> unit

module BaseMap : PrettyPrintable.PPMap with type key = base

module AccessMap : PrettyPrintable.PPMap with type key = access

module RawSet : PrettyPrintable.PPSet with type elt = Raw.t

module RawMap : PrettyPrintable.PPMap with type key = Raw.t
