(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for naming heap locations via the path used to access them (e.g., x.f.g, y[a].b) *)

type base = Var.t * Typ.t [@@deriving compare]

type access =
  | ArrayAccess of Typ.t * t list  (** array element type with list of access paths in index *)
  | FieldAccess of Fieldname.t  (** field name *)
[@@deriving compare, equal]

(** root var, and a list of accesses. closest to the root var is first that is, x.f.g is represented
    as (x, [f; g]) *)
and t = base * access list [@@deriving compare]

val get_typ : t -> Tenv.t -> Typ.t option
(** get the typ of the last access in the list of accesses if the list is non-empty, or the base if
    the list is empty. that is, for x.f.g, return typ(g), and for x, return typ(x) *)

val base_of_pvar : Pvar.t -> Typ.t -> base
(** create a base from a pvar *)

val of_pvar : Pvar.t -> Typ.t -> t
(** create an access path from a pvar *)

val of_id : Ident.t -> Typ.t -> t
(** create an access path from an ident *)

val of_var : Var.t -> Typ.t -> t
(** create an access path from a var *)

val append : t -> access list -> t
(** append new accesses to an existing access path; e.g., `append_access x.f [g, h]` produces
    `x.f.g.h` *)

(* used in infer/src/labs/ *)
val replace_prefix : prefix:t -> replace_with:t -> t -> t option [@@warning "-32"]

val equal_base : base -> base -> bool

val pp : Format.formatter -> t -> unit

val pp_base : Format.formatter -> base -> unit

val pp_access : Format.formatter -> access -> unit

(* used in infer/src/labs/ *)
val pp_access_list : Format.formatter -> access list -> unit [@@warning "-32"]

module Abs : sig
  type raw = t

  type t =
    | Abstracted of raw  (** abstraction of heap reachable from an access path, e.g. x.f* *)
    | Exact of raw  (** precise representation of an access path, e.g. x.f.g *)
  [@@deriving compare]

  val equal : t -> t -> bool

  val to_footprint : int -> t -> t
  (** replace the base var with a footprint variable rooted at formal index [formal_index] *)

  val get_footprint_index_base : base -> int option
  (** return the formal index associated with the base of this access path if there is one, or None
      otherwise *)

  val with_base : base -> t -> t
  (** swap base of existing access path for [base_var] (e.g., `with_base_bvar x y.f.g` produces
      `x.f.g` *)

  val extract : t -> raw
  (** extract a raw access path from its wrapper *)

  val is_exact : t -> bool
  (** return true if [t] is an exact representation of an access path, false if it's an abstraction *)

  val pp : Format.formatter -> t -> unit
end

module BaseMap : PrettyPrintable.PPMap with type key = base
