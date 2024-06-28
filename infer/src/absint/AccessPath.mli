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

(** root var, and a list of accesses. closest to the root var is first that is, x.f.g is represented
    as (x, [f; g]) *)
and t = base * access list [@@deriving compare, equal]

val base_of_pvar : Pvar.t -> Typ.t -> base
(** create a base from a pvar *)

val of_pvar : Pvar.t -> Typ.t -> t
(** create an access path from a pvar *)

(* used in infer/src/labs/ *)
val of_id : Ident.t -> Typ.t -> t
[@@warning "-unused-value-declaration"]
(** create an access path from an ident *)

val of_var : Var.t -> Typ.t -> t
(** create an access path from a var *)

(* used in infer/src/labs/ *)
val append : t -> access list -> t
[@@warning "-unused-value-declaration"]
(** append new accesses to an existing access path; e.g., `append_access x.f [g, h]` produces
    `x.f.g.h` *)

(* used in infer/src/labs/ *)
val replace_prefix : prefix:t -> replace_with:t -> t -> t option
[@@warning "-unused-value-declaration"]

val equal_base : base -> base -> bool

val pp : Format.formatter -> t -> unit

val pp_base : Format.formatter -> base -> unit

val pp_access : Format.formatter -> access -> unit

(* used in infer/src/labs/ *)
val pp_access_list : Format.formatter -> access list -> unit [@@warning "-unused-value-declaration"]

module Abs : sig
  type raw = t

  type t =
    | Abstracted of raw  (** abstraction of heap reachable from an access path, e.g. x.f* *)
    | Exact of raw  (** precise representation of an access path, e.g. x.f.g *)
  [@@deriving compare]

  val equal : t -> t -> bool

  val get_footprint_index_base : base -> int option
  (** return the formal index associated with the base of this access path if there is one, or None
      otherwise *)

  val extract : t -> raw
  (** extract a raw access path from its wrapper *)

  val is_exact : t -> bool
  (** return true if [t] is an exact representation of an access path, false if it's an abstraction *)

  val pp : Format.formatter -> t -> unit
end

module BaseMap : PrettyPrintable.PPMap with type key = base
