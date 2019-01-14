(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for naming heap locations via the path used to access them (e.g., x.f.g, y[a].b) *)

type base = Var.t * Typ.t [@@deriving compare]

type access =
  | ArrayAccess of Typ.t * t list  (** array element type with list of access paths in index *)
  | FieldAccess of Typ.Fieldname.t  (** field name *)
[@@deriving compare]

(** root var, and a list of accesses. closest to the root var is first that is, x.f.g is
      representedas (x, [f; g]) *)
and t = base * access list [@@deriving compare]

val truncate : t -> t * access option
(** remove and return the last access of the access path if the access list is non-empty. returns 
    the original access path * None if the access list is empty *)

val get_last_access : t -> access option
(** get the last access in the list. returns None if the list is empty *)

val get_field_and_annotation : t -> Tenv.t -> (Typ.Fieldname.t * Annot.Item.t) option
(** get the field name and the annotation of the last access in the list of accesses if
      the list is non-empty and the last access is a field access *)

val get_typ : t -> Tenv.t -> Typ.t option
(** get the typ of the last access in the list of accesses if the list is non-empty, or the base
      if the list is empty. that is, for x.f.g, return typ(g), and for x, return typ(x) *)

val base_of_pvar : Pvar.t -> Typ.t -> base
(** create a base from a pvar *)

val of_pvar : Pvar.t -> Typ.t -> t
(** create an access path from a pvar *)

val of_id : Ident.t -> Typ.t -> t
(** create an access path from an ident *)

val of_exp :
  include_array_indexes:bool -> Exp.t -> Typ.t -> f_resolve_id:(Var.t -> t option) -> t list
(** extract the access paths that occur in [exp], resolving identifiers using [f_resolve_id]. don't include index expressions in array accesses if [include_array_indexes] is false *)

val of_lhs_exp :
  include_array_indexes:bool -> Exp.t -> Typ.t -> f_resolve_id:(Var.t -> t option) -> t option
(** convert [lhs_exp] to an access path, resolving identifiers using [f_resolve_id] *)

val append : t -> access list -> t
(** append new accesses to an existing access path; e.g., `append_access x.f [g, h]` produces
    `x.f.g.h` *)

val is_prefix : t -> t -> bool
(** return true if [ap1] is a prefix of [ap2]. returns true for equal access paths *)

val replace_prefix : prefix:t -> t -> t -> t option [@@warning "-32"]

val inner_class_normalize : t -> t
(** transform an access path that starts on "this" of an inner class but which breaks out to
   access outer class fields to the outermost one.
   Cases handled (recursively):
- (this:InnerClass* ).(this$n:OuterClassAccessor).f. ... -> (this:OuterClass* ).f . ...
- this$n.(this$m:OuterClassAccessor).f ... -> (this$m:OuterClass* ).f . ...
  (happens in ctrs only)
- this$n.f ... -> this.f . ...
  (happens in ctrs only)
*)

val equal : t -> t -> bool

val equal_base : base -> base -> bool

val pp : Format.formatter -> t -> unit

val pp_base : Format.formatter -> base -> unit

val pp_access : Format.formatter -> access -> unit

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

  val ( <= ) : lhs:t -> rhs:t -> bool
  (** return true if \gamma(lhs) \subseteq \gamma(rhs) *)

  val pp : Format.formatter -> t -> unit
end

module BaseMap : PrettyPrintable.PPMap with type key = base
