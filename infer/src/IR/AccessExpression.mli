(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = private
  | Base of AccessPath.base
  | FieldOffset of t * Typ.Fieldname.t  (** field access *)
  | ArrayOffset of t * Typ.t * t list  (** array access *)
  | AddressOf of t  (** "address of" operator [&] *)
  | Dereference of t  (** "dereference" operator [*] *)
[@@deriving compare]

val base : AccessPath.base -> t

val field_offset : t -> Typ.Fieldname.t -> t

val array_offset : t -> Typ.t -> t list -> t

val address_of : t -> t
  [@@warning "-32"]
(** guarantees that we never build [AddressOf (Dereference t)] expressions: these become [t] *)

val dereference : t -> t
(** guarantees that we never build [Dereference (AddressOf t)] expressions: these become [t] *)

module Access : sig
  type nonrec t =
    | FieldAccess of Typ.Fieldname.t
    | ArrayAccess of Typ.t * t list
    | TakeAddress
    | Dereference
  [@@deriving compare]

  val pp : Format.formatter -> t -> unit
end

val to_accesses : t -> AccessPath.base * Access.t list

val to_access_path : t -> AccessPath.t

val to_access_paths : t list -> AccessPath.t list

val of_id : Ident.t -> Typ.t -> t
(** create an access expression from an ident *)

val get_base : t -> AccessPath.base

val replace_base : remove_deref_after_base:bool -> AccessPath.base -> t -> t

val is_base : t -> bool

val get_typ : t -> Tenv.t -> Typ.t option

val pp : Format.formatter -> t -> unit

val equal : t -> t -> bool

val of_lhs_exp :
     include_array_indexes:bool
  -> add_deref:bool
  -> Exp.t
  -> Typ.t
  -> f_resolve_id:(Var.t -> t option)
  -> t option
(** convert [lhs_exp] to an access expression, resolving identifiers using [f_resolve_id] *)
