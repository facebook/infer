(*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

type t =
  | Base of AccessPath.base
  | FieldOffset of t * Typ.Fieldname.t
  (* field access *)
  | ArrayOffset of t * Typ.t * t list
  (* array access *)
  | AddressOf of t
  (* address of operator & *)
  | Dereference of t
  (* dereference operator * *)
[@@deriving compare]

val to_access_path : t -> AccessPath.t

val to_access_paths : t list -> AccessPath.t list

val of_id : Ident.t -> Typ.t -> t
(** create an access expression from an ident *)

val get_base : t -> AccessPath.base

val get_typ : t -> Tenv.t -> Typ.t option

val pp : Format.formatter -> t -> unit

val equal : t -> t -> bool

val of_lhs_exp :
  include_array_indexes:bool -> add_deref:bool -> Exp.t -> Typ.t
  -> f_resolve_id:(Var.t -> t option) -> t option
(** convert [lhs_exp] to an access expression, resolving identifiers using [f_resolve_id] *)

val normalize : t -> t
