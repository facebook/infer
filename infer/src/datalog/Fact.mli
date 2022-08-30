(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t

val to_string : t -> string

val iter_fact_types : (string -> unit) -> unit

val is_generated_per_class : t -> Typ.Name.t option
(** If the fact is generated once for each class, return the corresponding class. *)

val entrypoint : Procname.t -> t

val extends : Typ.Name.t -> Typ.Name.t -> t

val cast : Procname.t -> Ident.t -> Ident.t -> Typ.t -> t

val alloc : Procname.t -> Ident.t -> Location.t -> Typ.t -> t

val virtual_call : Procname.t -> Location.t -> Ident.t -> Procname.t -> Ident.t -> t

val static_call : Procname.t -> Location.t -> Ident.t -> Procname.t -> t

val actual_arg : Procname.t -> Location.t -> Ident.t -> int -> Ident.t -> t

val formal_arg : Procname.t -> int -> Pvar.t -> t

val actual_return : Procname.t -> Location.t -> Ident.t -> t

val formal_return : Procname.t -> Ident.t -> t

val implem : Typ.Name.t -> Procname.t -> t

val load_field : Procname.t -> Ident.t -> Ident.t -> Fieldname.t -> t

val store_field : Procname.t -> Ident.t -> Fieldname.t -> Ident.t -> t

val move_load : Procname.t -> Ident.t -> Pvar.t -> t

val move_store : Procname.t -> Pvar.t -> Ident.t -> t
