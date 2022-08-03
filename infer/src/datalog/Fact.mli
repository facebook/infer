(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** In Alloc, the allocation site is uniquely identified by the string
    "class:line:assigned_variable" *)
type t =
  | Reachable of {proc_name: Procname.t}
  | Extends of {typ: Typ.Name.t; typ_super: Typ.Name.t}
  | Cast of {proc_name: Procname.t; dest: Ident.t; src: Ident.t; dest_typ: Typ.t}
  | Alloc of {proc_name: Procname.t; return: Ident.t; allocation_site: string; typ: Typ.t}
  | VirtualCall of {proc_name: Procname.t; call_site: string; return: Ident.t; call_proc: Procname.t}
  | StaticCall of {proc_name: Procname.t; call_site: string; call_proc: Procname.t}

val to_string : t -> string

val iter_fact_types : (string -> unit) -> unit

val reachable : Procname.t -> t

val extends : Typ.Name.t -> Typ.Name.t -> t

val cast : Procname.t -> Ident.t -> Ident.t -> Typ.t -> t

val alloc : Procname.t -> Ident.t -> Location.t -> Typ.t -> t

val virtual_call : Procname.t -> Location.t -> Ident.t -> Procname.t -> t

val static_call : Procname.t -> Location.t -> Ident.t -> Procname.t -> t
