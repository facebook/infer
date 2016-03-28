(*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Module for typestates: maps from expressions to annotated types, with extensions. *)

(** Parameters of a call. *)
type parameters = (Sil.exp * Sil.typ) list

type get_proc_desc = Procname.t -> Cfg.Procdesc.t option

(** Extension to a typestate with values of type 'a. *)
type 'a ext =
  {
    empty : 'a; (** empty extension *)
    check_instr : (** check the extension for an instruction *)
      Tenv.t -> get_proc_desc -> Procname.t ->
      Cfg.Procdesc.t ->'a -> Sil.instr -> parameters -> 'a;
    join : 'a -> 'a -> 'a; (** join two extensions *)
    pp : Format.formatter -> 'a -> unit (** pretty print an extension *)
  }

(** Typestate extended with elements of type 'a. *)
type 'a t

type range = Sil.typ * TypeAnnotation.t * (Location.t list)

val add_id : Ident.t -> range -> 'a t -> 'a t
val add : Pvar.t -> range -> 'a t -> 'a t
val empty : 'a ext -> 'a t
val equal : 'a t -> 'a t -> bool
val get_extension : 'a t -> 'a
val join : 'a ext -> 'a t -> 'a t -> 'a t
val lookup_id : Ident.t -> 'a t -> range option
val lookup_pvar : Pvar.t -> 'a t -> range option
val pp : 'a ext -> Format.formatter -> 'a t -> unit
val range_add_locs : range -> (Location.t list) -> range
val remove_id : Ident.t -> 'a t -> 'a t
val set_extension : 'a t -> 'a -> 'a t
