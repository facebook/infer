(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Module for Type Environments. *)

type t

(** Type for type environment. *)

(** Add a (name,typename) pair to the global type environment. *)

val add : t -> Typ.Name.t -> Typ.Struct.t -> unit

(** Create a new type environment. *)

val create : unit -> t

(** Fold a function over the elements of the type environment. *)

val fold : (Typ.Name.t -> Typ.Struct.t -> 'a -> 'a) -> t -> 'a -> 'a

(** iterate over a type environment *)

val iter : (Typ.Name.t -> Typ.Struct.t -> unit) -> t -> unit

(** Load a type environment from a file *)

val load_from_file : DB.filename -> t option

(** Look up a name in the global type environment. *)

val lookup : t -> Typ.Name.t -> Typ.Struct.t option

(** Construct a struct_typ, normalizing field types *)

val mk_struct :
  t -> ?default:Typ.Struct.t -> ?fields:Typ.Struct.fields -> ?statics:Typ.Struct.fields
  -> ?methods:Typ.Procname.t list -> ?supers:Typ.Name.t list -> ?annots:Annot.Item.t -> Typ.Name.t
  -> Typ.Struct.t

(** Check if typename is found in t *)

val mem : t -> Typ.Name.t -> bool

(** print a type environment *)

val pp : Format.formatter -> t -> unit

(** Save a type environment into a file *)

val store_to_file : DB.filename -> t -> unit

(** Get method that is being overriden by java_pname (if any) **)

val get_overriden_method : t -> Typ.Procname.java -> Typ.Procname.t option
