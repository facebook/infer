(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Module for Type Environments. *)

type t (** Type for type environment. *)

(** Add a (name,typename) pair to the global type environment. *)
val add : t -> Typename.t -> Sil.struct_typ -> unit

(** Create a new type environment. *)
val create : unit -> t

(** Expand a type if it is a typename by looking it up in the type environment. *)
val expand_type : t -> Sil.typ -> Sil.typ

(** Fold a function over the elements of the type environment. *)
val fold : (Typename.t -> Sil.struct_typ -> 'a -> 'a) -> t -> 'a -> 'a

(** iterate over a type environment *)
val iter : (Typename.t -> Sil.struct_typ -> unit) -> t -> unit

(** Look up a name in the global type environment. *)
val lookup : t -> Typename.t -> Sil.struct_typ option

(** Load a type environment from a file *)
val load_from_file : DB.filename -> t option

(** Check if typename is found in t *)
val mem : t -> Typename.t -> bool

(** Save a type environment into a file *)
val store_to_file : DB.filename -> t -> unit

(** print a type environment *)
val pp : Format.formatter -> t -> unit
