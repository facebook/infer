(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Identifiers: program variables and logical variables *)

(** Program and logical variables. *)

type t [@@deriving compare]

(** Equality for identifiers. *)

val equal : t -> t -> bool

(** Names used to replace strings. *)

type name [@@deriving compare]

(** Equality for names. *)

val equal_name : name -> name -> bool

(** Kind of identifiers. *)

type kind [@@deriving compare]

(** Equality for kind. *)

val equal_kind : kind -> kind -> bool

(** Set for identifiers. *)

module IdentSet : Caml.Set.S with type elt = t

(** Hash table with ident as key. *)

module IdentHash : Caml.Hashtbl.S with type key = t

(** Map with ident as key. *)

module IdentMap : Caml.Map.S with type key = t

module NameGenerator : sig
  type t

  (** Get the current name generator. *)

  val get_current : unit -> t

  (** Reset the name generator. *)

  val reset : unit -> unit

  (** Set the current name generator. *)

  val set_current : t -> unit
end

(** Convert an identfier list to an identifier set *)

val idlist_to_idset : t list -> IdentSet.t

val kprimed : kind

val knormal : kind

val kfootprint : kind

(** hash table with names as keys *)

module NameHash : Caml.Hashtbl.S with type key = name

(** Name used for primed tmp variables *)

val name_primed : name

(** Name used for spec variables *)

val name_spec : name

(** Name used for the return variable *)

val name_return : Mangled.t

(** Convert a string to a name. *)

val string_to_name : string -> name

(** Convert a name to a string. *)

val name_to_string : name -> string

(** Name of the identifier. *)

val get_name : t -> name

(** Create an identifier with default name for the given kind *)

val create : kind -> int -> t

(** Generate a normal identifier with the given name and stamp. *)

val create_normal : name -> int -> t

(** Create a "null" identifier for situations where the IR requires an id that will never be read *)

val create_none : unit -> t

(** Generate a primed identifier with the given name and stamp. *)

val create_primed : name -> int -> t

(** Generate a footprint identifier with the given name and stamp. *)

val create_footprint : name -> int -> t

(** Update the name generator so that the given id's are not generated again *)

val update_name_generator : t list -> unit

(** Create a fresh identifier with default name for the given kind. *)

val create_fresh : kind -> t

(** Generate a normal identifier whose name encodes a path given as a string. *)

val create_path : string -> t

(** Check whether an identifier is primed or not. *)

val is_primed : t -> bool

(** Check whether an identifier is normal or not. *)

val is_normal : t -> bool

(** Check whether an identifier is footprint or not. *)

val is_footprint : t -> bool

(** Check whether an identifier represents a path or not. *)

val is_path : t -> bool

(** Check whether an identifier is the special "none" identifier *)

val is_none : t -> bool

(** Convert a primed ident into a nonprimed one, keeping the stamp. *)

val make_unprimed : t -> t

(** Get the stamp of the identifier *)

val get_stamp : t -> int

(** Set the stamp of the identifier *)

val set_stamp : t -> int -> t

(** {2 Pretty Printing} *)

(** Pretty print a name. *)

val pp_name : Format.formatter -> name -> unit

(** Pretty print a name in latex. *)

val pp_name_latex : Latex.style -> Format.formatter -> name -> unit

(** Pretty print an identifier. *)

val pp : Pp.env -> Format.formatter -> t -> unit

(** Convert an identifier to a string. *)

val to_string : t -> string

(** Pretty print a list of identifiers. *)

val pp_list : Pp.env -> Format.formatter -> t list -> unit

(** Pretty print a list of names. *)

val pp_name_list : Format.formatter -> name list -> unit
