(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Identifiers: program variables and logical variables *)

open! IStd

(** Program and logical variables. *)
type t [@@deriving compare]

val equal : t -> t -> bool
(** Equality for identifiers. *)

(** Names used to replace strings. *)
type name [@@deriving compare]

val equal_name : name -> name -> bool
(** Equality for names. *)

(** Kind of identifiers. *)
type kind [@@deriving compare]

val equal_kind : kind -> kind -> bool
(** Equality for kind. *)

(** Set for identifiers. *)
module Set : Caml.Set.S with type elt = t

(** Hash table with ident as key. *)
module Hash : Caml.Hashtbl.S with type key = t

(** Map with ident as key. *)
module Map : Caml.Map.S with type key = t

module HashQueue : Hash_queue.S with type Key.t = t

module NameGenerator : sig
  type t

  val get_current : unit -> t
  (** Get the current name generator. *)

  val reset : unit -> unit
  (** Reset the name generator. *)

  val set_current : t -> unit
  (** Set the current name generator. *)
end

val idlist_to_idset : t list -> Set.t
(** Convert an identifier list to an identifier set *)

val kprimed : kind

val knormal : kind

val kfootprint : kind

val name_spec : name
(** Name used for spec variables *)

val name_return : Mangled.t
(** Name used for the return variable *)

val string_to_name : string -> name
(** Convert a string to a name. *)

val name_to_string : name -> string
(** Convert a name to a string. *)

val get_name : t -> name
(** Name of the identifier. *)

val create : kind -> int -> t
(** Create an identifier with default name for the given kind *)

val create_normal : name -> int -> t
(** Generate a normal identifier with the given name and stamp. *)

val create_none : unit -> t
(** Create a "null" identifier for situations where the IR requires an id that will never be read *)

val create_footprint : name -> int -> t
(** Generate a footprint identifier with the given name and stamp. *)

val update_name_generator : t list -> unit
(** Update the name generator so that the given id's are not generated again *)

val create_fresh : kind -> t
(** Create a fresh identifier with default name for the given kind. *)

val create_fresh_specialized_with_blocks : kind -> t
(** Create a fresh identifier with default name for the given kind, with a non-clashing id for objc block specialization *)

val create_path : string -> t
(** Generate a normal identifier whose name encodes a path given as a string. *)

val is_primed : t -> bool
(** Check whether an identifier is primed or not. *)

val is_normal : t -> bool
(** Check whether an identifier is normal or not. *)

val is_footprint : t -> bool
(** Check whether an identifier is footprint or not. *)

val is_path : t -> bool
(** Check whether an identifier represents a path or not. *)

val is_none : t -> bool
(** Check whether an identifier is the special "none" identifier *)

val get_stamp : t -> int
(** Get the stamp of the identifier *)

val set_stamp : t -> int -> t
(** Set the stamp of the identifier *)

(** {2 Pretty Printing} *)

val pp_name : Format.formatter -> name -> unit
(** Pretty print a name. *)

val pp : Format.formatter -> t -> unit
(** Pretty print an identifier. *)

val to_string : t -> string
(** Convert an identifier to a string. *)

val hashqueue_of_sequence : ?init:unit HashQueue.t -> t Sequence.t -> unit HashQueue.t

val set_of_sequence : ?init:Set.t -> t Sequence.t -> Set.t
