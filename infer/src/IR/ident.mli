(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Identifiers: program variables and logical variables *)

(** Program and logical variables. *)
type t

(** Names used to replace strings. *)
type name

(** Names for fields of class/struct/union *)
type fieldname

(** Kind of identifiers. *)
type kind

(** Set for identifiers. *)
module IdentSet : Set.S with type elt = t

(** Hash table with ident as key. *)
module IdentHash : Hashtbl.S with type key = t

(** Map with ident as key. *)
module IdentMap : Map.S with type key = t

(** Set for fieldnames *)
module FieldSet : Set.S with type elt = fieldname

(** Map for fieldnames *)
module FieldMap : Map.S with type key = fieldname

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
module NameHash : Hashtbl.S with type key = name

(** Name used for primed tmp variables *)
val name_primed : name

(** Name used for spec variables *)
val name_spec : name

(** Name used for the return variable *)
val name_return : Mangled.t

(** Convert a string to a name. *)
val string_to_name : string -> name

(** Create a field name at the given position *)
val create_fieldname : Mangled.t -> int -> fieldname

(** Convert a name to a string. *)
val name_to_string : name -> string

(** Convert a field name to a string. *)
val fieldname_to_string : fieldname -> string

(** Convert a fieldname to a simplified string with at most one-level path. *)
val fieldname_to_simplified_string : fieldname -> string

(** Convert a fieldname to a flat string without path. *)
val fieldname_to_flat_string : fieldname -> string

(** The class part of the fieldname *)
val java_fieldname_get_class : fieldname -> string

(** The last component of the fieldname *)
val java_fieldname_get_field : fieldname -> string

(** Check if the field is the synthetic this$n of a nested class, used to access the n-th outher instance. *)
val java_fieldname_is_outer_instance : fieldname -> bool

(** get the offset of a fieldname *)
val fieldname_offset : fieldname -> int

(** hidded fieldname constant *)
val fieldname_hidden : fieldname

(** hidded fieldname constant *)
val fieldname_is_hidden : fieldname -> bool

(** Name of the identifier. *)
val get_name : t -> name

(** Kind of the identifier. *)
val get_kind : t -> kind

(** Create an identifier with default name for the given kind *)
val create : kind -> int -> t

(** Generate a normal identifier with the given name and stamp. *)
val create_normal : name -> int -> t

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

(** Convert a primed ident into a nonprimed one, keeping the stamp. *)
val make_unprimed : t -> t

(** Get the stamp of the identifier *)
val get_stamp: t -> int

(** Set the stamp of the identifier *)
val set_stamp: t -> int -> t

(** {2 Comparision Functions} *)

(** Comparison for names. *)
val name_compare : name -> name -> int

(** Comparison for field names. *)
val fieldname_compare : fieldname -> fieldname -> int

(** Equality for names. *)
val name_equal : name -> name -> bool

(** Equality for field names. *)
val fieldname_equal : fieldname -> fieldname -> bool

(** Equality for kind. *)
val kind_equal : kind -> kind -> bool

(** Comparison for identifiers. *)
val compare : t -> t -> int

(** Equality for identifiers. *)
val equal : t -> t -> bool

(** Comparison for lists of identities *)
val ident_list_compare : t list -> t list -> int

(** Equality for lists of identities *)
val ident_list_equal : t list -> t list -> bool

(** {2 Pretty Printing} *)

(** Pretty print a name. *)
val pp_name : Format.formatter -> name -> unit

(** Pretty print a field name. *)
val pp_fieldname : Format.formatter -> fieldname -> unit

(** Pretty print a name in latex. *)
val pp_name_latex : Latex.style -> Format.formatter -> name -> unit

(** Pretty print a field name in latex. *)
val pp_fieldname_latex : Latex.style -> Format.formatter -> fieldname -> unit

(** Pretty print an identifier. *)
val pp : printenv -> Format.formatter -> t -> unit

(** Convert an identifier to a string. *)
val to_string : t -> string

(** Pretty print a list of identifiers. *)
val pp_list : printenv -> Format.formatter -> t list -> unit

(** Pretty print a list of names. *)
val pp_name_list : Format.formatter -> name list -> unit
