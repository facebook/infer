(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(** {2 Inferbo-specific constant field names} *)

val pp :
     pp_lhs:(Format.formatter -> 'a -> unit)
  -> sep:string
  -> Format.formatter
  -> 'a
  -> Fieldname.t
  -> unit
(** A parameterized pretty printer for field appended values *)

val get_type : Fieldname.t -> Typ.t option
(** Get type of field that is constructed in this module. This does not work in Java at the moment. *)

val c_strlen : unit -> Fieldname.t
(** Field for C string's length *)

val cpp_vector_elem : vec_typ:Typ.t -> Fieldname.t
(** Field for C++ vector's elements *)

val java_collection_internal_array : Fieldname.t
(** Field for Java collection's elements *)

val java_linked_list_index : Fieldname.t
(** Virtual field for index of Java's linked list *)

val java_linked_list_length : Fieldname.t
(** Virtual field for length of Java's linked list *)

val java_linked_list_next : Typ.t -> Fieldname.t
(** Virtual field for next of Java's linked list *)

val java_list_files_length : Fieldname.t
(** Virtual field for length of Java's files list in a directory *)

val cpp_collection_internal_array : Fieldname.t
(** Field for C++ collection's elements *)

val is_cpp_vector_elem : Fieldname.t -> bool
(** Check if the field is for C++ vector's elements *)

val is_java_collection_internal_array : Fieldname.t -> bool
(** Check if the field is for Java collection's elements *)

val objc_collection_internal_array : Fieldname.t
(** Field for ObjC's collection's elements *)

val objc_iterator_offset : Fieldname.t
(** Field for ObjC's nscollection's iterator offset *)

(** {2 Field domain constructor} *)

type 'prim t =
  | Prim of 'prim
  | Field of {prefix: 'prim t; fn: Fieldname.t; typ: Typ.t option}
  | StarField of {prefix: 'prim t; last_field: Fieldname.t}
      (** Represents a path starting with [prefix] and ending with the field [last_field], the
          middle can be anything. Invariants:

          - There is at most one StarField
          - StarField excluded, there are no duplicate fieldnames
          - StarField can only be followed by Deref elements *)
[@@deriving compare, equal]

val mk_append_field :
     prim_append_field:
       (   ?typ:Typ.t
        -> 'prim t
        -> Fieldname.t
        -> (depth:int -> 'prim t -> 'prim t)
        -> int
        -> 'prim
        -> 'prim t )
  -> prim_append_star_field:('prim t -> Fieldname.t -> ('prim t -> 'prim t) -> 'prim -> 'prim t)
  -> ?typ:Typ.t
  -> 'prim t
  -> Fieldname.t
  -> 'prim t

val mk_append_star_field :
     prim_append_star_field:('prim t -> Fieldname.t -> ('prim t -> 'prim t) -> 'prim -> 'prim t)
  -> 'prim t
  -> Fieldname.t
  -> 'prim t
