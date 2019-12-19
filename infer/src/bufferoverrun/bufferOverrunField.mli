(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

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

val cpp_vector_elem : vec_typ:Typ.t -> elt_typ:Typ.t -> Fieldname.t
(** Field for C++ vector's elements *)

val java_collection_internal_array : Fieldname.t
(** Field for Java collection's elements *)

val is_cpp_vector_elem : Fieldname.t -> bool
(** Check if the field is for C++ vector's elements *)

val is_java_collection_internal_array : Fieldname.t -> bool
(** Check if the field is for Java collection's elements *)
