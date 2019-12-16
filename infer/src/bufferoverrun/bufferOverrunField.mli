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
  -> Typ.Fieldname.t
  -> unit
(** A parameterized pretty printer for field appended values *)

val get_type : Typ.Fieldname.t -> Typ.t option
(** Get type of field that is constructed in this module. This does not work in Java at the moment. *)

val c_strlen : unit -> Typ.Fieldname.t
(** Field for C string's length *)

val cpp_vector_elem : vec_typ:Typ.t -> elt_typ:Typ.t -> Typ.Fieldname.t
(** Field for C++ vector's elements *)

val java_collection_internal_array : Typ.Fieldname.t
(** Field for Java collection's elements *)

val is_cpp_vector_elem : Typ.Fieldname.t -> bool
(** Check if the field is for C++ vector's elements *)

val is_java_collection_internal_array : Typ.Fieldname.t -> bool
(** Check if the field is for Java collection's elements *)
