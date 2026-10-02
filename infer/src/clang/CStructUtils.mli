(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val unrolled_copy_length : Tenv.t -> ?dest:Exp.t -> Typ.t -> int option
(** the length of an array type that is copied element by element: an array with at most
    [Config.clang_compound_literal_init_limit] elements, counting the elements of nested arrays, no
    dimension longer than that, and whose element by element copy copies at most 16 values, counting
    the fields of the structs it contains. If the destination [dest] is an array member of a class,
    or an element of one, the array members of that class must copy at most 16 values together. *)

val unrolled_member_array_length : Tenv.t -> Exp.t -> Typ.t -> int option
(** [unrolled_copy_length tenv ~dest:exp typ] if [exp] is an array member of a class or an element
    of one, [None] otherwise *)

val struct_copy :
  Tenv.t -> Location.t -> Exp.t -> Exp.t -> typ:Typ.t -> struct_name:Typ.name -> Sil.instr list

val array_copy : Tenv.t -> Location.t -> Exp.t -> Exp.t -> typ:Typ.t -> Sil.instr list
(** [array_copy tenv loc e1 e2 ~typ] copies the array of type [typ] at [e2] to [e1], element by
    element if [unrolled_copy_length tenv typ] is defined *)
