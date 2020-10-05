(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type std_vector_function =
  | Assign
  | Clear
  | Emplace
  | EmplaceBack
  | Insert
  | PushBack
  | Reserve
  | ShrinkToFit

val pp_std_vector_function : F.formatter -> std_vector_function -> unit

type java_iterator_function = Remove

type t =
  | CFree
  | ConstantDereference of IntLit.t
  | UninitializedDereference
  | CppDelete
  | EndIterator
  | GoneOutOfScope of Pvar.t * Typ.t
  | OptionalEmpty
  | StdVector of std_vector_function
  | JavaIterator of java_iterator_function
[@@deriving compare, equal]

val equal : t -> t -> bool

val is_abduce_free : t -> bool

val is_actual_free : t -> bool

val pp : F.formatter -> t -> unit

val issue_type_of_cause : t -> IssueType.t

val describe : F.formatter -> t -> unit
