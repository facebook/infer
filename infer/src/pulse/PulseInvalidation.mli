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
  | CustomFree of Procname.t
  | ConstantDereference of IntLit.t
  | CppDelete
  | CppDeleteArray
  | EndIterator
  | GoneOutOfScope of Pvar.t * Typ.t
  | OptionalEmpty
  | StdVector of std_vector_function
  | JavaIterator of java_iterator_function
[@@deriving compare, equal]

val isl_equiv : t -> t -> bool
(** check equality up to some ISL equivalences *)

val pp : F.formatter -> t -> unit

val describe : F.formatter -> t -> unit

type must_be_valid_reason =
  | BlockCall
  | InsertionIntoCollectionKey
  | InsertionIntoCollectionValue
  | SelfOfNonPODReturnMethod of Typ.t
  | NullArgumentWhereNonNullExpected of string
[@@deriving compare, equal]

val pp_must_be_valid_reason : F.formatter -> must_be_valid_reason option -> unit

val issue_type_of_cause : latent:bool -> t -> must_be_valid_reason option -> IssueType.t
