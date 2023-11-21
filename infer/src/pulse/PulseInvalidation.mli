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

type map_type = FollyF14Value | FollyF14Vector | FollyF14Fast

type map_function =
  | Clear
  | Rehash
  | Reserve
  | OperatorEqual
  | Insert
  | InsertOrAssign
  | Emplace
  | TryEmplace
  | TryEmplaceToken
  | EmplaceHint
  | OperatorBracket

val pp_map_type : F.formatter -> map_type -> unit

val pp_map_function : F.formatter -> map_function -> unit

type t =
  | CFree
  | ConstantDereference of IntLit.t
  | CppDelete
  | CppDeleteArray
  | EndIterator
  | GoneOutOfScope of Pvar.t * Typ.t
  | OptionalEmpty
  | StdVector of std_vector_function
  | CppMap of map_type * map_function
[@@deriving compare, equal]

val pp : F.formatter -> t -> unit

val describe : F.formatter -> t -> unit

val suggest : t -> string option

type must_be_valid_reason =
  | BlockCall
  | InsertionIntoCollectionKey
  | InsertionIntoCollectionValue
  | SelfOfNonPODReturnMethod of Typ.t
  | NullArgumentWhereNonNullExpected of PulseCallEvent.t * int option
[@@deriving compare, equal]

val pp_must_be_valid_reason : F.formatter -> must_be_valid_reason option -> unit

val issue_type_of_cause : latent:bool -> t -> must_be_valid_reason option -> IssueType.t
