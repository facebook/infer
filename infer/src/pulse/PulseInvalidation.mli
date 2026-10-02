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
  | Erase
  | Insert
  | PushBack
  | Reserve
  | Resize
  | ShrinkToFit
[@@deriving compare, equal, yojson_of]

val pp_std_vector_function : F.formatter -> std_vector_function -> unit

type std_string_function =
  | Append
  | Assign
  | Clear
  | Erase
  | Insert
  | OperatorAssign
  | OperatorPlusAssign
  | PopBack
  | PushBack
  | Replace
  | Reserve
  | Resize
  | ShrinkToFit
[@@deriving compare, equal, yojson_of]

val all_std_string_functions : std_string_function list

val std_string_method_name : std_string_function -> string
(** name of the [std::basic_string] member function, e.g. ["push_back"] *)

val pp_std_string_function : F.formatter -> std_string_function -> unit

(** [std::deque] and the node-based containers of the C++ standard library *)
type std_container =
  | Deque
  | List
  | Map
  | Multimap
  | Multiset
  | Set
  | UnorderedMap
  | UnorderedMultimap
  | UnorderedMultiset
  | UnorderedSet
[@@deriving compare, equal, yojson_of]

type std_container_function =
  | Clear
  | Emplace
  | EmplaceBack
  | EmplaceFront
  | Erase
  | Insert
  | PushBack
  | PushFront
[@@deriving compare, equal, yojson_of]

val pp_std_container : F.formatter -> std_container -> unit

type map_type = FollyF14Value | FollyF14Vector | FollyF14Fast
[@@deriving compare, equal, yojson_of]

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
[@@deriving compare, equal, yojson_of]

val pp_map_type : F.formatter -> map_type -> unit

val pp_map_function : F.formatter -> map_function -> unit

type t =
  | CFree
  | ComparedToNullInThisProcedure of Location.t
  | ConstantDereference of IntLit.t
  | CppDelete
  | CppDeleteArray
  | EndIterator
  | FClose
  | GoneOutOfScope of Pvar.t * Typ.t
  | OptionalEmpty
  | StdVector of std_vector_function
  | StdString of std_string_function
  | CppMap of map_type * map_function
  | StdContainer of std_container * std_container_function
[@@deriving compare, equal, yojson_of]

val pp : F.formatter -> t -> unit

val describe : F.formatter -> t -> unit

val suggest : t -> string option

val is_same_type : t -> t -> bool
(** whether both invalidations have are of the same variant case *)

type must_be_valid_reason =
  | BlockCall
  | InsertionIntoCollectionKey
  | InsertionIntoCollectionValue
  | SelfOfNonPODReturnMethod of Typ.t
  | NullArgumentWhereNonNullExpected of PulseCallEvent.t * int option
[@@deriving compare, equal, yojson_of]

val pp_must_be_valid_reason : F.formatter -> must_be_valid_reason option -> unit

val issue_type_of_cause : latent:bool -> t -> must_be_valid_reason option -> IssueType.t
