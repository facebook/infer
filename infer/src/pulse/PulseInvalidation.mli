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
[@@deriving compare]

val pp_std_vector_function : F.formatter -> std_vector_function -> unit

type t =
  | CFree
  | CppDelete
  | GoneOutOfScope of Pvar.t * Typ.t
  | Nullptr
  | StdVector of std_vector_function
[@@deriving compare]

val pp : F.formatter -> t -> unit

val issue_type_of_cause : t -> IssueType.t

val describe : F.formatter -> t -> unit
