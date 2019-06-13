(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

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

val pp_std_vector_function : Format.formatter -> std_vector_function -> unit

type t =
  | CFree of HilExp.AccessExpression.t
  | CppDelete of HilExp.AccessExpression.t
  | GoneOutOfScope of Pvar.t * Typ.t
  | Nullptr
  | StdVector of std_vector_function * HilExp.AccessExpression.t
[@@deriving compare]

val issue_type_of_cause : t -> IssueType.t

val describe : Format.formatter -> t -> unit

val pp : Format.formatter -> t -> unit
