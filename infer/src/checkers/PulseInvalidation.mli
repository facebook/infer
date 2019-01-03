(*
 * Copyright (c) 2018-present, Facebook, Inc.
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
  | CFree of HilExp.AccessExpression.t * Location.t
  | CppDelete of HilExp.AccessExpression.t * Location.t
  | CppDestructor of Typ.Procname.t * HilExp.AccessExpression.t * Location.t
  | Nullptr
  | StdVector of std_vector_function * HilExp.AccessExpression.t * Location.t
[@@deriving compare]

val issue_type_of_cause : t -> IssueType.t

val get_location : t -> Location.t option

val pp : Format.formatter -> t -> unit
