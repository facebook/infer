(*
 * Copyright (c) 2018-present, Facebook, Inc.
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

let pp_std_vector_function f = function
  | Assign ->
      F.fprintf f "std::vector::assign"
  | Clear ->
      F.fprintf f "std::vector::clear"
  | Emplace ->
      F.fprintf f "std::vector::emplace"
  | EmplaceBack ->
      F.fprintf f "std::vector::emplace_back"
  | Insert ->
      F.fprintf f "std::vector::insert"
  | PushBack ->
      F.fprintf f "std::vector::push_back"
  | Reserve ->
      F.fprintf f "std::vector::reserve"
  | ShrinkToFit ->
      F.fprintf f "std::vector::shrink_to_fit"


type t =
  | CFree of HilExp.AccessExpression.t * Location.t
  | CppDelete of HilExp.AccessExpression.t * Location.t
  | CppDestructor of Typ.Procname.t * HilExp.AccessExpression.t * Location.t
  | Nullptr
  | StdVector of std_vector_function * HilExp.AccessExpression.t * Location.t
[@@deriving compare]

let issue_type_of_cause = function
  | CFree _ ->
      IssueType.use_after_free
  | CppDelete _ ->
      IssueType.use_after_delete
  | CppDestructor _ ->
      IssueType.use_after_destructor
  | Nullptr ->
      IssueType.null_dereference
  | StdVector _ ->
      IssueType.vector_invalidation


let get_location = function
  | CFree (_, location)
  | CppDelete (_, location)
  | CppDestructor (_, _, location)
  | StdVector (_, _, location) ->
      Some location
  | Nullptr ->
      None


let pp f = function
  | CFree (access_expr, location) ->
      F.fprintf f "invalidated by call to `free(%a)` at %a" HilExp.AccessExpression.pp access_expr
        Location.pp_line location
  | CppDelete (access_expr, location) ->
      F.fprintf f "invalidated by call to `delete %a` at %a" HilExp.AccessExpression.pp access_expr
        Location.pp_line location
  | CppDestructor (proc_name, access_expr, location) ->
      F.fprintf f "invalidated by destructor call `%a(%a)` at %a" Typ.Procname.pp proc_name
        HilExp.AccessExpression.pp access_expr Location.pp_line location
  | Nullptr ->
      F.fprintf f "null pointer"
  | StdVector (std_vector_f, access_expr, location) ->
      F.fprintf f "potentially invalidated by call to `%a(%a, ..)` at %a" pp_std_vector_function
        std_vector_f HilExp.AccessExpression.pp access_expr Location.pp_line location
