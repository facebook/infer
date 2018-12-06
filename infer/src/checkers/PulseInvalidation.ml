(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

type std_vector_function = PushBack | Reserve [@@deriving compare]

let std_vector_function_pp f = function
  | PushBack ->
      F.fprintf f "std::vector::push_back"
  | Reserve ->
      F.fprintf f "std::vector::reserve"


type t =
  | CFree of AccessExpression.t * Location.t
  | CppDelete of AccessExpression.t * Location.t
  | CppDestructor of Typ.Procname.t * AccessExpression.t * Location.t
  | Nullptr
  | StdVector of std_vector_function * AccessExpression.t * Location.t
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
      F.fprintf f "invalidated by call to `free(%a)` at %a" AccessExpression.pp access_expr
        Location.pp location
  | CppDelete (access_expr, location) ->
      F.fprintf f "invalidated by call to `delete %a` at %a" AccessExpression.pp access_expr
        Location.pp location
  | CppDestructor (proc_name, access_expr, location) ->
      F.fprintf f "invalidated by destructor call `%a(%a)` at %a" Typ.Procname.pp proc_name
        AccessExpression.pp access_expr Location.pp location
  | Nullptr ->
      F.fprintf f "null pointer"
  | StdVector (std_vector_f, access_expr, location) ->
      F.fprintf f "potentially invalidated by call to `%a(%a, ..)` at %a" std_vector_function_pp
        std_vector_f AccessExpression.pp access_expr Location.pp location
