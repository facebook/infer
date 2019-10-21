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
  | CFree
  | CppDelete
  | GoneOutOfScope of Pvar.t * Typ.t
  | Nullptr
  | StdVector of std_vector_function
[@@deriving compare]

let issue_type_of_cause = function
  | CFree ->
      IssueType.use_after_free
  | CppDelete ->
      IssueType.use_after_delete
  | GoneOutOfScope _ ->
      IssueType.use_after_lifetime
  | Nullptr ->
      IssueType.null_dereference
  | StdVector _ ->
      IssueType.vector_invalidation


let describe f cause =
  match cause with
  | CFree ->
      F.pp_print_string f "was invalidated by call to `free()`"
  | CppDelete ->
      F.pp_print_string f "was invalidated by `delete`"
  | GoneOutOfScope (pvar, typ) ->
      let pp_var f pvar =
        if Pvar.is_cpp_temporary pvar then
          F.fprintf f "is the address of a C++ temporary of type `%a`" (Typ.pp_full Pp.text) typ
        else F.fprintf f "is the address of a stack variable `%a`" Pvar.pp_value pvar
      in
      F.fprintf f "%a whose lifetime has ended" pp_var pvar
  | Nullptr ->
      F.pp_print_string f "is the null pointer"
  | StdVector std_vector_f ->
      F.fprintf f "was potentially invalidated by `%a()`" pp_std_vector_function std_vector_f


let pp f invalidation =
  match invalidation with
  | CFree ->
      F.fprintf f "CFree(%a)" describe invalidation
  | CppDelete ->
      F.fprintf f "CppDelete(%a)" describe invalidation
  | GoneOutOfScope _ ->
      describe f invalidation
  | Nullptr ->
      describe f invalidation
  | StdVector _ ->
      F.fprintf f "StdVector(%a)" describe invalidation
