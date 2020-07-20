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


type java_iterator_function = Remove [@@deriving compare]

let pp_java_iterator_function f = function Remove -> F.pp_print_string f "Iterator.remove"

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
[@@deriving compare]

let issue_type_of_cause = function
  | CFree ->
      IssueType.pil_use_after_free
  | ConstantDereference i when IntLit.iszero i ->
      IssueType.pil_nullptr_dereference
  | ConstantDereference _ ->
     IssueType.pil_constant_address_dereference
  | UninitializedDereference ->
     IssueType.uninitialized_dereference
  | CppDelete ->
      IssueType.pil_use_after_delete
  | EndIterator ->
      IssueType.pil_vector_invalidation
  | GoneOutOfScope _ ->
     IssueType.pil_use_after_lifetime
  | OptionalEmpty ->
      IssueType.pil_optional_empty_access
  | JavaIterator _ | StdVector _ ->
      IssueType.pil_vector_invalidation

let equal v1 v2=
  match v1, v2 with
    | CFree, CFree -> true
    | ConstantDereference i1, ConstantDereference i2 -> IntLit.eq i1 i2
    | CppDelete, CppDelete
      | CFree, CppDelete
      | EndIterator, EndIterator
      | GoneOutOfScope _, GoneOutOfScope _
      | StdVector _, StdVector _
      | JavaIterator _, JavaIterator _ -> true
    | _ -> false

let is_abduce_free v=
  match v with
    | CFree -> true
    | _ -> false

let is_actual_free v=
  match v with
    | CppDelete | CFree -> true
    | _ -> false

let describe f cause =
  match cause with
  | CFree ->
      F.pp_print_string f "was invalidated by call to `free()`"
  | ConstantDereference i when IntLit.iszero i ->
      F.pp_print_string f "is the null pointer"
  | ConstantDereference i ->
     F.fprintf f "is the constant %a" IntLit.pp i
  | UninitializedDereference ->
      F.pp_print_string f "is uninitialized"
  | CppDelete ->
      F.pp_print_string f "was invalidated by `delete`"
  | EndIterator ->
      F.pp_print_string f "is pointed to by the `end()` iterator"
  | GoneOutOfScope (pvar, typ) ->
      let pp_var f pvar =
        if Pvar.is_cpp_temporary pvar then
          F.fprintf f "is the address of a C++ temporary of type `%a`" (Typ.pp_full Pp.text) typ
        else F.fprintf f "is the address of a stack variable `%a`" Pvar.pp_value pvar
      in
      F.fprintf f "%a whose lifetime has ended" pp_var pvar
  | OptionalEmpty ->
      F.pp_print_string f "is folly::None"
  | StdVector std_vector_f ->
      F.fprintf f "was potentially invalidated by `%a()`" pp_std_vector_function std_vector_f
  | JavaIterator java_iterator_f ->
      F.fprintf f "was potentially invalidated by `%a()`" pp_java_iterator_function java_iterator_f


let pp f invalidation =
  match invalidation with
  | CFree ->
      F.fprintf f "CFree(%a)" describe invalidation
  | ConstantDereference _ ->
     F.fprintf f "ConstantDereference(%a)" describe invalidation
  | UninitializedDereference ->
     F.fprintf f "UninitializedDereference(%a)" describe invalidation
  | CppDelete ->
      F.fprintf f "CppDelete(%a)" describe invalidation
  | EndIterator | GoneOutOfScope _ | OptionalEmpty ->
      describe f invalidation
  | StdVector _ ->
      F.fprintf f "StdVector(%a)" describe invalidation
  | JavaIterator _ ->
      F.fprintf f "JavaIterator(%a)" describe invalidation
