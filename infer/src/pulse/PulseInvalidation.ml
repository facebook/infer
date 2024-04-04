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
[@@deriving compare, equal]

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


type map_type = FollyF14Value | FollyF14Vector | FollyF14Fast [@@deriving compare, equal]

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
[@@deriving compare, equal]

let pp_map_type f = function
  | FollyF14Value ->
      F.fprintf f "folly::F14ValueMap"
  | FollyF14Vector ->
      F.fprintf f "folly::F14VectorMap"
  | FollyF14Fast ->
      F.fprintf f "folly::F14FastMap"


let pp_map_function f = function
  | Clear ->
      F.fprintf f "clear"
  | Rehash ->
      F.fprintf f "rehash"
  | Reserve ->
      F.fprintf f "reserve"
  | OperatorEqual ->
      F.fprintf f "operator="
  | Insert ->
      F.fprintf f "insert"
  | InsertOrAssign ->
      F.fprintf f "insert_or_assign"
  | Emplace ->
      F.fprintf f "emplace"
  | TryEmplace ->
      F.fprintf f "try_emplace"
  | TryEmplaceToken ->
      F.fprintf f "try_emplace_token"
  | EmplaceHint ->
      F.fprintf f "emplace_hint"
  | OperatorBracket ->
      F.fprintf f "operator[]"


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

type must_be_valid_reason =
  | BlockCall
  | InsertionIntoCollectionKey
  | InsertionIntoCollectionValue
  | SelfOfNonPODReturnMethod of Typ.t
  | NullArgumentWhereNonNullExpected of PulseCallEvent.t * int option
[@@deriving compare, equal]

let pp_must_be_valid_reason f = function
  | None ->
      F.fprintf f "None"
  | Some BlockCall ->
      F.fprintf f "Block"
  | Some InsertionIntoCollectionKey ->
      F.fprintf f "InsertionIntoCollectionKey"
  | Some InsertionIntoCollectionValue ->
      F.fprintf f "InsertionIntoCollectionValue"
  | Some (SelfOfNonPODReturnMethod _) ->
      F.fprintf f "SelfOfNonPODReturnMethod"
  | Some (NullArgumentWhereNonNullExpected _) ->
      F.fprintf f "NonNullExpected"


let issue_type_of_cause ~latent invalidation must_be_valid_reason =
  match invalidation with
  | CFree ->
      IssueType.use_after_free ~latent
  | ConstantDereference i when IntLit.iszero i -> (
    match must_be_valid_reason with
    | None ->
        IssueType.nullptr_dereference ~latent
    | Some BlockCall ->
        IssueType.nil_block_call ~latent
    | Some InsertionIntoCollectionKey | Some InsertionIntoCollectionValue ->
        IssueType.nil_insertion_into_collection ~latent
    | Some (SelfOfNonPODReturnMethod _) ->
        IssueType.nil_messaging_to_non_pod ~latent
    | Some (NullArgumentWhereNonNullExpected _) ->
        IssueType.null_argument ~latent )
  | ConstantDereference _ ->
      IssueType.constant_address_dereference ~latent
  | CppDelete | CppDeleteArray ->
      IssueType.use_after_delete ~latent
  | EndIterator ->
      IssueType.vector_invalidation ~latent
  | GoneOutOfScope _ ->
      IssueType.use_after_lifetime ~latent
  | OptionalEmpty ->
      IssueType.optional_empty_access ~latent
  | StdVector _ ->
      IssueType.vector_invalidation ~latent
  | CppMap _ ->
      IssueType.pulse_reference_stability


let describe f cause =
  match cause with
  | CFree ->
      F.pp_print_string f "was invalidated by call to `free()`"
  | ConstantDereference i when IntLit.iszero i ->
      F.pp_print_string f "is assigned to the null pointer"
  | ConstantDereference i ->
      F.fprintf f "is assigned to the constant %a" IntLit.pp i
  | CppDelete ->
      F.pp_print_string f "was invalidated by `delete`"
  | CppDeleteArray ->
      F.pp_print_string f "was invalidated by `delete[]`"
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
      F.pp_print_string f "is assigned an empty value"
  | StdVector std_vector_f ->
      F.fprintf f "was potentially invalidated by `%a`" pp_std_vector_function std_vector_f
  | CppMap (map_t, map_f) ->
      F.fprintf f "was potentially invalidated by `%a::%a`" pp_map_type map_t pp_map_function map_f


let suggest cause =
  match cause with
  | CppMap (_, OperatorBracket) ->
      Some
        "`operator[]` inserts when the key is not present, which can easily lead to unsafe code. \
         Use `.at` if the key is known to be in the map. If not, decomposing the expression, \
         performing another lookup, or using different map methods are common ways to fix and/or \
         improve the code."
  | _ ->
      None


let pp f invalidation =
  match invalidation with
  | CFree ->
      F.fprintf f "CFree(%a)" describe invalidation
  | ConstantDereference _ ->
      F.fprintf f "ConstantDereference(%a)" describe invalidation
  | CppDelete | CppDeleteArray ->
      F.fprintf f "CppDelete(%a)" describe invalidation
  | EndIterator | GoneOutOfScope _ | OptionalEmpty ->
      describe f invalidation
  | StdVector _ ->
      F.fprintf f "StdVector(%a)" describe invalidation
  | CppMap _ ->
      F.fprintf f "CppMap(%a)" describe invalidation
