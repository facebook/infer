(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type nullability = Nullable | Nonnull

let is_nullable nullability = match nullability with Nullable -> true | Nonnull -> false

let is_nonnull nullability = match nullability with Nullable -> false | Nonnull -> true

(*
  Subtype in the standard definition of subtyping `<:` operator:
  S <: T means that any term of type S can be safely used in a context
  where a term of type T is expected.
  The key fact is that Nonnull <: Nullable, but not the reverse.
  *)
let is_subtype ~subtype ~supertype =
  (*
    When it is NOT a subtype? When subtype is nullable and supertype is non-nullable.
    Everything else is allowed.
   *)
  not (is_nullable subtype && is_nonnull supertype)


let nullability_of_annotated_nullability annotated_nullability =
  match annotated_nullability with
  | AnnotatedNullability.Nullable _ ->
      Nullable
  | AnnotatedNullability.Nonnull _ ->
      Nonnull


let nullability_of_inferred_nullability inferred_nullability =
  (* This needs to be modified in order to support unknown nullability *)
  if InferredNullability.is_nullable inferred_nullability then Nullable else Nonnull


let passes_assignment_rule_for_annotated_nullability ~lhs ~rhs =
  let lhs_nullability = nullability_of_annotated_nullability lhs in
  let rhs_nullability = nullability_of_inferred_nullability rhs in
  is_subtype ~subtype:rhs_nullability ~supertype:lhs_nullability


let passes_assignment_rule_for_inferred_nullability ~lhs ~rhs =
  let lhs_nullability = nullability_of_inferred_nullability lhs in
  let rhs_nullability = nullability_of_inferred_nullability rhs in
  is_subtype ~subtype:rhs_nullability ~supertype:lhs_nullability


type type_role = Param | Ret

let passes_inheritance_rule type_role ~base ~overridden =
  let base_nullability = nullability_of_annotated_nullability base in
  let overridden_nullability = nullability_of_annotated_nullability overridden in
  let subtype, supertype =
    match type_role with
    | Ret ->
        (* covariance for ret *)
        (overridden_nullability, base_nullability)
    | Param ->
        (* contravariance for param *)
        (base_nullability, overridden_nullability)
  in
  is_subtype ~subtype ~supertype
