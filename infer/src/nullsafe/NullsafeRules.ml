(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let passes_assignment_rule ~lhs ~rhs = Nullability.is_subtype ~subtype:rhs ~supertype:lhs

let passes_dereference_rule = function
  | Nullability.Nullable ->
      false
  | Nullability.Nonnull ->
      true


type type_role = Param | Ret

let passes_inheritance_rule type_role ~base ~overridden =
  let subtype, supertype =
    match type_role with
    | Ret ->
        (* covariance for ret *)
        (overridden, base)
    | Param ->
        (* contravariance for param *)
        (base, overridden)
  in
  Nullability.is_subtype ~subtype ~supertype


let is_overannotated ~lhs ~rhs_upper_bound =
  Nullability.is_strict_subtype ~subtype:rhs_upper_bound ~supertype:lhs
