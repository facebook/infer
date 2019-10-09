(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let passes_assignment_rule ~lhs ~rhs = Nullability.is_subtype ~subtype:rhs ~supertype:lhs

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
