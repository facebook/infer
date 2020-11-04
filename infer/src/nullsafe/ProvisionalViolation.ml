(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t =
  | Assignment of AssignmentRule.ProvisionalViolation.t
  | Dereference of DereferenceRule.ProvisionalViolation.t

let of_issue = function
  | TypeErr.Condition_redundant _
  | TypeErr.Field_not_initialized _
  | TypeErr.Inconsistent_subclass _
  | TypeErr.Over_annotation _ ->
      None
  | TypeErr.Nullable_dereference {dereference_violation} ->
      DereferenceRule.ProvisionalViolation.from dereference_violation
      |> Option.map ~f:(fun violation -> Dereference violation)
  | TypeErr.Bad_assignment {assignment_violation} ->
      AssignmentRule.ProvisionalViolation.from assignment_violation
      |> Option.map ~f:(fun violation -> Assignment violation)
