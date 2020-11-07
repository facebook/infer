(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** A simple wrapper over provisional violations for Rules *)

type t =
  | Assignment of AssignmentRule.ProvisionalViolation.t
  | Dereference of DereferenceRule.ProvisionalViolation.t

val of_issue : TypeErr.err_instance -> t option
(** If the nullsafe issue is associated with a provisional violation, return it *)
