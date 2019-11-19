(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Helper tools for nicer rendering nullsafe error. *)

open! IStd

val is_object_nullability_self_explanatory : object_expression:string -> TypeOrigin.t -> bool
(** In order to understand why such and such object is nullable (or not nullable),
    we render its origin.
    In some cases this is redundant and adds extra noise for the user.
  *)

val get_strict_mode_violation_issue :
  bad_usage_location:Location.t -> TypeOrigin.t -> string * IssueType.t * Location.t
(** Situation when we tried to use DeclaredNonnull as Nonnull.
    This is disallowed only in strict mode.
    Returns a tuple (error message, issue type, error location).
    NOTE: Location of the error will be NOT in the place when the value is used (that is [bad_usage_location]),
    but where the value is first obtained from.
 *)
