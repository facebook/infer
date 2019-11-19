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
