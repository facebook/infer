(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Summary of per-procedure nullsafe result analysis *)

type t =
  { issues: TypeErr.err_instance list
        (** List of issues found during analysis. Note that not all of them were necessarily
            reported to the user (some of them might be deemed non actionable.) *) }

val pp : Format.formatter -> t -> unit
