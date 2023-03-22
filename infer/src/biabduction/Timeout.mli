(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Handle timeout events *)

val exe_timeout : ('a -> unit) -> 'a -> Exception.failure_kind option
(** Execute the function up to a given timeout. *)
