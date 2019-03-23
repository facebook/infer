(*
 * Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val init : unit -> unit
(** Parse properties, mentioned by [Config.topl_properties]. Does this only once. *)
