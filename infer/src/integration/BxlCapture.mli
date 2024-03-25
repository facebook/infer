(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val capture : string list -> unit
(** do a bxl/clang capture given the prog and build command (buck args) *)
