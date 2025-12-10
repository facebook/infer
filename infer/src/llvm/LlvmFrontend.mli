(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val capture : sources:string list -> In_channel.t -> unit

val capture_llair : source_file:string -> llair_file:string -> unit
