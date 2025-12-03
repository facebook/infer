(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type module_state

val init_module_state : Llair.Program.t -> Textual.Lang.t -> module_state

val language_of_source_file : string -> Textual.Lang.t

val capture : sources:string list -> In_channel.t -> unit

val capture_llair : string -> module_state -> unit
