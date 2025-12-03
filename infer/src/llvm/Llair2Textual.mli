(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Llair

type module_state

val init_module_state : Program.t -> Textual.Lang.t -> module_state

val translate : source_file:string -> module_state -> Textual.Module.t
