(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val capture : command:string -> args:string list -> unit

val capture_buck : command:string -> args:string list -> unit
