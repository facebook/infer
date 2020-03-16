(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val capture : prog:string -> args:string list -> unit
(** do an ant capture with the given prog (i.e. ant) and args *)
