(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = Caml_threads.Mutex.t

val create : unit -> t

val critical_section : t -> f:(unit -> 'a) -> 'a
