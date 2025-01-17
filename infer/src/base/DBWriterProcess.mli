(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val use_daemon : unit -> bool

val override_use_daemon : bool -> unit
(** override the default of whether the process should use DB daemon [true] or not [false] *)

val perform : DBWriterCommand.t -> unit
