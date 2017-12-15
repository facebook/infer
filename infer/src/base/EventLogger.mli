(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

type event = UncaughtException of exn * int  (** exception, exitcode *)

val get_log_identifier : unit -> string

val prepare : unit -> unit

val log : event -> unit

val dump : unit -> unit
