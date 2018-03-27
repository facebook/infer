(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Models for the builtin functions supported *)
include BUILTINS.S with type t = Builtin.registered

val init : unit -> unit
(** Clients of Builtin module should call this before Builtin module is used.
    WARNING: builtins are not guaranteed to be registered with the Builtin module
    until after init has been called. *)
