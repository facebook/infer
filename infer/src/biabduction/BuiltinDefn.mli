(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Models for the builtin functions supported *)
include BUILTINS.S with type t = Builtin.registered

val init : unit -> unit
(** Clients of Builtin module should call this before Builtin module is used. WARNING: builtins are
    not guaranteed to be registered with the Builtin module until after init has been called. *)
