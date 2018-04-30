(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Eradicate NPEs. *)

val callback_eradicate : Callbacks.proc_callback_t

val callback_check_return_type : TypeCheck.check_return_type -> Callbacks.proc_callback_t

(** Type for a module that provides a main callback function *)
module type CallBackT = sig
  val callback : TypeCheck.checks -> Callbacks.proc_callback_t
end
(* CallBackT *)

(** Extension to the type checker. *)
module type ExtensionT = sig
  type extension

  val ext : extension TypeState.ext

  val update_payload : extension TypeState.t option -> Specs.payload -> Specs.payload
end
