(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Eradicate NPEs. *)

val callback_eradicate : Callbacks.proc_callback_t

val callback_check_return_type : TypeCheck.check_return_type -> Callbacks.proc_callback_t

(** Type for a module that provides a main callback function *)
module type CallBackT = sig
  val callback : TypeCheck.checks -> Callbacks.proc_callback_t
end

(** Extension to the type checker. *)
module type ExtensionT = sig
  val update_payloads : TypeState.t option -> Payloads.t -> Payloads.t
end
