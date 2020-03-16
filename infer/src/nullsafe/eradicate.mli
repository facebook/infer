(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** The main entry point for Nullsafe typechecker. *)

val proc_callback : Callbacks.proc_callback_t
(** Proc-level callback for nullsafe. *)

val file_callback : Callbacks.file_callback_t
(** File-level callback for nullsafe. Is called after all proc-level callbacks are called and
    calculated their summaries *)

val callback_check_return_type : TypeCheck.check_return_type -> Callbacks.proc_callback_t
(** For checkers that explore eradicate/nullsafe infra, but not part of nullsafe.Annot Call the
    given check_return_type at the end of every procedure. *)

(** Type for a module that provides a main callback function *)
module type CallBackT = sig
  val callback : TypeCheck.checks -> Callbacks.proc_callback_t
end

(** Extension to the type checker. *)
module type ExtensionT = sig
  val update_payloads : NullsafeSummary.t option -> Payloads.t -> Payloads.t
end
