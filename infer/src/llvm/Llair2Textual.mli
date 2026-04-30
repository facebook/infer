(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Llair

type module_state

val init_module_state : Program.t -> Textual.Lang.t -> module_state

val translate : source_file:string -> module_state:module_state -> Textual.Module.t

(** Counters of frontend-coverage gaps, accumulated as side-effects of [translate]. Used by
    [LlvmFrontend.log_stats] to ship the totals to Scuba once per capture. *)
type frontend_stats = {unsupported_exps: int; unsupported_op2s: int}

val read_and_reset_frontend_stats : unit -> frontend_stats
(** Read the current totals and zero them. Call once at the end of a capture batch. *)
