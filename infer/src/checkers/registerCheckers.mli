(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(* val active_procedure_checkers : unit -> *)
(*   (Callbacks.proc_callback_t * bool * Config.language option) list *)

(* val active_cluster_checkers  : unit -> *)
(*   (Callbacks.cluster_callback_t * bool * Config.language option) list *)

val register : unit -> unit
