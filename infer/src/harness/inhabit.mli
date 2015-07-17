(*
* Copyright (c) 2013 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*)

open Utils

(** Generate a procedure that calls a given sequence of methods. Useful for harness/test generation. *)

type lifecycle_trace = (Procname.t * Sil.typ option) list

type callback_trace = (Sil.exp * Sil.typ) list

(** create a procedure named harness_name that calls each of the methods in trace in the specified
order with the specified receiver and add it to the execution environment *)
val inhabit_trace : lifecycle_trace -> callback_trace -> Procname.t ->

DB.source_file Procname.Map.t -> Sil.tenv -> unit

val source_dir_from_name : Procname.t -> DB.source_file Procname.Map.t -> DB.source_dir

val cfg_from_name : Procname.t -> DB.source_file Procname.Map.t -> Cfg.cfg

val cg_from_name : Procname.t -> DB.source_file Procname.Map.t -> Cg.t

val procdesc_from_name : Procname.t -> DB.source_file Procname.Map.t -> Cfg.Procdesc.t
