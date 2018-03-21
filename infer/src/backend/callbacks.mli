(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Module to register and invoke callbacks *)

type proc_callback_args =
  { get_proc_desc: Typ.Procname.t -> Procdesc.t option
  ; get_procs_in_file: Typ.Procname.t -> Typ.Procname.t list
  ; tenv: Tenv.t
  ; summary: Specs.summary
  ; proc_desc: Procdesc.t
  ; exe_env: Exe_env.t }

(** Type of a procedure callback:
    - List of all the procedures the callback will be called on.
    - get_proc_desc to get a proc desc from a proc name
    - Type environment.
    - Procedure for the callback to act on. *)
type proc_callback_t = proc_callback_args -> Specs.summary

type cluster_callback_args =
  {procedures: (Tenv.t * Procdesc.t) list; get_proc_desc: Typ.Procname.t -> Procdesc.t option}

type cluster_callback_t = cluster_callback_args -> unit

val register_procedure_callback : ?dynamic_dispath:bool -> Language.t -> proc_callback_t -> unit
(** register a procedure callback *)

val register_cluster_callback : Language.t -> cluster_callback_t -> unit
(** register a cluster callback *)

val iterate_callbacks : Exe_env.t -> unit
(** Invoke all the registered callbacks. *)
