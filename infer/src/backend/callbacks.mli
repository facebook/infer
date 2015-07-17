(*
* Copyright (c) 2013 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*)

(** Module to register and invoke callbacks *)


(** Type of a procedure callback:
- List of all the procedures the callback will be called on.
- get_proc_desc to get a proc desc from a proc name.
- Idenv to look up the definition of ids in a cfg.
- Type environment.
- Procedure for the callback to act on. *)
type proc_callback_t =
  Procname.t list ->
  (Procname.t -> Cfg.Procdesc.t option) ->
  Idenv.t ->
  Sil.tenv ->
  Procname.t ->
  Cfg.Procdesc.t ->
  unit

type cluster_callback_t =
  Procname.t list ->
  (Procname.t -> Cfg.Procdesc.t option) ->
  (Idenv.t * Sil.tenv * Procname.t * Cfg.Procdesc.t) list ->
  unit

(** register a procedure callback *)
val register_procedure_callback : Sil.language option -> proc_callback_t -> unit

(** register a cluster callback *)
val register_cluster_callback : Sil.language option -> cluster_callback_t -> unit

(** un-register all the procedure callbacks currently registered *)
val unregister_all_callbacks : unit -> unit

(** Invoke all the registered callbacks. *)
val iterate_callbacks : (Procname.t -> unit) -> Cg.t -> Exe_env.t -> unit

(** Find synthetic (access or bridge) methods in the procedure and inline them in the cfg. *)
val proc_inline_synthetic_methods: Cfg.cfg -> Cfg.Procdesc.t -> unit
