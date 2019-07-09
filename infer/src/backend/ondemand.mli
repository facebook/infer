(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for on-demand analysis. *)

val get_proc_desc : Typ.Procname.t -> Procdesc.t option
(** Find a proc desc for the procedure, perhaps loading it from disk. *)

val analyze_proc_desc : caller_summary:Summary.t -> Procdesc.t -> Summary.t option
(** [analyze_proc_desc ~caller_summary callee_pdesc] performs an on-demand analysis of callee_pdesc
   triggered during the analysis of caller_summary *)

val analyze_proc_name : ?caller_summary:Summary.t -> Typ.Procname.t -> Summary.t option
(** [analyze_proc_name ~caller_summary callee_pname] performs an on-demand analysis of callee_pname
   triggered during the analysis of caller_summary *)

val set_exe_env : Exe_env.t -> unit
(** Set the execution enviroment used during on-demand analysis. *)

val clear_cache : unit -> unit
(** empty the cache of ondemand results *)

val analyze_file : Exe_env.t -> SourceFile.t -> unit
(** Invoke all the callbacks registered in {!Callbacks} on the given file. *)

val analyze_proc_name_toplevel : Exe_env.t -> Typ.Procname.t -> unit
(** Invoke all the callbacks registered in {!Callbacks} on the given procedure. *)
