(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Utils

(** Support for Execution environments *)

(** initial state, used to add cg's *)
type initial

(** execution environment: a global call graph, and map from procedure names to cfg and tenv *)
type t

(** freeze the execution environment, so it can be queried *)
val freeze : initial -> t

(** create a new execution environment, given an optional set restricting the active procedures *)
val create : Procname.Set.t option -> initial

(** Add a callee to the exe_env, and extend the file_map and proc_map. *)
val add_callee : t -> DB.source_file -> Procname.t -> unit

(** add call graph from the source dir in the spec db, with relative tenv and cfg, to the execution environment *)
val add_cg : initial -> DB.source_dir -> Cg.t option

(** like add_cg, but use exclude_fun to determine files to be excluded *)
val add_cg_exclude_fun : initial -> DB.source_dir -> (DB.source_file -> bool) -> Cg.t option

(** add a new source file -> file data mapping. arguments are the components of the file_data
  * record *)
val add_file_mapping : t -> DB.source_file -> int -> DB.filename -> Sil.tenv option -> DB.filename
  -> Cfg.cfg option -> unit

(** get the global call graph *)
val get_cg : t -> Cg.t

(** get the procedures defined in more than one file *)
val get_procs_defined_in_several_files : t -> Procname.Set.t

(** return the source file associated to the procedure *)
val get_source : t -> Procname.t -> DB.source_file

(** return the type environment associated to the procedure *)
val get_tenv : t -> Procname.t -> Sil.tenv

(** return the cfg associated to the procedure *)
val get_cfg : t -> Procname.t -> Cfg.cfg

(** [iter_files f exe_env] applies [f] to the source file and tenv and cfg for each file in [exe_env] *)
val iter_files : (DB.source_file -> Sil.tenv -> Cfg.cfg -> unit) -> t -> unit

(** [fold_files f exe_env] folds f through the source file, tenv, and cfg for each file in [exe_env] *)
val fold_files : (DB.source_file -> Sil.tenv -> Cfg.cfg -> 'a -> 'a) -> 'a -> t -> 'a

(** check if a procedure is marked as active *)
val proc_is_active : t -> Procname.t -> bool

(** add a procedure to the set of active procedures *)
val add_active_proc : t -> Procname.t -> unit
