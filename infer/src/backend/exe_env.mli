(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Support for Execution environments *)

(** initial state, used to add cg's *)
type initial

(** execution environment: a global call graph, and map from procedure names to cfg and tenv *)
type t

val add_cg : initial -> DB.source_dir -> unit
(** add call graph from the source dir in the spec db,
    with relative tenv and cfg, to the execution environment *)

val from_cluster : Cluster.t -> t
(** Create an exe_env from a cluster *)

val get_cg : t -> Cg.t
(** get the global call graph *)

val get_source : t -> Typ.Procname.t -> SourceFile.t option
(** return the source file associated to the procedure *)

val get_tenv : t -> Typ.Procname.t -> Tenv.t
(** return the type environment associated to the procedure *)

val get_cfg : t -> Typ.Procname.t -> Cfg.cfg option
(** return the cfg associated to the procedure *)

val get_proc_desc : t -> Typ.Procname.t -> Procdesc.t option
(** return the proc desc associated to the procedure *)

val iter_files : (SourceFile.t -> Cfg.cfg -> unit) -> t -> unit
(** [iter_files f exe_env] applies [f] to the source file and tenv and cfg for each file in [exe_env] *)
