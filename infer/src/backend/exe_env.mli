(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Support for Execution environments *)

type file_data

type t = private
  { proc_map: file_data Typ.Procname.Hash.t  (** map from procedure name to file data *)
  ; file_map: file_data SourceFile.Hash.t  (** map from source files to file data *)
  ; source_file: SourceFile.t  (** source file being analyzed *) }

val mk : SourceFile.t -> t
(** Create an exe_env from a source file *)

val get_tenv : t -> Typ.Procname.t -> Tenv.t
(** return the type environment associated to the procedure *)

val get_cfg : t -> Typ.Procname.t -> Cfg.t option
(** return the cfg associated to the procedure *)

val get_proc_desc : t -> Typ.Procname.t -> Procdesc.t option
(** return the proc desc associated to the procedure *)

val iter_files : (SourceFile.t -> Cfg.t -> unit) -> t -> unit
(** [iter_files f exe_env] applies [f] to the source file and tenv and cfg for each file in [exe_env] *)
