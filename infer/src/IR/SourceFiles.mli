(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val add : SourceFile.t -> Cfg.t -> Tenv.per_file -> IntegerWidths.t option -> unit
(** Add or replace the row corresponding to the source file into the database. *)

val get_all : filter:Filtering.source_files_filter -> unit -> SourceFile.t list
(** get all the source files in the database *)

val proc_names_of_source : SourceFile.t -> Procname.t list
(** list of all the proc names (declared and defined) found in a source file *)

val get_procs_in_file : Procname.t -> Procname.t list
(** return the list of procedures in the file where the given procedure name was defined *)

val is_empty : unit -> bool
(** whether there exists at least one captured source file *)

val is_freshly_captured : SourceFile.t -> bool
(** whether the source file was captured in the last capture phase *)

val mark_all_stale : unit -> unit
(** mark all source files as stale; do be called at the start of a new capture phase *)

val pp_all :
     filter:Filtering.source_files_filter
  -> type_environment:bool
  -> procedure_names:bool
  -> freshly_captured:bool
  -> Format.formatter
  -> unit
  -> unit
