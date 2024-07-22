(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for storing issues detected outside of per-procedure analysis (and hence not serialized
    as a part of procedure summary). *)
type t

val empty : t

val iter : f:(Procname.t -> Errlog.t -> unit) -> t -> unit

val get_or_add : proc:Procname.t -> t -> t * Errlog.t
(** Get the error log for a given procname. If there is none, add an empty one to the map. Return
    the resulting map together with the errlog. *)

val is_stored : checker:Checker.t -> file:SourceFile.t -> bool
(** Returns true iff an issue log for the given checker and source file is stored in the database *)

val store : checker:Checker.t -> file:SourceFile.t -> t -> unit
(** If there are any issues in the log, [store ~checker ~file] stores the map in the database.
    Otherwise, no write occurs. *)

val invalidate : SourceFile.t -> unit
(** Delete any stored issues for the given [source_file] from the database. *)

val invalidate_all : procedures:Procname.t list -> unit
(** Delete any stored issues that depend on the given [procedures] from the database. *)

val iter_all_issues : f:(Checker.t -> Procname.t -> Errlog.t -> unit) -> unit
(** iterate over all stored issues from all registered checkers in arbitrary order *)
