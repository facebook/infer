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

val get_or_add : proc:Procname.t -> t -> t * Errlog.t
(** Get the error log for a given procname. If there is none, add an empty one to the map. Return
    the resulting map together with the errlog. *)

val store : checker:Checker.t -> file:SourceFile.t -> t -> unit
(** If there are any issues in the log, [store ~checker ~file] stores the map in the database.
    Otherwise, no write occurs. *)

val iter_all_issues : f:(Checker.t -> Procname.t -> Errlog.t -> unit) -> unit
(** iterate over all stored issues from all registered checkers in arbitrary order *)
