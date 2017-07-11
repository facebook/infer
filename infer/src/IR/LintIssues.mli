(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Module to store a set of issues per procedure *)

val errLogMap : Errlog.t Typ.Procname.Map.t ref

val exists_issues : unit -> bool

val get_err_log : Typ.Procname.t -> Errlog.t
(** Save issues to a file *)

val store_issues : DB.filename -> Errlog.t Typ.Procname.Map.t -> unit
(** Load issues from the given file *)

val load_issues_to_errlog_map : string -> unit
(** Load all the lint issues in the given dir and update the issues map *)
