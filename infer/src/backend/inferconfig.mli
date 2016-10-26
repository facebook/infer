(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Filter type for a source file *)
type path_filter = DB.source_file -> bool

(** Filter type for an error name. *)
type error_filter = Localise.t -> bool

(** Filter type for a procedure name *)
type proc_filter = Procname.t -> bool

type filters =
  {
    path_filter : path_filter;
    error_filter : error_filter;
    proc_filter : proc_filter;
  }

(** Filters that accept everything. *)
val do_not_filter : filters

(** Create filters based on the config file *)
val create_filters : Config.analyzer -> filters

val never_return_null_matcher : DB.source_file -> Procname.t -> bool
val suppress_warnings_matcher : DB.source_file -> Procname.t -> bool
val skip_translation_matcher : DB.source_file -> Procname.t -> bool
val modeled_expensive_matcher : (string -> bool) -> Procname.t -> bool

(** Load the config file and list the files to report on *)
val test: unit -> unit

(** is_checker_enabled [error_name] is [true] if [error_name] is whitelisted in .inferconfig or if
    it's enabled by default *)
val is_checker_enabled : string -> bool
