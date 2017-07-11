(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Filter type for a source file *)
type path_filter = SourceFile.t -> bool

(** Filter type for an error name. *)
type error_filter = Localise.t -> bool

(** Filter type for a procedure name *)
type proc_filter = Typ.Procname.t -> bool

type filters = {path_filter: path_filter; error_filter: error_filter; proc_filter: proc_filter}

val do_not_filter : filters
(** Filters that accept everything. *)

val create_filters : Config.analyzer -> filters
(** Create filters based on the config file *)

val never_return_null_matcher : SourceFile.t -> Typ.Procname.t -> bool

val skip_translation_matcher : SourceFile.t -> Typ.Procname.t -> bool

val modeled_expensive_matcher : (string -> bool) -> Typ.Procname.t -> bool

val test : unit -> unit
(** Load the config file and list the files to report on *)

val is_checker_enabled : string -> bool
(** is_checker_enabled [error_name] is [true] if [error_name] is whitelisted in .inferconfig or if
    it's enabled by default *)
