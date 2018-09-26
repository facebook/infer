(*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Filter type for a source file *)
type path_filter = SourceFile.t -> bool

(** Filter type for an error name. *)
type error_filter = IssueType.t -> bool

(** Filter type for a procedure name *)
type proc_filter = Typ.Procname.t -> bool

type filters = {path_filter: path_filter; error_filter: error_filter; proc_filter: proc_filter}

val create_filters : unit -> filters
(** Create filters based on the config file *)

val never_return_null_matcher : SourceFile.t -> Typ.Procname.t -> bool

val skip_translation_matcher : SourceFile.t -> Typ.Procname.t -> bool

val skip_implementation_matcher : SourceFile.t -> Typ.Procname.t -> bool

val modeled_expensive_matcher : (string -> bool) -> Typ.Procname.t -> bool

val test : unit -> unit
(** Load the config file and list the files to report on *)
