(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
type proc_filter = Procname.t -> bool

type filters = {path_filter: path_filter; error_filter: error_filter; proc_filter: proc_filter}

val create_filters : unit -> filters
(** Create filters based on the config file *)

val never_return_null_matcher : SourceFile.t -> Procname.t -> bool

val capture_block_list_file_matcher : SourceFile.t -> bool

val modeled_expensive_matcher : (string -> bool) -> Procname.t -> bool
