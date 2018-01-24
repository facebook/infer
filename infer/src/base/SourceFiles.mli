(*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

val get_all : unit -> SourceFile.t list
(** get all the source files in the database *)

val proc_names_of_source : SourceFile.t -> Typ.Procname.t list
(** list of all the proc names (declared and defined) found in a source file *)

val is_captured : SourceFile.t -> bool
(** has the source file been captured? *)
